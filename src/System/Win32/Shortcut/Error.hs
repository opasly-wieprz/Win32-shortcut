{-# LANGUAGE LambdaCase #-}

module System.Win32.Shortcut.Error
where

import Control.Monad (join)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Trans (MonadTrans, lift)

import System.Win32.Shortcut.Internal

-- | In @Win32 API@ 'HRESULT' is used to indicate error and warning
--   conditions. 'HRESULTError' wraps 'HRESULT' to give it some meaning.
data HRESULTError
  = E_ABORT
  -- ^ Operation aborted.
  | E_ACCESSDENIED
  -- ^ General access denied error.
  | E_FAIL
  -- ^ Unspecified failure.
  | E_HANDLE
  -- ^ Handle that is not valid.
  | E_INVALIDARG
  -- ^ One or more arguments are not valid.
  | E_NOINTERFACE
  -- ^ No such interface supported.
  | E_NOTIMPL
  -- ^ Not implemented.
  | E_OUTOFMEMORY
  -- ^ Failed to allocate necessary memory.
  | E_POINTER
  -- ^ Pointer that is not valid.
  | E_UNEXPECTED
  -- ^ Unexpected failure.
  | HRESULTUnknown HRESULT
  -- ^ Other, unknown 'HRESULT' value.
  deriving (Show)

succeeded :: HRESULT -> Either HRESULTError ()
succeeded x
  | x == s_OK           = Right ()
  | x == e_ACCESSDENIED = Left E_ACCESSDENIED
  | x == e_FAIL         = Left E_FAIL
  | x == e_HANDLE       = Left E_HANDLE
  | x == e_INVALIDARG   = Left E_INVALIDARG
  | x == e_NOINTERFACE  = Left E_NOINTERFACE
  | x == e_NOTIMPL      = Left E_NOTIMPL
  | x == e_OUTOFMEMORY  = Left E_OUTOFMEMORY
  | x == e_POINTER      = Left E_POINTER
  | x == e_UNEXPECTED   = Left E_UNEXPECTED
  | otherwise           = Left (HRESULTUnknown x)

succeeded' :: (HRESULTError -> Either e ()) -> HRESULT -> Either e ()
succeeded' f = either f Right . succeeded

withExcept'
  :: (MonadTrans t, Monad (t m), Monad m)
  => (HRESULTError -> Either e ())
  -> t m (m HRESULT)
  -> ExceptT e (t m) ()
withExcept' f = ExceptT . fmap (succeeded' f) . join . fmap lift

overLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
overLeft f = either (Left . f) Right


-- | Some error that comes from 'HRESULT'.
data OrHRESULTError err
  = OtherError err
  -- ^ 'HRESULT' value was expected and fits @err@.
  | HRESULTError HRESULTError
  -- ^ 'HRESULT' did not fit @err@.
  deriving (Show)

-- | @COM@ cannot create specified interface.
data CoCreateInstanceError
  = CCI_REGDB_E_CLASSNOTREG
  -- ^ A specified class is not registered in the registration database.
  --   Also can indicate that the type of server you requested
  --   in the CLSCTX enumeration is not registered or the values
  --   for the server types in the registry are corrupt.
  | CCI_CLASS_E_NOAGGREGATION
  -- ^ This class cannot be created as part of an aggregate.
  | CCI_E_NOINTERFACE
  -- ^ The specified class does not implement the requested interface,
  --   or the controlling IUnknown does not expose the requested interface.
  | CCI_E_POINTER
  -- ^ The ppv parameter is NULL.
  | CCI_CO_E_NOTINITIALIZED
  -- ^ @COM@ library was not initialized.
  deriving (Show)

toCoCreateInstanceError :: HRESULTError -> Either (OrHRESULTError CoCreateInstanceError) ()
toCoCreateInstanceError = Left . \case
  HRESULTUnknown x
    | x == -2147221164 -- 0x80040154
      -> OtherError CCI_REGDB_E_CLASSNOTREG
    | x == -2147221232 -- 0x80040110
      -> OtherError CCI_CLASS_E_NOAGGREGATION
    | x == -2147221008 -- 0x800401f0
      -> OtherError CCI_CO_E_NOTINITIALIZED
  E_NOINTERFACE -> OtherError CCI_E_NOINTERFACE
  E_POINTER     -> OtherError CCI_E_POINTER
  other         -> HRESULTError other


-- | @COM@ failed to initialize.
data CoInitializeError
  = CI_RPC_E_CHANGED_MODE
  -- ^ A previous call to CoInitializeEx specified the concurrency model
  --   for this thread as multithread apartment (MTA).
  --   This could also indicate that a change from neutral-threaded
  --   apartment to single-threaded apartment has occurred.
  | CI_CO_E_NOTINITIALIZED
  -- ^ You need to initialize the @COM@ library on a thread
  --   before you call any of the library functions.
  --   Otherwise, the @COM@ function will return 'CI_CO_E_NOTINITIALIZED'.
  deriving (Show)

toCoInitializeError :: HRESULTError -> Either (OrHRESULTError CoInitializeError) ()
toCoInitializeError = Left . \case
  HRESULTUnknown x
    | x == -2147417850 -- 0x80010106
      -> OtherError CI_RPC_E_CHANGED_MODE
    | x == -2147221008 -- 0x800401F0
      -> OtherError CI_CO_E_NOTINITIALIZED
  other -> HRESULTError other


-- | @IPersistFile@ interface failed to save a file.
data SaveError
  = SE_S_FALSE
  -- ^ The object was not successfully saved.
  deriving (Show)

toSaveError :: HRESULTError -> Either (OrHRESULTError SaveError) ()
toSaveError = \case
  HRESULTUnknown x
    | x == s_FALSE -> Left (OtherError SE_S_FALSE)
  other -> Left (HRESULTError other)


-- | @IPersistFile@ interface failed to load a file.
data LoadError
  = LE_E_OUTOFMEMORY
  -- ^ The object could not be loaded due to a lack of memory.
  | LE_E_FAIL
  -- ^ The object could not be loaded for some reason
  --   other than a lack of memory.
  deriving (Show)

toLoadError :: HRESULTError -> Either (OrHRESULTError LoadError) ()
toLoadError = Left . \case
  E_OUTOFMEMORY -> OtherError LE_E_OUTOFMEMORY
  E_FAIL -> OtherError LE_E_FAIL
  other -> HRESULTError other


data PathError
  = Path_S_FALSE
  -- ^ The operation is successful but no path is retrieved.
  | PathTooLong
  -- ^ Path length >= 260 characters.
  deriving (Show)

toPathError :: HRESULTError -> Either (OrHRESULTError PathError) ()
toPathError = \case
  HRESULTUnknown x
    | x == s_FALSE -> Left (OtherError Path_S_FALSE)
  other -> Left (HRESULTError other)


data ArgumentsError
  = ArgumentsTooLong
  -- ^ Arguments length >= 32768 characters.
  deriving (Show)


data WorkingDirectoryError
  = WorkingDirectoryTooLong
  deriving (Show)


data DescriptionError
  = DescriptionTooLong
  deriving (Show)


data IconLocationError
  = IconLocationTooLong
  deriving (Show)


-- | Catch-all type for errors.
data ShortcutError
  = InitializationError              (OrHRESULTError CoInitializeError)
  | CreateIShellLinkInterfaceError   (OrHRESULTError CoCreateInstanceError)
  | CreateIPersistFileInterfaceError (OrHRESULTError CoCreateInstanceError)
  | LoadError                        (OrHRESULTError LoadError)
  | SaveError                        (OrHRESULTError SaveError)
  | InvalidPath                      (OrHRESULTError PathError)
  | InvalidArguments                 (OrHRESULTError ArgumentsError)
  | InvalidWorkingDirectory          (OrHRESULTError WorkingDirectoryError)
  | InvalidDescription               (OrHRESULTError DescriptionError)
  | InvalidIconLocation              (OrHRESULTError IconLocationError)
  | InvalidHotkey  HRESULTError
  | InvalidShowCmd HRESULTError
  deriving (Show)
