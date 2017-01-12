{-# LANGUAGE LambdaCase #-}

module System.Win32.Shortcut.Error
where

import System.Win32.Shortcut.Internal

data HRESULTError
  = E_ABORT
  -- ^ Operation aborted
  | E_ACCESSDENIED
  -- ^ General access denied error
  | E_FAIL
  -- ^ Unspecified failure
  | E_HANDLE
  -- ^ Handle that is not valid
  | E_INVALIDARG
  -- ^ One or more arguments are not valid
  | E_NOINTERFACE
  -- ^ No such interface supported
  | E_NOTIMPL
  -- ^ Not implemented
  | E_OUTOFMEMORY
  -- ^ Failed to allocate necessary memory
  | E_POINTER
  -- ^ Pointer that is not valid
  | E_UNEXPECTED
  -- ^ Unexpected failure
  | HRESULTUnknown HRESULT
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
succeeded' f = either f (Right . id) . succeeded


data OrHRESULTError a
  = OtherError a
  | HRESULTError HRESULTError
  deriving (Show)


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
  deriving (Show)

toCoCreateInstanceError :: HRESULTError -> Either (OrHRESULTError CoCreateInstanceError) ()
toCoCreateInstanceError = Left . \case
  HRESULTUnknown x
    | x == rEGDB_E_CLASSNOTREG   -> OtherError CCI_REGDB_E_CLASSNOTREG
    | x == cLASS_E_NOAGGREGATION -> OtherError CCI_CLASS_E_NOAGGREGATION
  E_NOINTERFACE -> OtherError CCI_E_NOINTERFACE
  E_POINTER     -> OtherError CCI_E_POINTER
  other         -> HRESULTError other


data CoInitializeError
  = CI_RPC_E_CHANGED_MODE
  -- ^ A previous call to CoInitializeEx specified the concurrency model
  --   for this thread as multithread apartment (MTA).
  --   This could also indicate that a change from neutral-threaded
  --   apartment to single-threaded apartment has occurred.
  deriving (Show)

toCoInitializeError :: HRESULTError -> Either (OrHRESULTError CoInitializeError) ()
toCoInitializeError = Left . \case
  HRESULTUnknown x
    | x == rPC_E_CHANGED_MODE -> OtherError CI_RPC_E_CHANGED_MODE
  other -> HRESULTError other


data SaveError
  = SE_S_FALSE
  -- ^ The object was not successfully saved.
  deriving (Show)

toSaveError :: HRESULTError -> Either (OrHRESULTError SaveError) ()
toSaveError = \case
  HRESULTUnknown x
    | x == s_FALSE -> Left (OtherError SE_S_FALSE)
  other -> Left (HRESULTError other)


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
  -- ^ Path length >= MAX_PATH characters.
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
  -- ^ Working directory length >= 32768 characters.
  deriving (Show)


data DescriptionError
  = DescriptionTooLong
  -- ^ Description length >= 32768 characters.
  deriving (Show)


data IconLocationError
  = IconLocationTooLong
  -- ^ Icon location path length >= 32768 characters.
  deriving (Show)


data Error
  = CreateIShellLinkInterfaceError   (OrHRESULTError CoCreateInstanceError)
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
