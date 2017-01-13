{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Working with @.lnk@ format should be a matter of serializing.
-- This library takes simpler approach, utilizing @Component Object Model@
-- (@COM@) library. Even though @COM@ provides some means of serialization,
-- they cannot be used in a pure fashion - the library needs
-- to be initialized and some @COM@ functions still query the
-- system for data. For this reason this library sticks to 'IO'.
--
-- Before calling 'writeShortcut' or 'readShortcut', @COM@ library
-- must be initialized with 'initialize'.
--
-- Library does not support shortcut's @IDList@s, so creating or
-- reading links to devices or network connections is not possible.
--
-- === Example
-- @
-- import Control.Monad.Except
--
-- main = print . runExceptT $ do
--   let link = empty { targetPath = "notepad.exe" }
--
--   ExceptT initialize
--   ExceptT $ writeShortcut link "c:\\\\link.lnk"
--   ret <- ExceptT $ readShortcut "c:\\\\link.lnk"
--   liftIO $ uninitialize
--
--   return ret
-- @
-- @
-- >>> main
-- Right (Shortcut {targetPath = "C:\\\\Windows\\\\system32\\\\notepad.exe",
-- arguments = "", workingDirectory = "", showCmd = ShowNormal,
-- description = "", iconLocation = ("",0), hotkey = 0})
-- @
module System.Win32.Shortcut (

  Shortcut (..),
  empty,
  ShowCmd (..),

  -- * Basic operations
  writeShortcut,
  unsafeWriteShortcut,
  readShortcut,

  -- * COM initialization
  initialize,
  uninitialize,

  -- * Errors
  ShortcutError (..),

  -- ** File IO Errors
  LoadError (..),
  SaveError (..),

  -- ** Argument errors
  PathError (..),
  ArgumentsError (..),
  WorkingDirectoryError (..),
  DescriptionError (..),
  IconLocationError (..),

  -- ** Other errors
  CoCreateInstanceError (..),
  CoInitializeError (..),
  HRESULTError (..),
  OrHRESULTError (..)
)
where

import Control.Monad (when, void)
import Control.Monad.Cont (ContT (..))
import Control.Monad.Except (ExceptT (..), withExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Foreign (allocaArray)
import Foreign.C (peekCWString)

import System.Win32.Shortcut.Error
import System.Win32.Shortcut.Internal


-- | Defines how a window will be opened when a link is executed.
data ShowCmd
  = ShowNormal
  -- ^ Start normally.
  | ShowMaximized
  -- ^ Start maximized.
  | ShowMinimized
  -- ^ Start minimized.
  deriving (Show)

fromShowCmd :: ShowCmd -> CInt
fromShowCmd = \case
  ShowNormal    -> sW_SHOWNORMAL
  ShowMaximized -> sW_SHOWMAXIMIZED
  ShowMinimized -> sW_SHOWMINNOACTIVE

toShowCmd :: CInt -> ShowCmd
toShowCmd x
  | x == sW_SHOWNORMAL      = ShowNormal
  | x == sW_SHOWMAXIMIZED   = ShowMaximized
  | x == sW_SHOWMINNOACTIVE = ShowMinimized
  | otherwise               = ShowNormal


-- | A shell link.
--
--   It seems that @.lnk@ format permits up to 32767
--   characters in text fields (259 for 'targetPath'), however
--   if 'workingDirectory', 'description' or 'iconLocation' is
--   longer then 259 characters @COM@ won't be able to read the
--   shortcut corectly ('readShortcut' will return faulty link without
--   raising any error, and @explorer.exe@ may not interpret it properly).
--   For this reason two write functions are provided. 'unsafeWriteShortcut'
--   will allow long fields and 'writeShortcut' which will raise
--   error if 'workingDirectory', 'description' or 'iconLocation'
--   is longer then 259 characters.
data Shortcut = Shortcut {
  targetPath :: FilePath,
  -- ^ Path to target.
  arguments :: String,
  -- ^ Arguments for target.
  workingDirectory :: FilePath,
  -- ^ Path to working directory.
  showCmd :: ShowCmd,
  description :: String,
  iconLocation :: (FilePath, Int),
  -- ^ Path to icon container (e.g. @.exe@, @.dll@, or @.ico@ file)
  --   and icon index.
  hotkey :: WORD
  -- ^ The virtual key code is in the low-order byte,
  --   and the modifier flags are in the high-order byte.
  --   @'hotkey' == 0@ means no hotkey will be used.
} deriving (Show)


type Callee struct vtbl fun = vtbl -> VtblPtrFun struct fun

newtype Caller struct vtbl = Call {
  call :: forall fun . Callee struct vtbl fun -> fun
}

makeMethodCaller
  :: (Storable vtbl, Storable struct)
  => (struct -> Ptr vtbl)
  -> Ptr (Ptr struct)
  -> IO (Caller struct vtbl)
makeMethodCaller getVtbl structPtrPtr = do
  structPtr  <- peek structPtrPtr
  structVtbl <- peek structPtr >>= peek . getVtbl
  return $ Call $ \getMtd -> getMtd structVtbl structPtr


type IShellLinkWCallee fun = Callee IShellLinkW IShellLinkWVtbl fun

type IShellLinkWCaller = Caller IShellLinkW IShellLinkWVtbl

ishQueryInterface' :: IShellLinkWCallee (REFIID -> Ptr (Ptr ()) -> IO HRESULT)
ishQueryInterface' = dynIshQueryInterface . ishQueryInterface

getPath' :: IShellLinkWCallee (LPWSTR -> CInt -> Ptr WIN32_FIND_DATAW -> DWORD -> IO HRESULT)
getPath' = dynGetPath . getPath

setPath' :: IShellLinkWCallee (LPCWSTR -> IO HRESULT)
setPath' = dynSetPath . setPath

getArguments' :: IShellLinkWCallee (LPWSTR -> CInt -> IO HRESULT)
getArguments' = dynGetArguments . getArguments

setArguments' :: IShellLinkWCallee (LPCWSTR -> IO HRESULT)
setArguments' = dynSetArguments . setArguments

getWorkingDirectory' :: IShellLinkWCallee (LPWSTR -> CInt -> IO HRESULT)
getWorkingDirectory' = dynGetWorkingDirectory . getWorkingDirectory

setWorkingDirectory' :: IShellLinkWCallee (LPCWSTR -> IO HRESULT)
setWorkingDirectory' = dynSetWorkingDirectory . setWorkingDirectory

getShowCmd' :: IShellLinkWCallee (Ptr CInt -> IO HRESULT)
getShowCmd' = dynGetShowCmd . getShowCmd

setShowCmd' :: IShellLinkWCallee (CInt -> IO HRESULT)
setShowCmd' = dynSetShowCmd . setShowCmd

getDescription' :: IShellLinkWCallee (LPWSTR -> CInt -> IO HRESULT)
getDescription' = dynGetDescription . getDescription

setDescription' :: IShellLinkWCallee (LPCWSTR -> IO HRESULT)
setDescription' = dynSetDescription . setDescription

getHotkey' :: IShellLinkWCallee (Ptr WORD -> IO HRESULT)
getHotkey' = dynGetHotkey . getHotkey

setHotkey' :: IShellLinkWCallee (WORD -> IO HRESULT)
setHotkey' = dynSetHotkey . setHotkey

getIconLocation' :: IShellLinkWCallee (LPWSTR -> CInt -> Ptr CInt -> IO HRESULT)
getIconLocation' = dynGetIconLocation . getIconLocation

setIconLocation' :: IShellLinkWCallee (LPCWSTR -> CInt -> IO HRESULT)
setIconLocation' = dynSetIconLocation . setIconLocation

ishRelease' :: IShellLinkWCallee (IO ULONG)
ishRelease' = dynIshRelease . ishRelease


type IPersistFileCallee fun = Callee IPersistFile IPersistFileVtbl fun

type IPersistFileCaller = Caller IPersistFile IPersistFileVtbl

save' :: IPersistFileCallee (LPCOLESTR -> WINBOOL -> IO HRESULT)
save' = dynSave . save

load' :: IPersistFileCallee (LPCOLESTR -> DWORD -> IO HRESULT)
load' = dynLoad . load

ipRelease' :: IPersistFileCallee (IO ULONG)
ipRelease' = dynIpRelease . ipRelease


withCaller
  :: (Storable struct, Storable vtbl)
  => (Ptr (Ptr ()) -> IO HRESULT)
  -> (struct -> Ptr vtbl)
  -> Callee struct vtbl (IO ULONG)
  -> ExceptT (OrHRESULTError CoCreateInstanceError) (ContT r IO) (Caller struct vtbl)
withCaller new getVtbl release = do
  structPtr <- lift . ContT . with $ nullPtr
  res <- liftIO $ new (castPtr structPtr)
  case succeeded' toCoCreateInstanceError res of
    Left err -> throwError err
    Right _ -> lift . ContT $ \k -> do
      caller <- makeMethodCaller getVtbl structPtr
      ret <- k caller
      void $ call caller release
      return ret

withIShellLinkCaller :: ExceptT (OrHRESULTError CoCreateInstanceError) (ContT r IO) IShellLinkWCaller
withIShellLinkCaller =
  withCaller
    (c_CoCreateInstance c_CLSID_ShellLink nullPtr cLSCTX_ALL c_IID_IShellLinkW)
    ishlpVtbl
    ishRelease'

withIPersistFileCaller
  :: IShellLinkWCaller
  -> ExceptT (OrHRESULTError CoCreateInstanceError) (ContT r IO) IPersistFileCaller
withIPersistFileCaller shellLinkCaller =
  withCaller
    (call shellLinkCaller ishQueryInterface' c_IID_IPersistFile)
    iplpVtbl
    ipRelease'


-- Max length of a CString fields, including terminator
longFieldLength, shortFieldLength :: CInt
longFieldLength = 32768
shortFieldLength = mAX_PATH + 1 -- does not apply to targetPath


-- | Create a shortcut under specified location. 'initialize' must be
--   called beforehand. 'targetPath' will be resolved with
--   respect to whatever is found in @PATH@ variable or desktop
--   if saved path is not absolute.
writeShortcut:: Shortcut -> FilePath -> IO (Either ShortcutError ())
writeShortcut = writeShortcutGeneric True

-- | Same as 'writeShortcut', but allows long 'description',
--   'workingDirectory' and 'iconLocation' fields. @COM@ and @explorer.exe@
--   may not interpret created link correctly.
unsafeWriteShortcut :: Shortcut -> FilePath -> IO (Either ShortcutError ())
unsafeWriteShortcut = writeShortcutGeneric False

writeShortcutGeneric :: Bool -> Shortcut -> FilePath -> IO (Either ShortcutError ())
writeShortcutGeneric safeRead shortcut path = flip runContT return . runExceptT $ do

  let throwIfTooLong f maxLength err = when (length (f shortcut) >= fromIntegral maxLength) (throwError err)
      throwIfTooLong' f = throwIfTooLong f (if safeRead then shortFieldLength else longFieldLength) in
    do throwIfTooLong targetPath mAX_PATH        (InvalidPath      $ OtherError PathTooLong)
       throwIfTooLong arguments  longFieldLength (InvalidArguments $ OtherError ArgumentsTooLong)

       throwIfTooLong' workingDirectory     (InvalidWorkingDirectory $ OtherError WorkingDirectoryTooLong)
       throwIfTooLong' description          (InvalidDescription      $ OtherError DescriptionTooLong)
       throwIfTooLong' (fst . iconLocation) (InvalidIconLocation     $ OtherError IconLocationTooLong)

  shellLinkCaller <- withExceptT CreateIShellLinkInterfaceError withIShellLinkCaller

  withExcept' (Left . InvalidPath . HRESULTError) $
    call shellLinkCaller setPath' <$> ContT (withCWString $ targetPath shortcut)

  withExcept' (Left . InvalidArguments . HRESULTError) $
    call shellLinkCaller setArguments' <$> ContT (withCWString $ arguments shortcut)

  withExcept' (Left . InvalidWorkingDirectory . HRESULTError) $
    call shellLinkCaller setWorkingDirectory' <$> ContT (withCWString $ workingDirectory shortcut)

  withExcept' (Left . InvalidShowCmd) $
    call shellLinkCaller setShowCmd' <$> pure (fromShowCmd $ showCmd shortcut)

  withExcept' (Left . InvalidDescription . HRESULTError) $
    call shellLinkCaller setDescription' <$> ContT (withCWString $ description shortcut)

  let (iconLocation', iconIndex) = iconLocation shortcut
  withExcept' (Left . InvalidIconLocation . HRESULTError) $
    call shellLinkCaller setIconLocation' <$> ContT (withCWString iconLocation') <*> pure (fromIntegral iconIndex)

  withExcept' (Left . InvalidHotkey) . pure $
    call shellLinkCaller setHotkey' (hotkey shortcut)

  iPersistFileCaller <- withExceptT CreateIPersistFileInterfaceError $
    withIPersistFileCaller shellLinkCaller

  withExcept' (overLeft SaveError . toSaveError) $
    call iPersistFileCaller save' <$> ContT (withCWString path) <*> pure tRUE


-- | Read a shortcut from the supplied location. 'initialize' must be
--   called beforehand.
readShortcut :: FilePath -> IO (Either ShortcutError Shortcut)
readShortcut path = flip runContT return . runExceptT $ do

  shellLinkCaller <- withExceptT CreateIShellLinkInterfaceError withIShellLinkCaller

  iPersistFileCaller <- withExceptT CreateIPersistFileInterfaceError $
    withIPersistFileCaller shellLinkCaller

  withExcept' (overLeft LoadError . toLoadError) $
    call iPersistFileCaller load' <$> ContT (withCWString path) <*> pure sTGM_READ

  pathPtr <- lift . ContT $ allocaArray (fromIntegral mAX_PATH)
  withExcept' (overLeft InvalidPath . toPathError) . pure $
    call shellLinkCaller getPath' pathPtr mAX_PATH nullPtr sLGP_RAWPATH

  argumentsPtr <- lift . ContT $ allocaArray (fromIntegral longFieldLength)
  withExcept' (Left . InvalidArguments . HRESULTError) . pure $
    call shellLinkCaller getArguments' argumentsPtr longFieldLength

  workingDirectoryPtr <- lift . ContT $ allocaArray (fromIntegral shortFieldLength)
  withExcept' (Left . InvalidWorkingDirectory . HRESULTError) . pure $
    call shellLinkCaller getWorkingDirectory' workingDirectoryPtr shortFieldLength

  showCmdPtr <- lift . ContT $ with 0
  withExcept' (Left . InvalidShowCmd) . pure $
    call shellLinkCaller getShowCmd' showCmdPtr

  descriptionPtr <- lift . ContT $ allocaArray (fromIntegral shortFieldLength)
  withExcept' (Left . InvalidDescription . HRESULTError) . pure $
    call shellLinkCaller getDescription' descriptionPtr shortFieldLength

  iconLocationPtr <- lift . ContT $ allocaArray (fromIntegral shortFieldLength)
  iconIndexPtr <- lift . ContT $ with 0
  withExcept' (Left . InvalidIconLocation . HRESULTError) . pure $
    call shellLinkCaller getIconLocation' iconLocationPtr shortFieldLength iconIndexPtr

  hotkeyPtr <- lift . ContT $ with 0
  withExcept' (Left . InvalidHotkey) . pure $
    call shellLinkCaller getHotkey' hotkeyPtr

  liftIO $
    Shortcut <$> peekCWString pathPtr
             <*> peekCWString argumentsPtr
             <*> peekCWString workingDirectoryPtr
             <*> (toShowCmd <$> peek showCmdPtr)
             <*> peekCWString descriptionPtr
             <*> ((,) <$> peekCWString iconLocationPtr
                      <*> (fromIntegral <$> peek iconIndexPtr))
             <*> peek hotkeyPtr


-- | Initialize @COM@ library for current thread.
--   Wraps <https://msdn.microsoft.com/en-us/library/windows/desktop/ms695279(v=vs.85).aspx CoInitializeEx>
--   function.
initialize :: IO (Either ShortcutError ())
initialize = succeeded' (overLeft InitializationError . toCoInitializeError)
  <$> c_CoInitializeEx nullPtr cOINIT_MULTITHREADED

-- | Uninitialize @COM@ library for current thread.
uninitialize :: IO ()
uninitialize = c_CoUninitialize


-- | An empty link. All fields are set to empty/default values.
empty :: Shortcut
empty = Shortcut {
  targetPath = "",
  arguments = "",
  workingDirectory = "",
  showCmd = ShowNormal,
  description = "",
  iconLocation = ("", 0),
  hotkey = 0
}
