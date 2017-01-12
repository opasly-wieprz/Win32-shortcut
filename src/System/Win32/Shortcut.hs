{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module System.Win32.Shortcut (
  Shortcut (..),
  ShowCmd (..),
  writeShortcut,
  unsafeWriteShortcut,
  readShortcut,
  initialize,
  uninitialize,
  empty,

  WORD,
  HRESULT,
  HRESULTError (..),
  OrHRESULTError (..),
  CoCreateInstanceError (..),
  CoInitializeError (..),
  SaveError (..),
  LoadError (..),
  PathError (..),
  ArgumentsError (..),
  WorkingDirectoryError (..),
  DescriptionError (..),
  IconLocationError (..),
  Error (..)
)
where

import Control.Monad (join, when, void)
import Control.Monad.Cont (ContT (..))
import Control.Monad.Except (ExceptT (..), withExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Foreign (allocaArray)
import Foreign.C (peekCWString)

import System.Win32.Shortcut.Error
import System.Win32.Shortcut.Internal

data ShowCmd
  = ShowNormal
  | ShowMaximized
  | ShowMinimized
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


data Shortcut = Shortcut {
  targetPath       :: FilePath,
  arguments        :: String,
  workingDirectory :: FilePath,
  showCmd          :: ShowCmd,
  description      :: String,
  iconLocation     :: (FilePath, Int),
  hotkey           :: WORD
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


withExcept'
  :: (HRESULTError -> Either e ())
  -> ContT r IO (IO HRESULT)
  -> ExceptT e (ContT r IO) ()
withExcept' f = ExceptT . fmap (succeeded' f) . join . fmap lift

overLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
overLeft f = either (Left . f) Right


-- including terminator
longFieldLength, shortFieldLength :: CInt
longFieldLength = 32768
shortFieldLength = mAX_PATH + 1


writeShortcut :: Shortcut -> FilePath -> IO (Either Error ())
writeShortcut = writeShortcutGeneral True

unsafeWriteShortcut :: Shortcut -> FilePath -> IO (Either Error ())
unsafeWriteShortcut = writeShortcutGeneral False

writeShortcutGeneral :: Bool -> Shortcut -> FilePath -> IO (Either Error ())
writeShortcutGeneral safeRead shortcut path = flip runContT return . runExceptT $ do

  let throwIfTooLong f maxLength err = when (length (f shortcut) >= fromIntegral maxLength) (throwError err)
      throwIfTooLong' f = throwIfTooLong f (if safeRead then shortFieldLength else longFieldLength) in
    do throwIfTooLong targetPath mAX_PATH        (InvalidPath $      OtherError PathTooLong)
       throwIfTooLong arguments  longFieldLength (InvalidArguments $ OtherError ArgumentsTooLong)

       throwIfTooLong' workingDirectory     (InvalidWorkingDirectory $ OtherError WorkingDirectoryTooLong)
       throwIfTooLong' description          (InvalidDescription $      OtherError DescriptionTooLong)
       throwIfTooLong' (fst . iconLocation) (InvalidIconLocation $     OtherError IconLocationTooLong)

  shellLinkCaller <- withExceptT CreateIShellLinkInterfaceError withIShellLinkCaller

  withExcept' (Left . InvalidPath . HRESULTError) $
    call shellLinkCaller setPath' <$> ContT (withCWString (targetPath shortcut))

  withExcept' (Left . InvalidArguments . HRESULTError) $
    call shellLinkCaller setArguments' <$> ContT (withCWString (arguments shortcut))

  withExcept' (Left . InvalidWorkingDirectory . HRESULTError) $
    call shellLinkCaller setWorkingDirectory' <$> ContT (withCWString (workingDirectory shortcut))

  withExcept' (Left . InvalidShowCmd) $
    call shellLinkCaller setShowCmd' <$> pure (fromShowCmd $ showCmd shortcut)

  withExcept' (Left . InvalidDescription . HRESULTError) $
    call shellLinkCaller setDescription' <$> ContT (withCWString (description shortcut))

  let (iconLocation', iconIndex) = iconLocation shortcut
  withExcept' (Left . InvalidIconLocation . HRESULTError) $
    call shellLinkCaller setIconLocation' <$> ContT (withCWString iconLocation') <*> pure (fromIntegral iconIndex)

  withExcept' (Left . InvalidHotkey) . pure $
    call shellLinkCaller setHotkey' (hotkey shortcut)

  iPersistFileCaller <- withExceptT CreateIPersistFileInterfaceError $
    withIPersistFileCaller shellLinkCaller

  withExcept' (overLeft SaveError . toSaveError) $
    call iPersistFileCaller save' <$> ContT (withCWString path) <*> pure tRUE


readShortcut :: FilePath -> IO (Either Error Shortcut)
readShortcut path = flip runContT return . runExceptT $ do

  shellLinkCaller <- withExceptT CreateIShellLinkInterfaceError withIShellLinkCaller

  iPersistFileCaller <- withExceptT CreateIPersistFileInterfaceError $
    withIPersistFileCaller shellLinkCaller

  withExcept' (overLeft LoadError . toLoadError) $
    call iPersistFileCaller load' <$> ContT (withCWString path) <*> pure sTGM_READ

  pathPtr <- lift . ContT $ allocaArray (fromIntegral $ mAX_PATH)
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

initialize :: IO (Either (OrHRESULTError CoInitializeError) ())
initialize = succeeded' toCoInitializeError <$> c_CoInitialize nullPtr

uninitialize :: IO ()
uninitialize = c_CoUninitialize

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
