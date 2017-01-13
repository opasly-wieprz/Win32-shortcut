{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Win32.Shortcut.Internal (
  module Foreign,
  module Foreign.C,
  module System.Win32,
  ULONG,
  OLECHAR,
  LPCOLESTR,
  LPOLESTR,
  WINBOOL,
  tRUE,
  fALSE,
  mAX_PATH,
  GUID (..),
  CLSID,
  IID,
  REFIID,
  REFCLSID,
  WIN32_FIND_DATAW (..),
  SHITEMID (..),
  ITEMIDLIST (..),
  LPITEMIDLIST,
  PIDLIST_ABSOLUTE,
  LPCITEMIDLIST,
  PCIDLIST_ABSOLUTE,
  HWND__ (..),
  HWND,
  VtblPtrFun,
  VtblMethod,
  MethodCast,
  IShellLinkWMethod,
  IShellLinkW (..),
  IShellLinkWVtbl (..),
  IShellLinkWMethodCast,
  dynIshQueryInterface,
  dynIshAddRef,
  dynIshRelease,
  dynGetPath,
  dynGetIDList,
  dynSetIDList,
  dynGetDescription,
  dynSetDescription,
  dynGetWorkingDirectory,
  dynSetWorkingDirectory,
  dynGetArguments,
  dynSetArguments,
  dynGetHotkey,
  dynSetHotkey,
  dynGetShowCmd,
  dynSetShowCmd,
  dynGetIconLocation,
  dynSetIconLocation,
  dynSetRelativePath,
  dynResolve,
  dynSetPath,
  IUnknownMethod,
  IUnknown (..),
  IUnknownVtbl (..),
  IUnknownMethodCast,
  dynIuQueryInterface,
  dynIuAddRef,
  dynIuRelease,
  LPUNKNOWN,
  IPersistFileMethod,
  IPersistFile (..),
  IPersistFileVtbl (..),
  IPersistFileMethodCast,
  dynIpQueryInterface,
  dynIpAddRef,
  dynIpRelease,
  dynGetClassID,
  dynIsDirty,
  dynLoad,
  dynSave,
  dynSaveCompleted,
  dynGetCurFile,
  cLSCTX_INPROC_HANDLER,
  cLSCTX_INPROC_SERVER,
  cLSCTX_LOCAL_SERVER,
  cLSCTX_REMOTE_SERVER,
  cLSCTX_ALL,
  sW_SHOWNORMAL,
  sW_SHOWMAXIMIZED,
  sW_SHOWMINNOACTIVE,
  s_OK,
  s_FALSE,
  e_ABORT,
  e_ACCESSDENIED,
  e_FAIL,
  e_HANDLE,
  e_INVALIDARG,
  e_NOINTERFACE,
  e_NOTIMPL,
  e_OUTOFMEMORY,
  e_POINTER,
  e_UNEXPECTED,
  SLGP_FLAGS,
  sLGP_SHORTPATH,
  sLGP_UNCPRIORITY,
  sLGP_RAWPATH,
  sLGP_RELATIVEPRIORITY,
  sTGM_READ,
  COINITBASE,
  cOINITBASE_MULTITHREADED,
  COINIT,
  cOINIT_APARTMENTTHREADED,
  cOINIT_MULTITHREADED,
  cOINIT_DISABLE_OLE1DDE,
  cOINIT_SPEED_OVER_MEMORY,
  c_CoInitializeEx,
  c_CoUninitialize,
  c_CoCreateInstance,
  c_CLSID_ShellLink,
  c_IID_IShellLinkW,
  c_IID_IPersistFile
)
where

import Foreign (
  Storable(..),
  Ptr,
  FunPtr,
  nullPtr,
  castPtr,
  with,
  (.|.))
import Foreign.C (
  CInt (..),
  CWchar,
  withCWString)
import System.Win32 (
  UCHAR,
  USHORT,
  BYTE,
  WORD,
  DWORD,
  LPVOID,
  LPCWSTR,
  LPWSTR,
  FILETIME,
  HRESULT)
import TH.Derive (
  Deriving,
  derive)

#include "windows_cconv.h"

type ULONG = DWORD

type OLECHAR = CWchar

type LPCOLESTR = Ptr OLECHAR

type LPOLESTR = Ptr OLECHAR

type WINBOOL = CInt

tRUE, fALSE :: WINBOOL
tRUE  = 1
fALSE = 0

mAX_PATH :: CInt
mAX_PATH = 260

data GUID = GUID {
  data1 :: !ULONG,
  data2 :: !USHORT,
  data3 :: !USHORT,
  data4 :: !(Ptr UCHAR) -- uchar[8]
} deriving (Show)

$($(derive [d| instance Deriving (Storable GUID) |]))

type CLSID = GUID

type IID = GUID

type REFIID = Ptr IID

type REFCLSID = Ptr IID


data WIN32_FIND_DATAW = WIN32_FIND_DATAW {
  dwFileAttributes   :: !DWORD,
  ftCreationTime     :: !FILETIME,
  ftLastAccessTime   :: !FILETIME,
  ftLastWriteTime    :: !FILETIME,
  nFileSizeHigh      :: !DWORD,
  nFileSizeLow       :: !DWORD,
  dwReserved0        :: !DWORD,
  dwReserved1        :: !DWORD,
  cFileName          :: !(Ptr CWchar), -- char[max_path]
  cAlternateFileName :: !(Ptr CWchar)  -- char[14]
} deriving (Show)

$($(derive [d| instance Deriving (Storable WIN32_FIND_DATAW) |]))


data SHITEMID = SHITEMID {
  cb   :: !USHORT,
  abID :: !(Ptr BYTE) -- BYTE[1]
} deriving (Show)

$($(derive [d| instance Deriving (Storable SHITEMID) |]))


newtype ITEMIDLIST = ITEMIDLIST {
  mkid :: SHITEMID
} deriving (Show)

$($(derive [d| instance Deriving (Storable ITEMIDLIST) |]))

type LPITEMIDLIST = Ptr ITEMIDLIST

type PIDLIST_ABSOLUTE = LPITEMIDLIST

type LPCITEMIDLIST = Ptr ITEMIDLIST

type PCIDLIST_ABSOLUTE = LPCITEMIDLIST


newtype HWND__ = HWND__ {
  unused :: CInt
} deriving (Show)

$($(derive [d| instance Deriving (Storable HWND__) |]))

type HWND = Ptr HWND__


type VtblPtrFun struct fun = Ptr struct -> fun

type VtblMethod struct fun = FunPtr (VtblPtrFun struct fun)

type MethodCast struct fun = VtblMethod struct fun -> VtblPtrFun struct fun


type IShellLinkWMethod fun = VtblMethod IShellLinkW fun

newtype IShellLinkW = IShellLinkW {
  ishlpVtbl :: Ptr IShellLinkWVtbl
} deriving (Show)

data IShellLinkWVtbl = IShellLinkWVtbl {
  ishQueryInterface   :: !(IShellLinkWMethod (REFIID -> Ptr (Ptr ()) -> IO HRESULT)),
  ishAddRef           :: !(IShellLinkWMethod (IO ULONG)),
  ishRelease          :: !(IShellLinkWMethod (IO ULONG)),
  getPath             :: !(IShellLinkWMethod (LPWSTR -> CInt -> Ptr WIN32_FIND_DATAW -> DWORD -> IO HRESULT)),
  getIDList           :: !(IShellLinkWMethod (Ptr PIDLIST_ABSOLUTE -> IO HRESULT)),
  setIDList           :: !(IShellLinkWMethod (PCIDLIST_ABSOLUTE -> IO HRESULT)),
  getDescription      :: !(IShellLinkWMethod (LPWSTR -> CInt -> IO HRESULT)),
  setDescription      :: !(IShellLinkWMethod (LPCWSTR -> IO HRESULT)),
  getWorkingDirectory :: !(IShellLinkWMethod (LPWSTR -> CInt -> IO HRESULT)),
  setWorkingDirectory :: !(IShellLinkWMethod (LPCWSTR -> IO HRESULT)),
  getArguments        :: !(IShellLinkWMethod (LPWSTR -> CInt -> IO HRESULT)),
  setArguments        :: !(IShellLinkWMethod (LPCWSTR -> IO HRESULT)),
  getHotkey           :: !(IShellLinkWMethod (Ptr WORD -> IO HRESULT)),
  setHotkey           :: !(IShellLinkWMethod (WORD -> IO HRESULT)),
  getShowCmd          :: !(IShellLinkWMethod (Ptr CInt -> IO HRESULT)),
  setShowCmd          :: !(IShellLinkWMethod (CInt -> IO HRESULT)),
  getIconLocation     :: !(IShellLinkWMethod (LPWSTR -> CInt -> Ptr CInt -> IO HRESULT)),
  setIconLocation     :: !(IShellLinkWMethod (LPCWSTR -> CInt -> IO HRESULT)),
  setRelativePath     :: !(IShellLinkWMethod (LPCWSTR -> DWORD -> IO HRESULT)),
  resolve             :: !(IShellLinkWMethod (HWND -> DWORD -> IO HRESULT)),
  setPath             :: !(IShellLinkWMethod (LPCWSTR -> IO HRESULT))
} deriving (Show)

$($(derive [d| instance Deriving (Storable IShellLinkW) |]))
$($(derive [d| instance Deriving (Storable IShellLinkWVtbl) |]))

type IShellLinkWMethodCast fun = MethodCast IShellLinkW fun

foreign import WINDOWS_CCONV "dynamic"
  dynIshQueryInterface   :: IShellLinkWMethodCast (REFIID -> Ptr (Ptr ()) -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynIshAddRef           :: IShellLinkWMethodCast (IO ULONG)

foreign import WINDOWS_CCONV "dynamic"
  dynIshRelease          :: IShellLinkWMethodCast (IO ULONG)

foreign import WINDOWS_CCONV "dynamic"
  dynGetPath             :: IShellLinkWMethodCast (LPWSTR -> CInt -> Ptr WIN32_FIND_DATAW -> DWORD -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynGetIDList           :: IShellLinkWMethodCast (Ptr PIDLIST_ABSOLUTE -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetIDList           :: IShellLinkWMethodCast (PCIDLIST_ABSOLUTE -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynGetDescription      :: IShellLinkWMethodCast (LPWSTR -> CInt -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetDescription      :: IShellLinkWMethodCast (LPCWSTR -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynGetWorkingDirectory :: IShellLinkWMethodCast (LPWSTR -> CInt -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetWorkingDirectory :: IShellLinkWMethodCast (LPCWSTR -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynGetArguments        :: IShellLinkWMethodCast (LPWSTR -> CInt -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetArguments        :: IShellLinkWMethodCast (LPCWSTR -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynGetHotkey           :: IShellLinkWMethodCast (Ptr WORD -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetHotkey           :: IShellLinkWMethodCast (WORD -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynGetShowCmd          :: IShellLinkWMethodCast (Ptr CInt -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetShowCmd          :: IShellLinkWMethodCast (CInt -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynGetIconLocation     :: IShellLinkWMethodCast (LPWSTR -> CInt -> Ptr CInt -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetIconLocation     :: IShellLinkWMethodCast (LPCWSTR -> CInt -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetRelativePath     :: IShellLinkWMethodCast (LPCWSTR -> DWORD -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynResolve             :: IShellLinkWMethodCast (HWND -> DWORD -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSetPath             :: IShellLinkWMethodCast (LPCWSTR -> IO HRESULT)


type IUnknownMethod fun = VtblMethod IUnknown fun

newtype IUnknown = IUnknown {
  iunklpVtbl :: Ptr IUnknownVtbl
} deriving (Show)

data IUnknownVtbl = IUnknownVtbl {
  iuQueryInterface :: !(IUnknownMethod (REFIID -> Ptr (Ptr ()) -> IO HRESULT)),
  iuAddRef         :: !(IUnknownMethod (IO ULONG)),
  iuRelease        :: !(IUnknownMethod (IO ULONG))
} deriving (Show)

$($(derive [d| instance Deriving (Storable IUnknown) |]))
$($(derive [d| instance Deriving (Storable IUnknownVtbl) |]))

type IUnknownMethodCast fun = MethodCast IUnknown fun

foreign import WINDOWS_CCONV "dynamic"
  dynIuQueryInterface :: IUnknownMethodCast (REFIID -> Ptr (Ptr ()) -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynIuAddRef         :: IUnknownMethodCast (IO ULONG)

foreign import WINDOWS_CCONV "dynamic"
  dynIuRelease        :: IUnknownMethodCast (IO ULONG)

type LPUNKNOWN = Ptr IUnknown


type IPersistFileMethod fun = VtblMethod IPersistFile fun

newtype IPersistFile = IPersistFile {
  iplpVtbl :: Ptr IPersistFileVtbl
} deriving (Show)

data IPersistFileVtbl = IPersistFileVtbl {
  ipQueryInterface :: !(IPersistFileMethod (REFIID -> Ptr (Ptr ()) -> IO HRESULT)),
  ipAddRef         :: !(IPersistFileMethod (IO ULONG)),
  ipRelease        :: !(IPersistFileMethod (IO ULONG)),
  getClassID       :: !(IPersistFileMethod (Ptr CLSID -> IO HRESULT)),
  isDirty          :: !(IPersistFileMethod (IO HRESULT)),
  load             :: !(IPersistFileMethod (LPCOLESTR -> DWORD -> IO HRESULT)),
  save             :: !(IPersistFileMethod (LPCOLESTR -> WINBOOL -> IO HRESULT)),
  saveCompleted    :: !(IPersistFileMethod (LPCOLESTR -> IO HRESULT)),
  getCurFile       :: !(IPersistFileMethod (Ptr LPOLESTR -> IO HRESULT))
} deriving (Show)

$($(derive [d| instance Deriving (Storable IPersistFile) |]))
$($(derive [d| instance Deriving (Storable IPersistFileVtbl) |]))

type IPersistFileMethodCast fun = MethodCast IPersistFile fun

foreign import WINDOWS_CCONV "dynamic"
  dynIpQueryInterface :: IPersistFileMethodCast (REFIID -> Ptr (Ptr ()) -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynIpAddRef         :: IPersistFileMethodCast (IO ULONG)

foreign import WINDOWS_CCONV "dynamic"
  dynIpRelease        :: IPersistFileMethodCast (IO ULONG)

foreign import WINDOWS_CCONV "dynamic"
  dynGetClassID       :: IPersistFileMethodCast (Ptr CLSID -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynIsDirty          :: IPersistFileMethodCast (IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynLoad             :: IPersistFileMethodCast (LPCOLESTR -> DWORD -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSave             :: IPersistFileMethodCast (LPCOLESTR -> WINBOOL -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynSaveCompleted    :: IPersistFileMethodCast (LPCOLESTR -> IO HRESULT)

foreign import WINDOWS_CCONV "dynamic"
  dynGetCurFile       :: IPersistFileMethodCast (Ptr LPOLESTR -> IO HRESULT)


cLSCTX_INPROC_SERVER, cLSCTX_INPROC_HANDLER           :: DWORD
cLSCTX_LOCAL_SERVER, cLSCTX_REMOTE_SERVER, cLSCTX_ALL :: DWORD
cLSCTX_INPROC_SERVER  = 0x1
cLSCTX_INPROC_HANDLER = 0x2
cLSCTX_LOCAL_SERVER   = 0x4
cLSCTX_REMOTE_SERVER  = 0x10
cLSCTX_ALL = cLSCTX_INPROC_SERVER
         .|. cLSCTX_INPROC_HANDLER
         .|. cLSCTX_LOCAL_SERVER
         .|. cLSCTX_REMOTE_SERVER


sW_SHOWNORMAL :: CInt
sW_SHOWNORMAL = 1

sW_SHOWMAXIMIZED :: CInt
sW_SHOWMAXIMIZED = 3

sW_SHOWMINNOACTIVE :: CInt
sW_SHOWMINNOACTIVE = 7


s_OK, s_FALSE, e_ABORT, e_ACCESSDENIED, e_FAIL, e_HANDLE, e_INVALIDARG :: HRESULT
e_NOINTERFACE, e_NOTIMPL, e_OUTOFMEMORY, e_POINTER, e_UNEXPECTED       :: HRESULT
s_OK                  =           0 -- 0x00000000 - negative hex literals
s_FALSE               =           1 -- 0x00000001   trigger a warning
e_ABORT               = -2147467260 -- 0x80004004
e_ACCESSDENIED        = -2147024891 -- 0x80070005
e_FAIL                = -2147467259 -- 0x80004005
e_HANDLE              = -2147024890 -- 0x80070006
e_INVALIDARG          = -2147024809 -- 0x80070057
e_NOINTERFACE         = -2147467262 -- 0x80004002
e_NOTIMPL             = -2147467263 -- 0x80004001
e_OUTOFMEMORY         = -2147024882 -- 0x8007000E
e_POINTER             = -2147467261 -- 0x80004003
e_UNEXPECTED          = -2147418113 -- 0x8000FFFF


type SLGP_FLAGS = DWORD

sLGP_SHORTPATH :: SLGP_FLAGS
sLGP_SHORTPATH = 0x1

sLGP_UNCPRIORITY :: SLGP_FLAGS
sLGP_UNCPRIORITY = 0x2

sLGP_RAWPATH :: SLGP_FLAGS
sLGP_RAWPATH = 0x4

sLGP_RELATIVEPRIORITY :: SLGP_FLAGS
sLGP_RELATIVEPRIORITY = 0x8


sTGM_READ :: DWORD
sTGM_READ = 0x00000000


type COINITBASE = DWORD

cOINITBASE_MULTITHREADED :: COINITBASE
cOINITBASE_MULTITHREADED = 0x0

type COINIT = DWORD

cOINIT_APARTMENTTHREADED :: COINIT
cOINIT_APARTMENTTHREADED = 0x2

cOINIT_MULTITHREADED :: COINIT
cOINIT_MULTITHREADED = cOINITBASE_MULTITHREADED

cOINIT_DISABLE_OLE1DDE :: COINIT
cOINIT_DISABLE_OLE1DDE = 0x4

cOINIT_SPEED_OVER_MEMORY :: COINIT
cOINIT_SPEED_OVER_MEMORY  = 0x8


foreign import WINDOWS_CCONV "objbase.h CoInitializeEx"
  c_CoInitializeEx :: LPVOID -> DWORD -> IO HRESULT

foreign import WINDOWS_CCONV "objbase.h CoUninitialize"
  c_CoUninitialize :: IO ()


foreign import WINDOWS_CCONV "combaseapi.h CoCreateInstance"
  c_CoCreateInstance :: REFCLSID -> LPUNKNOWN -> DWORD -> REFIID -> Ptr LPVOID -> IO HRESULT


foreign import WINDOWS_CCONV "shobjidl.h &CLSID_ShellLink"
  c_CLSID_ShellLink :: Ptr GUID

foreign import WINDOWS_CCONV "shobjidl.h &IID_IShellLinkW"
  c_IID_IShellLinkW :: Ptr GUID

foreign import WINDOWS_CCONV "shobjidl.h &IID_IPersistFile"
  c_IID_IPersistFile :: Ptr GUID
