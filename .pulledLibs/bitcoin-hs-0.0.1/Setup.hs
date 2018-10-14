
-- really fragile hack to build the assembly code into the library
-- unfortunately, cabal is not exactly helpful for this task, so to speak...

{-# LANGUAGE CPP #-}

import Control.Monad
import Data.Maybe

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.PackageDescription

import System.Process   -- System.Cmd
import System.Exit
import System.FilePath
import System.Directory

--------------------------------------------------------------------------------
-- * system-dependent configuration

data OS
  = Linux
  | Windows
  | MacOSX
  deriving (Eq,Show)

data CallConv
  = CDecl
  | Win64
  | SystemV
  deriving (Eq,Show)

host_bits :: Int
#if defined i386_HOST_ARCH
host_bits = 32
#elif defined x86_64_HOST_ARCH
host_bits = 64
#else
#error cannot figure out host architecture (for nasm)
#endif

host_os :: OS
#if defined mingw32_HOST_OS
host_os = Windows
#elif defined linux_HOST_OS
host_os = Linux
#elif defined darwin_HOST_OS
host_os = MacOSX
#else
#error cannot figure out target operating system (for nasm)
#endif

objfmt :: String
objfmt = case host_os of
  Windows -> "-fwin"   ++ show host_bits
  Linux   -> "-felf"   ++ show host_bits
  MacOSX  -> "-fmacho" ++ show host_bits

callConv :: CallConv
callConv = case host_bits of
  32 -> CDecl
  64 -> case host_os of
    Windows -> Win64
    _       -> SystemV

--------------------------------------------------------------------------------

mySubDir :: FilePath
mySubDir = "Bitcoin/Crypto/cbits/"

theFlags :: LocalBuildInfo -> FlagAssignment
theFlags lbi = configConfigurationsFlags $ configFlags lbi

doCompileWithAsm :: LocalBuildInfo -> Bool
doCompileWithAsm lbi =
  case lookup (mkFlagName "x86asm") (unFlagAssignment $ theFlags lbi) of
    Nothing -> False
    Just b  -> b

targetDir :: LocalBuildInfo -> FilePath
targetDir lbi = buildDir lbi </> mySubDir

--------------------------------------------------------------------------------

myBuildHook pkgdesc localbuildinfo userhooks buildflags = do

  when (doCompileWithAsm localbuildinfo) $ do
    putStrLn "now trying to run nasm to compile our sweet hand-written assembly code..."

    let nasmpath = "nasm"

    findExecutable nasmpath >>= \mb -> case mb of
      Nothing -> error "nasm executable not found in path - either install it or use the cabal flag -f-X86ASM"
      Just p  -> putStrLn $ "nasm executable is: " ++ p

    let srcdir = "." </> mySubDir
        tgtdir = targetDir localbuildinfo

    createDirectoryIfMissing True tgtdir  -- in a clean build, the dist/build directory doesn't exist yet...

    let dcallconv = case callConv of
          CDecl   -> "-DCALLCONV_CDECL"
          Win64   -> "-DCALLCONV_WIN64"
          SystemV -> "-DCALLCONV_SYSTEMV"

    let asmsrc = case host_bits of
          32 -> srcdir </> "asm_modp_x86.asm"
          64 -> srcdir </> "asm_modp_x64.asm"

    let objfile = case host_bits of
          32 -> tgtdir </> "asm_modp_x86.o"
          64 -> tgtdir </> "asm_modp_x64.o"

    let cmd = unwords [ nasmpath , objfmt , dcallconv , asmsrc , "-o" , objfile ]

    exitcode <- system cmd
    case exitcode of
      ExitSuccess   -> return ()
      ExitFailure k -> error ("nasm failed with exist code " ++ show k)

{-
    putStrLn "now let's create a library out of the object file..."

    let cmd = unwords ["ar", "rcs" , tgtdir </> "libmodp.a" , tgtdir </> "asm_modp.o" ]
    exitcode <- system cmd
    case exitcode of
      ExitSuccess -> return ()
      ExitFailure k -> error ("ar failed with exist code " ++ show k)
-}

  -- print localbuildinfo

  (buildHook simpleUserHooks) pkgdesc localbuildinfo userhooks buildflags

--------------------------------------------------------------------------------

myConfHook (genpkgdesc,hookbuildinfo) cfgflags = do
  lbi <- (confHook simpleUserHooks) (genpkgdesc,hookbuildinfo) cfgflags

  print $ theFlags lbi

  putStrLn $ "word size:  " ++ show host_bits ++ " bits"
  putStrLn $ "obj format: " ++ objfmt

  if (not $ doCompileWithAsm lbi)
    then return lbi
    else do

      putStrLn "now trying configure this mess to use our object file later on..."

      let lpd   = localPkgDescr lbi
          lib   = fromJust (library lpd)
          libbi = libBuildInfo lib

      let tgtdir = targetDir lbi
      createDirectoryIfMissing True tgtdir    -- in a clean build, the dist/build directory doesn't exist yet...

      fulltgtdir <- canonicalizePath tgtdir   -- this may fail if the directory does not exist???

      let objfile = case host_bits of
            32 -> fulltgtdir </> "asm_modp_x86.o"
            64 -> fulltgtdir </> "asm_modp_x64.o"

      -- now, seriously, cabal please...
      let progconf = withPrograms lbi
          progconf' = userSpecifyArgs "ar" [objfile]
                    $ userSpecifyArgs "ld" [objfile]
                    $ progconf
          libbi' = libbi

{-
      let libbi' = libbi { ldOptions = ldOptions libbi ++ [objfile] }
      let libbi' = libbi { options = options libbi ++ [(GHC,[objfile])] }
      let libbi' = libbi
            { extraLibs    = extraLibs    libbi ++ ["modp"]
            , extraLibDirs = extraLibDirs libbi ++ [fulltgtdir]
            }
-}

      -- print libbi'

      let lib' = lib { libBuildInfo = libbi' }
          lpd' = lpd { library = Just lib' }

      return $ lbi { localPkgDescr = lpd' , withPrograms = progconf' }

--------------------------------------------------------------------------------

main :: IO ()
main = do
  defaultMainWithHooks $ simpleUserHooks { confHook = myConfHook , buildHook = myBuildHook }
