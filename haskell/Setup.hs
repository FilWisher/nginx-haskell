{-# LANGUAGE RecordWildCards #-}

import Distribution.Simple
import Distribution.Simple.PackageIndex
import Distribution.Types.LocalBuildInfo
import Distribution.InstalledPackageInfo

import System.IO (writeFile)
import Data.List (intercalate, nub)
import Data.Monoid ((<>))

-- TODO: This may not link everything necessary. Double check it against Roman Cheplyaka's script:
--       https://ro-che.info/files/2017-07-26-haskell-library-in-c-project/opts.pl
-- TODO: need to ensure CFFI doesn't get linked in
main = defaultMainWithHooks $ simpleUserHooks
        { buildHook = \desc info hooks flags -> do
            let
                pkgs = allPackages (installedPkgs info)
                ldopts = mconcat $ map ldOptions pkgs
                libDirs = mconcat $ map (\pkg -> nub $ libraryDynDirs pkg ++ libraryDirs pkg) pkgs
                rpathDirs = map ("-rpath,"++) libDirs
                linkDirs = map ("-L"++) libDirs
                hsLibs = map (\lib -> "-l" ++ lib ++ "-ghc8.2.2") (mconcat $ map hsLibraries pkgs)

                nginxHaskellLib = "$ngx_addon_dir/haskell/libNginxHS.so"

            writeToConfig "../config" NginxAddonConfig
                { addonName = "ngx_http_haskell_module"
                , addonSources =
                    [ "$ngx_addon_dir/src/ngx_http_haskell_module.c"
                    ]
                , addonDeps =
                    [ "$ngx_addon_dir/src/ngx_http_haskell_module.h"
                    , nginxHaskellLib
                    ]
                , cflags =
                    [ "-fPIC"
                    , "-I$(ghc --print-libdir)/include"
                    ]
                , addonLdOpts =
                    ("-lNginxHS -Wl,-rpath,$ngx_addon_dir/haskell -L$ngx_addon_dir/haskell":ldopts)
                        ++ rpathDirs
                        ++ hsLibs
                        ++ linkDirs
                , addonLibs =
                    [ nginxHaskellLib
                    ]
                }
            buildHook simpleUserHooks desc info hooks flags
        }

-- | The config for defining the `config` options required for compiling the
-- Nginx addon. This needs to be generated here so we know the include and
-- library paths and linker flags.
data NginxAddonConfig = NginxAddonConfig
    { addonName    :: String
    , addonSources :: [String]
    , addonDeps    :: [String]
    , addonLdOpts  :: [String]
    , addonLibs    :: [String]
    , cflags       :: [String]
    }

writeToConfig :: FilePath -> NginxAddonConfig -> IO ()
writeToConfig path NginxAddonConfig{..} =
    writeFile path config
    where
        nl      = " \\\n    "
        sources = intercalate nl addonSources
        deps    = intercalate nl addonDeps
        ldopts  = intercalate nl addonLdOpts
        libs    = intercalate nl addonLibs
        flags   = unwords cflags
        config  = unlines
            [ "ngx_addon_name=" <> addonName
            , "HTTP_MODULES=\"$HTTP_MODULES " <> addonName <> "\""
            , ""
            , "NGX_ADDON_SRCS=\"$NGX_ADDON_SRCS " <> nl <> sources <> "\""
            , ""
            , "NGX_ADDON_DEPS=\"$NGX_ADDON_DEPS " <> nl <> deps <> "\""
            , ""
            , "NGX_LD_OPT=\"$NGX_LD_OPT " <> nl <> ldopts <> "\""
            , ""
            , "CORE_LIBS=\"$CORE_LIBS " <> nl <> libs <> "\""
            , ""
            , "CFLAGS=\"$CFLAGS " <> flags <> "\""
            ]
