

import Azubi

main :: IO ()
main = azubiMain azubiSetup



-- | example
myConfig :: AzubiConfig
myConfig = azubiConfig Gentoo $ []
      & installed "vim"
      ! installed "salt"
      & (submodule $ []
         ! exists (Directory "/dev/shm/azubi-test")
         & contains (File "/dev/shm/azubi/contains") [ "this is a azubi test-line : " ++ (show i) | i <- [1,2..10]]
         & exists (Symlink "/dev/shm/azubi-link" "/dev/shm/azubi/contains")
        )
      ! (submodule $ []
         & installed "sub-emacs"
         & installed "sub-vim"
        )
      ! exists (Directory "/dev/shm/Downloads")
      ! exists (Directory "/dev/shm/Documents")
      & ((submodule $ []
          & contains (File "/etc/vim/config") ["# config"]
         ) `requires` (submodule $ []
                       & installed "vim"
                      ))
      & installed "vim"
      ! exists (File "/dev/shm")

azubiSetup :: AzubiConfig
azubiSetup = azubiConfig Gentoo $ []
  & contains (File "~/azubi-config/azubi-config.cabal") [ "name:                azubi-config"
                                                        , "version:             0.1.0.0"
                                                        , "build-type:          Simple"
                                                        , "cabal-version:       >=1.10"
                                                        , ""
                                                        , "executable config"
                                                        , "  main-is:             config.hs"
                                                        , "  build-depends:       base >=4.8 && < 4.9"
                                                        , "                     , azubi"
                                                        , "  hs-source-dirs:      ."
                                                        , "  default-language:    Haskell2010"
                                                        ]
  & contains (File "~/azubi-config/config-example.hs") ( map (\c -> "-- | " ++ c ) azubiLogo
                                                        ++ [ ""
                                                           , "import Azubi"
                                                           , ""
                                                           , "main :: IO ()"
                                                           , "main = azubiMain $ azubi Gentoo $ []"
                                                           , "      & installed \"vim\""
                                                           , "      ! installed \"salt\""
                                                           ])
  & contains (File "~/azubi-config/README.md") [ "# Azubi Example"
                                               , ""
                                               , "## How to start"
                                               , ""
                                               , "copy or link the config-example.hs file to config.hs"
                                               , "and run cabal configure."
                                               , "now you can start editing config.hs and run cabal run."
                                               , "this will create a azubi.sh file you can run."
                                               , ""
                                               , "    cabal run && bash azubi.sh"]



-- next :
-- ------
-- commands
-- git


-- sketch on the syntax
--
-- main = do
--  azubi Gentoo $ []
--      & User.exists "palo" [ Uid 1000, Gid 1000 ]
--      & User.exists "renoise" [ Uid 1001, Home "/home/music" ]
--      & running "service"
--      & uptodate (Git "/home/palo/.dot_i3/" "git@github.com/mrVanDalo/dot_i3" "master")                  -- | does pulls
--      & uptodate (GitWithSubmodules "/home/palo/.dot_jack" "git@github.com/mrVanDalo/dot_jack" "master") -- | same but also takes car of submodules
--      & backup (File "/etc/ssh/") (Directory "/backups/etc/ssh") -- | will "cp -a /etc/ssh /backups/etc/ssh/`date`"
--      & published (Git "/home/palo/.dot_shell" "git@github.com/mrVanDalo/dot_shell" "master" "azubi wip message")  -- | creates commits and pushes on the master branch using default message
--
