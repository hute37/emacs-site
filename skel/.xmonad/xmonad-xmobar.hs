 import XMonad
 import XMonad.Hooks.DynamicLog
 import XMonad.Hooks.ManageDocks
 import XMonad.Util.Run
 import System.IO

 main = do
 xmproc <- spawnPipe "/usr/bin/xmobar /path/to/your/config-file"
 xmonad $ defaultConfig
      { manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = avoidStruts $ layoutHook defaultConfig
      , logHook = dynamicLogWithPP xmobarPP
           { ppOutput = hPutStrLn xmproc
           , ppTitle = xmobarColor "blue" "" . shorten 50
           , ppLayout = const "" -- to disable the layout info on xmobar
           }
      }

