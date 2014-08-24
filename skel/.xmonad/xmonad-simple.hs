--
    -- An example, simple ~/.xmonad/xmonad.hs file.
    -- It overrides a few basic settings, reusing all the other defaults.
    --

import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS


myManageHook = composeAll [
  isFullscreen --> doFullFloat
  , (className =? "Pidgin" <&&> (title =? "Pidgin" <||> title =? "Accounts")) --> doCenterFloat
  , (className =? "Pidgin") --> doShift "3"
  , (className =? "Gnome-panel" <&&> title =? "Run Application") --> doCenterFloat
  , (className =? "Gcr-prompter") --> doCenterFloat
]
    

--main = xmonad $ defaultConfig
main = xmonad $ gnomeConfig
{ 
    borderWidth        = 2
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#3300ff"
  , terminal           = "evilvte"
--, modMask            = mod4Mask
  , manageHook       = myManageHook <+> manageHook gnomeConfig
  , layoutHook  = smartBorders (layoutHook gnomeConfig)
}


