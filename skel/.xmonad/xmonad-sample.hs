--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
 
import XMonad
import System.Exit

import XMonad.Config.Gnome


import XMonad.Actions.Submap
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.Input
import XMonad.Actions.DynamicWorkspaces

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive

import XMonad.Layout.Spiral
import XMonad.Util.Scratchpad

import Graphics.X11.Xlib
import Graphics.X11.Xinerama
-- import Graphics.X11.Xlib.Extras
-- import Graphics.X11.Xlib.Event

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.List
import Data.Function

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "x-terminal-emulator"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 2
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
--myModMask       = mod4Mask
myModMask       = mod1Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
--myWorkspaces    = ["emacs","lunatique","www", "terminals"] ++ map show [5..10] ++ ["wifi", "music", "SP"]

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#aa0000"

myXPConfig = defaultXPConfig

-- sendKeyPress :: KeyMask -> KeySym -> X ()
-- sendKeyPress = userCode $ withDisplay sendKeyPress'
-- 
-- sendKeyPress' :: Display -> KeyMask -> KeySym -> X ()
-- sendKeyPress' dpy mask key = do
--   root <- asks theRoot
--   time <- currentTime
--   evt  <-


renameWS :: String -> X ()
renameWS newTag = windows $ \s -> let old = W.tag $ W.workspace $ W.current s
                                  in W.renameTag old newTag s

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return)       , spawn $ XMonad.terminal conf)
    , ((modMask,               xK_c)            , spawn $ XMonad.terminal conf)
 
    , ((modMask,               xK_p     )       , spawn "mpc toggle")
    , ((modMask,               xK_bracketright) , spawn "mpc volume +5")
    , ((modMask,               xK_bracketleft)  , spawn "mpc volume -5")
    , ((modMask .|. shiftMask, xK_l)            , spawn "gnome-screensaver-command --lock")

    , ((modMask              , xK_x)            , spawn "xmodmap ~/.Xmodmap")
 
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)
 
    -- Restart xmonad
    , ((modMask .|. shiftMask, xK_r     ),
       broadcastMessage ReleaseResources >> restart "xmonad" True)

    , ((modMask              , xK_g     ),
       workspacePrompt  myXPConfig (windows . W.greedyView))
    , ((modMask .|. shiftMask, xK_g     ),
       workspacePrompt  myXPConfig (windows . W.shift))

    -- C-t submap
    , ((controlMask, xK_t)       , submap . M.fromList $
       [ ((controlMask,  xK_t)      ,   toggleWS) 
       , ((0,            xK_Tab)    ,   windows W.focusDown) -- @@ Move focus to the next window
       , ((shiftMask,    xK_Tab)    ,   windows W.focusUp  ) -- @@ Move focus to the previous window
       , ((0,            xK_c)      ,   spawn $ XMonad.terminal conf)
       , ((0,            xK_k)      ,   kill)
       , ((0,            xK_Return) ,   windows W.swapMaster)
--       , ((shiftMask,    xK_1)      ,   spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
       , ((shiftMask,    xK_1)      ,   scratchpadSpawnActionTerminal "gterm")
       , ((0,            xK_p)      ,   spawn "/home/nelhage/bin/viewpdf")
       , ((0,            xK_s)      ,   sshPrompt myXPConfig)

       , ((shiftMask,    xK_s)      ,   spawn "/usr/bin/tracker-search-tool")
       , ((0,            xK_x)      ,   spawn "/home/nelhage/bin/rp-hm-complete.sh")

       , ((0,            xK_g)      ,   workspacePrompt  myXPConfig (windows . W.greedyView))
       , ((shiftMask,    xK_g)      ,   workspacePrompt  myXPConfig (windows . W.shift))
       , ((0,            xK_n)      ,   inputPrompt myXPConfig "Workspace name" ?+ addWorkspace)
       , ((shiftMask,    xK_k)      ,   removeWorkspace)

       , ((0,            xK_r)      ,   renameWorkspace myXPConfig)
       , ((0,            xK_q)      ,   viewScreen 0)
       , ((0,            xK_w)      ,   viewScreen 1)




-- ///////////////////////////////////////////////////////

-- a basic CycleWS setup

--   , ((modm,               xK_Down),  nextWS)
--   , ((modm,               xK_Up),    prevWS)
--   , ((modm .|. shiftMask, xK_Down),  shiftToNext)
--   , ((modm .|. shiftMask, xK_Up),    shiftToPrev)
--   , ((modm,               xK_Right), nextScreen)
--   , ((modm,               xK_Left),  prevScreen)
--   , ((modm .|. shiftMask, xK_Right), shiftNextScreen)
--   , ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
--   , ((modm,               xK_z),     toggleWS)

-- If you want to follow the moved window, you can use both actions: 

--  , ((modm .|. shiftMask, xK_Down), shiftToNext >> nextWS)
--  , ((modm .|. shiftMask, xK_Up),   shiftToPrev >> prevWS)

-- You can also get fancier with moveTo, shiftTo, and findWorkspace. 
-- For example: 

--   , ((modm     , xK_f), moveTo Next EmptyWS)  -- find a free workspace
--   , ((modm .|. controlMask, xK_Right),        -- a crazy keybinding!
--         do t <- findWorkspace getSortByXineramaRule Next NonEmptyWS 2
--            windows . view $ t       

-- ///////////////////////////////////////////////////////


       , ((modMask,      xK_Left)   ,   prevWS)
       , ((modMask,      xK_Right)  ,   nextWS)

       ] ++
       [((0, k), windows $ W.greedyView i)
        | (i, k) <- zip (take 10 (XMonad.workspaces conf)) [xK_1 ..]
       ])
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((modMask .|. mask, key), f sc)
        | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button3, Raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button2, Set the window to floating mode and resize by dragging
    , ((modMask, button2), (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $
           tiled
           ||| Mirror tiled
           ||| Full
           ||| tabbed shrinkText defaultTheme
           ||| threeCol
--           ||| spiral (4/3)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     threeCol = ThreeCol nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 2/100
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
--    , className =? "display"        --> doFloat
    , className =? "Wpa_gui"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , title     =? "zsh-onetime"    --> doFloat
    , manageDocks
    , scratchpadManageHook (W.RationalRect 0.125 0.25 0.75 0.5)
    ]
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook = fadeInactiveLogHook 0xA0000000
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad defaults
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults = gnomeConfig {
      -- simple stuff
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
--        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook
    }

