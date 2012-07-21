import XMonad
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BorderResize
import XMonad.Layout.Monitor
import XMonad.Layout.NoBorders
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Cross
import XMonad.Layout.IM
import XMonad.Layout.WindowArranger
import XMonad.Actions.WindowGo (runOrRaise)
import qualified XMonad.StackSet as W

import Control.OldException(catchDyn,try)
import Control.Concurrent
import Data.Monoid
import Data.Ratio ((%))
import qualified Data.Map        as M
import DBus
import DBus.Connection
import DBus.Message

import System.Cmd
import System.Exit

import XMonad
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "gnome-terminal"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- Border colors for unfocused and focused windows, respectively.

-- Black Scheme -
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#373737"

-- White Scheme -
--myNormalBorderColor  = "#DDDDDD"
--myFocusedBorderColor = "#FFFFFF"

-- Blue Scheme --
-- myNormalBorderColor  = "#828596"
-- myFocusedBorderColor = "#323B84"

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
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
myWorkspaces    = ["Web","Term","pub","mail","irc","mpd","7","8","9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32" ]

centerFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ W.RationalRect 0.25 0.25 0.5 0.5) f
hugeCenterFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ W.RationalRect 0.15 0.15 0.7 0.7) f
fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
  
     -- Dock Struts
      ((modm, xK_F12 ), sendMessage ToggleStruts)

    -- Fullscreen 
    , ((modm, xK_F11 ), fullFloatFocused)

    -- Center
    , ((modm, xK_F10 ), centerFloatFocused)

    -- CENTER
    , ((modm, xK_F9 ), hugeCenterFloatFocused)
 
    -- launch a terminal
    , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window 
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Force Quit
    , ((controlMask .|. shiftMask, xK_Escape), spawn "xkill" )
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
     -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window

   , ( (modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Always resize
    , ((modm .|. controlMask              , xK_s    ), sendMessage  Arrange         )
    , ((modm .|. controlMask .|. shiftMask, xK_s    ), sendMessage  DeArrange       )
    , ((modm .|. controlMask              , xK_Left ), sendMessage (MoveLeft      1))
    , ((modm .|. controlMask              , xK_Right), sendMessage (MoveRight     1))
    , ((modm .|. controlMask              , xK_Down ), sendMessage (MoveDown      1))
    , ((modm .|. controlMask              , xK_Up   ), sendMessage (MoveUp        1))
    , ((modm                 .|. shiftMask, xK_Left ), sendMessage (IncreaseLeft  1))
    , ((modm                 .|. shiftMask, xK_Right), sendMessage (IncreaseRight 1))
    , ((modm                 .|. shiftMask, xK_Down ), sendMessage (IncreaseDown  1))
    , ((modm                 .|. shiftMask, xK_Up   ), sendMessage (IncreaseUp    1))
    , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  1))
    , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight 1))
    , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  1))
    , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp    1))

    -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    -- , ((modm , xK_b ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
--    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these
-- values.  If you change layout bindings be sure to use
-- 'mod-shift-space' after restarting (with 'mod-q') to reset your
-- layout state to the new defaults, as xmonad preserves your old
-- layout settings by default.  The available layouts.  Note that each
-- layout is separated by |||, which denotes layout choice.    
-- partitions the screen into two panes tiled = Tall nmaster delta
-- ratio

myLayout = avoidStruts $ smartBorders $ withIM (1%7) (ClassName "Tkabber") (tiled ||| Mirror tiled ||| noBorders Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

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
    [ resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 

------------------------------------------------------------------------
-- Startup
--
 
myStartupHook = do
  -- Compositing
--  spawn "$HOME/usr/bin/xcompcontrol" -- I want ma gpu
  -- Unity -- Now done in the session file
--  spawn "/usr/local/bin/unity-2d-spread"
--  spawn "/usr/local/bin/unity-2d-places"
--  spawn "/usr/local/bin/unity-2d-panel"
--  spawn "/usr/local/bin/unity-2d-launcher"
  -- Terms
  spawn "/usr/bin/tilda"
  -- Launchers
--  spawn "/usr/bin/gnome-pie" -- Yu buggy
  spawn "/usr/bin/synapse"
  -- GNOME
--  spawn "sleep 10 && /usr/bin/gnome-settings-daemon"
--  spawn "/usr/bin/gnome-panel"

------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X
-- event.  See the 'DynamicLog' extension for examples.  To emulate
-- dwm's status bar > logHook = dynamicLogDzen myLogHook = return ()
 
------------------------------------------------------------------------
-- Dbus UTIL

myPrettyPrinter dbus = defaultPP {
    ppOutput  = outputThroughDBus dbus
  , ppTitle   = returnBlank
  , ppCurrent = pangoColor "#ffffff" . wrap "[" "]" . pangoSanitize
  , ppVisible = pangoColor "#dddddd" . wrap "(" ")" . pangoSanitize
  , ppHidden  = wrap " " " "
  , ppUrgent  = pangoColor "red"
  }
 
-- This retry is really awkward, but sometimes DBus won't let us get our
-- name unless we retry a couple times.
getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
                                                getWellKnownName dbus)
 where
  tryGetName = do
    namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
    addArgs namereq [String "org.xmonad.Log", Word32 5]
    sendWithReplyAndBlock dbus namereq 0
    return ()

outputThroughDBus :: Connection -> String -> IO ()
outputThroughDBus dbus str = do
  let str' = "<font color=\"#dddddd\">" ++ str ++ "</span>"
  msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
  addArgs msg [String str']
  send dbus msg 0 `catchDyn` (\ (DBus.Error _ _ ) -> return 0)
  return ()

returnBlank :: String -> String
returnBlank x = ""

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<font color=\"" ++ fg ++ "\">"
  right = "</font>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
 where
  sanitize '>'  acc = "&gt;" ++ acc
  sanitize '<'  acc = "&lt;" ++ acc
  sanitize '\"' acc = "&quot;" ++ acc
  sanitize '&'  acc = "&amp;" ++ acc
  sanitize x    acc = x:acc

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

main = withConnection Session $ \ dbus -> do
         xmonad gnomeConfig {
	
        -- simple stuff
           terminal           = myTerminal,
           focusFollowsMouse  = myFocusFollowsMouse,
           borderWidth        = myBorderWidth,
           modMask            = myModMask,
           --numlockMask        = myNumlockMask,
           workspaces         = myWorkspaces,
           normalBorderColor  = myNormalBorderColor,
           focusedBorderColor = myFocusedBorderColor,
 
          -- key bindings
           keys               = myKeys,
           mouseBindings      = myMouseBindings,
 
         -- hooks, layouts
           layoutHook         = desktopLayoutModifiers myLayout,
           logHook            = logHook gnomeConfig >> dynamicLogWithPP (myPrettyPrinter dbus),
           startupHook        = myStartupHook,
           handleEventHook    = fullscreenEventHook,
           manageHook = composeAll [
                 -- Gui Elements, panels, docks, unity, gnome, kde, ...
                   className =? "Unity-2d-spread"    --> doFullFloat
    	         , className =? "Unity-2d-panel"     --> doIgnore
    	         , className =? "Unity-2d-launcher"  --> doIgnore
                 , className =? "Gnome-panel"        --> doIgnore
    	         , resource  =? "desktop_window"     --> doIgnore

                 -- Launchers
                 , className =? "Do"                 --> doIgnore
    	         , className =? "Gnome-pie"          --> doIgnore
    	         , className =? "Synapse"            --> doIgnore
        	 , className =? "Tilda"              --> doCenterFloat

                 -- Applications
 	         , className =? "MPlayer"            --> doFullFloat
    	         , className =? "Vlc"                --> doFullFloat
    	         , className =? "Software-center"    --> doCenterFloat
    	         , className =? "." 		     --> doCenterFloat
    	         , className =? "Wine" 		     --> doFloat
    	         , className =? "Stickynotes_applet" --> doFloat

                 -- Special window States
    	    	 , isDialog                          --> doCenterFloat
    	         , isFullscreen                      --> doFullFloat 

                 -- Groups
	         , manageDocks
	 --      , manageHook gnomeConfig
         --      , manageHook kdeConfig
	]
   }
