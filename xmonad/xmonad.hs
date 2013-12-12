-- Imports {{{
import Control.Monad(liftM2)

import Graphics.X11.ExtraTypes.XF86

import System.IO

import XMonad
import XMonad.Core
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.MagicFocus
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.ResizableTile
import XMonad.Util.Run
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
-- }}}

-- Local Variables {{{
rangerExec = "export EDITOR=vim; " ++ myTerminal ++ " -e ranger"

modm = mod4Mask
myTerminal = "urxvtc"

myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]
myWorkspaceKeys = [xK_1..xK_9] ++ [xK_0,xK_minus,xK_equal]

myAppGSMenu = [ ("Chromium", "chromium")
              , ("Terminal", myTerminal)
              , ("Ranger", rangerExec)
              , ("Skype", "skype")
              , ("gVim", "gvim")
              , ("Eclipse", "eclipse")
              , ("Alsa Mixer", myTerminal ++ " -e alsamixer")
              , ("VLC", "vlc")
              , ("Gimp", "gimp")
              , ("Irssi", myTerminal ++ " -e irssi")
              -- , ("minicom", myTerminal ++ " -e minicom") -- specific menu for the two configs
              -- deluge, playonlinux, virtualbox, mpd/c
              ]
--}}}
-- Local Methods {{{
-- http://ixti.net/software/2013/09/07/xmonad-action-gridselect-spawnselected-with-nice-titles.html
mySpawnSelected :: [(String, String)] -> X()
mySpawnSelected lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig
-- }}}

-- Hooks {{{
-- Manage {{{
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Xmessage"   --> doCenterFloat
    , className =? "Gimp"       --> viewShift "-"
    -- , [ className =? a --> doFloat <+> doShift "=" | a <- vassalClassnames ]
    -- , [ appName =? b --> doFloat <+> viewShift "1" | b <- floatingInOne ]
    , className =? "VASSAL-launch-ModuleManager"  --> doFloat <+> doShift "="
    , className =? "VASSAL-launch-Player" --> doFloat <+> doShift "="
    , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat <+> viewShift "1" -- Hangouts
    , appName =? "crx_hmjkmjkepdijhoojdojkdfohbdgmmhki" --> viewShift "1" -- Google Keep
    ]
    where 
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        -- vassalClassnames = ["VASSAL-launch-ModuleManager", "VASSAL-launch-Player"]
        -- floatingInOne = ["crx_nckgahadagoaajjgafhacjanaoiihapd",  -- Hangouts
                         -- "crx_hmjkmjkepdijhoojdojkdfohbdgmmhki"] -- Google Keep
-- }}}
-- Log {{{
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#e87511" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             =   ""
      , ppOutput            =   hPutStrLn h
    }
-- }}}
-- Layout{{{
incDelta = 3/100
myDefaultLayout = tiledStd ||| Mirror tiledStd ||| Full
    where
        tiledStd = Tall 1 incDelta (1/2) -- number of masters, % to inc when resizing, % of screen used by master
myLayoutHook =   onWorkspace "1" (Tall 1 incDelta (3/4))
               $ onWorkspace "2" (magicFocus (Mirror (Tall 1 incDelta (2/3))))
               $ myDefaultLayout
-- }}}
-- }}}

-- Keybindings {{{
myKeys =
            [ ((modm, xK_q), spawn "~/.xmonad/restart")
            , ((modm, xK_a), spawn "dmenu_run")
            -- , ((modm .|. shiftMask, xK_a), spawn "gmrun") -- Is there a point to this?
            , ((modm, xK_e), spawn rangerExec)
            , (((controlMask .|. shiftMask), xK_Escape), spawn (myTerminal ++ " -e htop"))
            , ((0, xF86XK_Sleep), spawn "sudo /usr/sbin/pm-suspend")
            , ((modm, xK_w), spawn "$HOME/.bin/rotate-wallpaper $HOME/.look/bgs")
            -- GridSelect
            , ((modm, xK_z), mySpawnSelected myAppGSMenu)
            , (((modm .|. shiftMask), xK_z), goToSelected defaultGSConfig)
            , ((modm, xK_x), gridselectWorkspace defaultGSConfig W.greedyView)
            , (((modm .|. shiftMask), xK_x), gridselectWorkspace defaultGSConfig (\ws -> W.greedyView ws . W.shift ws))
            -- Workspace helpers
            , (((modm .|. mod1Mask), xK_k), prevWS)
            , (((modm .|. mod1Mask), xK_j), nextWS)
            , (((controlMask .|. mod1Mask), xK_Left), prevWS)
            , (((controlMask .|. mod1Mask), xK_Right), nextWS)
            , (((modm .|. mod1Mask), xK_l), moveTo Next NonEmptyWS)
            , (((modm .|. mod1Mask), xK_h), moveTo Prev NonEmptyWS)
            , (((modm .|. mod1Mask .|. shiftMask), xK_l), moveTo Next EmptyWS)
            , (((modm .|. mod1Mask .|. shiftMask), xK_h), moveTo Prev EmptyWS)
            , (((modm .|. mod1Mask .|. shiftMask), xK_j), toggleWS)
            -- Window helpers
            , (((modm .|. mod1Mask), xK_space), windows W.swapMaster)
            -- Backlight
            , (((modm .|. controlMask .|. shiftMask), xK_Left), spawn "xbacklight -inc 20")
            , (((modm .|. controlMask .|. shiftMask), xK_Right), spawn "xbacklight -dec 20")
            -- Volume
            , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 3+")
            , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 3-")
            , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle; amixer -q set Speaker toggle; amixer -q set Headphone toggle")
            , ((modm, xF86XK_AudioMute), spawn (myTerminal ++ " -e alsamixer"))
            -- PrintScreen
            , ((0, xK_Print), spawn "scrot")
            , ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s")
            ]
            ++
            [((m .|. modm, k), windows $ f i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
            ++
            [((modm .|. controlMask, k), windows $ swapWithCurrent i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys]
--}}}

-- Main {{{
compmgr = "xcompmgr"
workspaceStatusBar = "sleep 3s; dzen2 -fn '-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*' -x '1440' -y '0' -h '16' -w '220' -fg '#FFFFFF' -bg '#1B1D1E'"
conkyStatusBar = "conky -c ~/.conky/xmonad.conf | dzen2 -y '0' -x '2732' -w '1366' -h '16' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -fn '-*-terminus-medium-r-*-*-12-*-*-*-*-*-*-*'"
main = do
        compMgrStart <- spawn compmgr
        dzenLeftBar <- spawnPipe workspaceStatusBar
        dzenRightBar <- spawnPipe conkyStatusBar
        xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } -- TODO: figure out appropriate color
               $ defaultConfig
            { workspaces = myWorkspaces
            , terminal = myTerminal
            , modMask = modm
            , focusFollowsMouse = False
            , manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks
            , layoutHook = avoidStruts . smartBorders $ myLayoutHook
            , logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0.7 >> updatePointer (Relative 1 1)
            , handleEventHook = fadeWindowsEventHook <+> fullscreenEventHook
            , borderWidth = 0
            } `additionalKeys` myKeys
--}}}

-- vim: set foldmethod=marker:
