-- Imports {{{
import Control.Monad(liftM2)

import Graphics.X11.ExtraTypes.XF86

import System.IO

import XMonad
import XMonad.Core
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.Run
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
--}}}

-- Local Variables {{{
modm = mod4Mask
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]
-- myWorkspaces = ["α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ"]
myWorkspaceKeys = [xK_1..xK_9] ++ [xK_0,xK_minus,xK_equal]
myDefaultGSMenu = ["chromium-browser" -- C
                  , "terminator", "netflix-desktop", "gvim", "vlc" -- S, E, N, W
                  , "Gimp"
                  ]
-- Gimp, Eclipse, Arduino... minicom?
--}}}

-- Hooks {{{
-- Manage Hooks {{{
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Xmessage"   --> doCenterFloat
    , className =? "Gimp"       --> doFloat <+> viewShift "-"
    , className =? "VASSAL-launch-ModuleManager"  --> doFloat <+> doShift "="
    , className =? "VASSAL-launch-Player" --> doFloat <+> doShift "="
    , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat <+> viewShift "2" -- Hangouts
    , appName =? "crx_hmjkmjkepdijhoojdojkdfohbdgmmhki" --> doFloat <+> viewShift "2" -- Google Keep

    -- , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doF copyToAll
    ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift
-- }}}
-- Log Hooks {{{
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
-- Fade Hooks {{{
myFadeHook0 = composeAll
    [ isUnfocused --> transparency 1.0
    ,                 opaque
    ]
myFadeHook :: X ()
myFadeHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1
-- }}}
-- }}}

-- Keybindings {{{
myKeys =
            [ ((modm, xK_q), spawn "~/.xmonad/restart")
            , ((modm, xK_a), spawn "gmrun")
            , ((modm, xK_z), goToSelected defaultGSConfig)
            , (((modm .|. shiftMask), xK_z), spawnSelected defaultGSConfig myDefaultGSMenu)
            -- Workspace helpers
            , (((modm .|. mod1Mask), xK_Left), prevWS)
            , (((modm .|. mod1Mask), xK_Right), nextWS)
            , (((modm .|. shiftMask), xK_Left), shiftToPrev)
            , (((modm .|. shiftMask), xK_Right), shiftToNext)
            , (((modm .|. mod1Mask), xK_Up), moveTo Next EmptyWS)
            , (((modm .|. mod1Mask), xK_Down), toggleWS)
            -- Backlight
            , (((modm .|. controlMask .|. shiftMask), xK_Left), spawn "xbacklight -inc 20")
            , (((modm .|. controlMask .|. shiftMask), xK_Right), spawn "xbacklight -dec 20")
            -- Volume
            , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 3+")
            , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 3-")
            , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle; amixer -q set Speaker toggle; amixer -q set Headphone toggle")
            -- PrintScreen
            , ((0, xK_Print), spawn "scrot")
            , ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s")
            -- Sleep!
            , ((0, xF86XK_Sleep), spawn "sudo /usr/sbin/pm-suspend")
            ]
            ++
            [((m .|. modm, k), windows $ f i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- }}}

-- Main {{{
compmgr = "xcompmgr"
-- workspaceStatusBar = "sleep 3s; dzen2 -x '1440' -y '0' -h '16' -w '270' -fg '#FFFFFF' -bg '#1B1D1E'"
-- workspaceStatusBar = "sleep 3s; dzen2 -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -x '1440' -y '0' -h '16' -w '270' -fg '#FFFFFF' -bg '#1B1D1E'"
workspaceStatusBar = "sleep 3s; dzen2 -fn '-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*' -x '1440' -y '0' -h '16' -w '230' -fg '#FFFFFF' -bg '#1B1D1E'"
conkyStatusBar = "conky -c ~/.conky/xmonad.conf | dzen2 -y '0' -x '2732' -w '1366' -h '16' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -fn '-*-terminus-medium-r-*-*-12-*-*-*-*-*-*-*'"
-- conkyStatusBar = "conky -c ~/.conky/xmonad.conf | dzen2 -y '0' -x '2732' -w '1366' -h '16' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -fn '-*-dejavusans-*-r-*-*-12-*-*-*-*-*-*-*'"
main = do
        compMgrStart <- spawn compmgr
        dzenLeftBar <- spawnPipe workspaceStatusBar
        dzenRightBar <- spawnPipe conkyStatusBar
        xmonad $ defaultConfig
            { workspaces = myWorkspaces
            , modMask = modm
            , manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks
            , layoutHook = avoidStruts . smartBorders $ layoutHook defaultConfig
            , logHook = fadeWindowsLogHook myFadeHook0 <+> myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
            -- , logHook = myFadeHook <+> myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
            -- , logHook = fadeWindowsLogHook myFadeHook -- <+> myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
            -- , handleEventHook = fullscreenEventHook
            , handleEventHook = fadeWindowsEventHook
            , borderWidth = 0
            } `additionalKeys` myKeys
--}}}
