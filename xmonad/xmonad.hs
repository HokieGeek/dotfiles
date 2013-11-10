-- Imports {{{
import Control.Monad(liftM2)

import System.IO

import XMonad
import XMonad.Core
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.Run
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
--}}}

-- Hooks {{{
-- Manage Hooks {{{
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Gimp"                           --> doFloat <+> viewShift "-"
    , className =? "VASSAL-launch-ModuleManager"    --> doFloat <+> doShift "9"
    , className =? "VASSAL-launch-Player"           --> doFloat <+> doShift "9"
    , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat <+> viewShift "2" -- Hangouts
    , appName =? "crx_hmjkmjkepdijhoojdojkdfohbdgmmhki" --> doFloat <+> viewShift "7" -- Google Keep

    -- , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doF copyToAll
    ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift
-- }}}
-- Log Hooks {{{
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             =   ""
      , ppOutput            =   hPutStrLn h
    }
-- }}}
-- }}}

-- TODO: need to define the keybindings!!
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]

-- Main {{{
workspaceStatusBar = "sleep 3s; dzen2 -x '1440' -y '0' -h '16' -w '270' -fg '#FFFFFF' -bg '#1B1D1E'"
conkyStatusBar = "conky -c ~/.conky/xmonad.conf | dzen2 -y '0' -x '2732' -w '1366' -h '16' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF'"
main = do
        dzenLeftBar <- spawnPipe workspaceStatusBar
        dzenRightBar <- spawnPipe conkyStatusBar
        xmonad $ defaultConfig
            { workspaces = myWorkspaces
            , modMask = mod4Mask
            , manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks
            , layoutHook = avoidStruts . smartBorders $ layoutHook defaultConfig
            , logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
            , borderWidth = 0
            } `additionalKeys`
            [ (((mod4Mask .|. mod1Mask), xK_Left), prevWS)
            , (((mod4Mask .|. mod1Mask), xK_Right), nextWS)
            , (((mod4Mask .|. shiftMask), xK_Left), shiftToPrev)
            , (((mod4Mask .|. shiftMask), xK_Right), shiftToNext)
            , (((mod4Mask .|. mod1Mask), xK_Up), moveTo Next EmptyWS)
            , (((mod4Mask .|. mod1Mask), xK_Down), toggleWS)
            -- , (((mod1Mask), xK_0), W.greedyView "0")
            , (((mod4Mask .|. controlMask .|. shiftMask), xK_Left), spawn "xbacklight -inc 20")
            , (((mod4Mask .|. controlMask .|. shiftMask), xK_Right), spawn "xbacklight -dec 20")
            -- XF86AudioRaiseVolume
            , ((0, 0x1008FF13), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 3+")
            -- XF86AudioLowerVolume
            , ((0, 0x1008FF11), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 3-")
            -- XF86AudioMute
            , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
            , ((0, xK_Print), spawn "scrot")
            , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
            ]
            -- ++
            -- [((m .|. modMask, k), windows $ f i)
                -- | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
                -- , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--}}}
