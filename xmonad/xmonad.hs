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
import XMonad.Util.EZConfig(additionalKeys)

import qualified XMonad.StackSet as W
--}}}

-- Hooks {{{
-- Manage Hooks {{{
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Gimp"                           --> doFloat
    , className =? "VASSAL-launch-ModuleManager"    --> doFloat <+> doShift "9"
    , className =? "VASSAL-launch-Player"           --> doFloat <+> doShift "9"
    , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat <+> viewShift "2" -- Hangouts
    , appName =? "crx_hmjkmjkepdijhoojdojkdfohbdgmmhki" --> doFloat <+> viewShift "7" -- Google Keep

    -- , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doF copyToAll
    ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift
-- }}}
-- Log Hooks {{{
myBitmapsDir = "/home/andres/dzen-temp"
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
-- }}}
-- }}}

-- TODO: why do these not work?
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]

-- Main {{{
myXmonadBar = "dzen2 -x '1440' -y '0' -h '24' -w '640' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myStatusBar = "conky -c /home/andres/.conky/conky_dzen | dzen2 -x '2080' -w '1040' -h '24' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -y '0'"
main = do
        dzenLeftBar <- spawnPipe myXmonadBar
        dzenRightBar <- spawnPipe myStatusBar
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
            , (((mod4Mask .|. mod1Mask), xK_Down), moveTo Next EmptyWS)
            , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
            , ((0, xK_Print), spawn "scrot")
            , ((mod4Mask .|. shiftMask, xK_S), spawn "sudo /usr/sbin/pm-suspend")
            , (((mod4Mask .|. controlMask .|. shiftMask), xK_Left), spawn "xbacklight -inc 20")
            , (((mod4Mask .|. controlMask .|. shiftMask), xK_Right), spawn "xbacklight -dec 20")
            ]
--}}}
