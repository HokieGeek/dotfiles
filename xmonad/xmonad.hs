-- Imports {{{
import Data.IORef

import Control.Monad(liftM2)

import Graphics.X11.ExtraTypes.XF86

import System.IO

import XMonad
import XMonad.Core
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation -- historyHook
import XMonad.Actions.RotSlaves
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops --fullScreenEventHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows -- fadeWindowEventHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.MagicFocus
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- }}}

-- Local Variables {{{
rangerExec = "export EDITOR=vim; " ++ myTerminal ++ " -e ranger"

modm = mod4Mask
myTerminal = "urxvtc"
colorForeground = "#9F0AC4"
colorDimmed = "#9F0AC4"
colorBackground = "#1B1D1E"
font = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*"

myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]
myWorkspaceKeys = [xK_1..xK_9] ++ [xK_0,xK_minus,xK_equal]

myAppGSMenu = [ ("Chromium", "chromium")
              , ("Terminal", myTerminal)
              , ("Irssi", myTerminal ++ " -e irssi")
              , ("Gimp", "gimp")
              , ("Skype", "skype")
              , ("Eclipse", "eclipse")
              , ("MPC", myTerminal ++ " -e ncmpcpp")
              , ("VirtualBox", "virtualbox")
              , ("Ranger", rangerExec)
              , ("VLC", "vlc")
              , ("Alsa Mixer", myTerminal ++ " -e alsamixer")
              , ("gVim", "gvim")
              , ("Steam", "/usr/share/playonlinux/playonlinux --run \"Steam\" %F")
              -- , ("Deluge", "deluge")
              -- , ("minicom", myTerminal ++ " -e minicom") -- specific menu for the two configs
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
    , className =? "VASSAL-launch-ModuleManager"  --> doFloat <+> doShift "="
    , className =? "VASSAL-launch-Player" --> doFloat <+> doShift "="
    , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat -- Hangouts
    , appName =? "crx_hmjkmjkepdijhoojdojkdfohbdgmmhki" --> viewShift "1" -- Google Keep
    ] <+> manageDocks
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
-- }}}
-- Log {{{
myLogHook h = (dynamicLogWithPP (myDzen h)) <+> historyHook
                                            >> fadeInactiveLogHook 0.5
                                            >> updatePointer (Relative 1 1)

myDzen h = defaultPP
    {
        ppCurrent           =   dzenColor colorForeground colorBackground . pad . dzenWorkspaceSymbol
      , ppVisible           =   dzenColor "white" colorBackground . pad . dzenWorkspaceSymbol
      , ppHidden            =   dzenColor "white" colorBackground . pad . dzenWorkspaceSymbol
      , ppHiddenNoWindows   =   dzenColor "#363636" colorBackground . pad . dzenWorkspaceSymbol
      , ppUrgent            =   dzenColor "#ff0000" colorBackground . pad . dzenWorkspaceSymbol
      , ppLayout            =   (\x -> "")
      , ppTitle             =   (\x -> "")
      , ppSep               =   ""
      , ppWsSep             =   ""
      , ppOutput            =   hPutStrLn h
    }
    where
        dzenWorkspaceSymbol x = "^i(/home/andres/.xmonad/imgs/workspace.xbm)"
-- }}}
-- Layout{{{
incDelta = 3/100
myDefaultLayout = tiledStd ||| Mirror tiledStd ||| Full
    where
        tiledStd = ResizableTall 1 incDelta (1/2) [] -- # masters, % to inc when resizing, % of screen used by master, slaves
myLayoutHook = avoidStruts
               $ onWorkspace "1" ((ResizableTall 1 incDelta (3/4) []) ||| Full)
               $ onWorkspace "2" (magicFocus (Mirror (ResizableTall 1 incDelta (2/3) [])) ||| Full)
               $ myDefaultLayout
-- }}}
-- HandleEvent {{{
myHandleEventHook = fadeWindowsEventHook <+> fullscreenEventHook
-- }}}
-- }}}

-- Keybindings {{{
myKeys =    [ ((modm, xK_q), spawn "~/.xmonad/restart")
            , ((modm, xK_a), spawn "dmenu_run")
            , (((controlMask .|. shiftMask), xK_Escape), spawn (myTerminal ++ " -e htop"))
            , ((0, xF86XK_Sleep), spawn "sudo /usr/sbin/pm-suspend")
            , ((modm, xK_w), spawn "$HOME/.bin/rotate-wallpaper $HOME/.look/bgs")
            , ((0, xK_F1), spawn "xinput -disable 'ELAN Touchscreen'")
            -- GridSelect
            , ((modm, xK_z), mySpawnSelected myAppGSMenu)
            , ((modm, xK_x), goToSelected defaultGSConfig)
            , (((modm .|. shiftMask), xK_x), gridselectWorkspace defaultGSConfig (\ws -> W.greedyView ws . W.shift ws))
            -- Workspace helpers
            , (((modm .|. mod1Mask), xK_k), prevWS)
            , (((modm .|. mod1Mask), xK_j), nextWS)
            , (((modm .|. shiftMask), xK_k), moveTo Prev NonEmptyWS)
            , (((modm .|. shiftMask), xK_j), moveTo Next NonEmptyWS)
            , ((modm, xK_BackSpace), toggleWS)
            , ((modm, xK_n), moveTo Next EmptyWS)
            , (((modm .|. shiftMask), xK_n), shiftTo Next EmptyWS)
            , (((modm .|. mod1Mask), xK_n), moveTo Next EmptyWS <+> spawn "chromium")
            , (((modm .|. controlMask), xK_n), moveTo Next EmptyWS <+> spawn myTerminal)
            -- Window helpers
            , (((modm .|. shiftMask), xK_BackSpace), nextMatch History (return True))
            , (((modm .|. shiftMask), xK_h), sendMessage MirrorShrink) -- shrink the master area
            , (((modm .|. shiftMask), xK_l), sendMessage MirrorExpand) -- expand the master area
            , (((modm .|. controlMask), xK_j), rotSlavesDown)
            , (((modm .|. controlMask), xK_k), rotSlavesUp)
            -- Backlight
            , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 20")
            , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 20")
            -- Volume
            , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 3+")
            , ((shiftMask, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 100")
            , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 3-")
            , ((shiftMask, xF86XK_AudioLowerVolume), spawn "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; amixer -q set Master playback 30")
            , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle; amixer -q set Speaker toggle; amixer -q set Headphone toggle")
            , ((shiftMask, xF86XK_AudioMute), spawn (myTerminal ++ " -e alsamixer"))
            -- PrintScreen
            , ((0, xK_Print), spawn "scrot")
            , ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s")
            -- Media
            , ((shiftMask, xF86XK_AudioPlay), spawn "chromium --new-window https://play.google.com/music/listen#/ap/queue")
            , ((controlMask, xF86XK_AudioPlay), spawn "chromium --new-window http://pandora.com")
            -- ((0, xF86XK_AudioStop), spawn "???")
            -- ((0, xF86XK_AudioPrev), spawn "???")
            -- ((0, xF86XK_AudioNext), spawn "???")

            -- , ((modm, xK_F10), addWorkspace "y")
            -- , (((modm .|. shiftMask), xK_F10), removeEmptyWorkspace)
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
workspaceStatusBar = "sleep 3s; dzen2 -fn '" ++ font ++ "' -x '1440' -y '0' -h '16' -w '280' -fg '#FFFFFF' -bg '" ++ colorBackground ++ "' -ta l"
conkyStatusBar = "~/.conky/statusbar.py --color-fg '" ++ colorForeground ++ "' > /tmp/xmonad.conkyrc && conky -b -c /tmp/xmonad.conkyrc | dzen2 -y '0' -x '2732' -w '1366' -h '16' -ta 'r' -bg '" ++ colorBackground ++ "' -fg '#FFFFFF' -fn '" ++ font ++ "'"
main = do
        compMgrStart <- spawn compmgr
        dzenRightBar <- spawn conkyStatusBar
        dzenLeftBar  <- spawnPipe workspaceStatusBar
        xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", colorForeground, "-xs", "1"] }
               $ defaultConfig
            { workspaces = myWorkspaces
            , terminal = myTerminal
            , modMask = modm
            , focusFollowsMouse = False
            , borderWidth = 0
            , manageHook = myManageHook
            , layoutHook = myLayoutHook
            , logHook = myLogHook dzenLeftBar
            , handleEventHook = myHandleEventHook
            }
            `additionalKeys` myKeys
--}}}

-- vim: set foldmethod=marker number relativenumber:
