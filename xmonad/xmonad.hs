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

-- unmuteAllChannels = "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; "
unmuteAllChannels = "amixer -q set Master unmute ; "
xF86XK_AudioMicMute :: KeySym
xF86XK_AudioMicMute = 0x1008ffb2
-- xF86XK_AudioMicMute = 269025202

modm = mod4Mask
myTerminal = "urxvtc"
-- myBrowser = "chromium-dev"
myBrowser = "google-chrome-stable"
colorForeground = "#9F0AC4"
colorDimmed = "#9F0AC4"
colorBackground = "#1B1D1E"
font = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*"

myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]
myWorkspaceKeys = [xK_1..xK_9] ++ [xK_0,xK_minus,xK_equal]

myAppGSMenu = [ ("Chrome", myBrowser)
              -- , ("Netflix", "netflix-desktop")
              , ("Netflix", myBrowser ++ " --new-window http://netflix.com")
              , ("Irssi", myTerminal ++ " -e irssi")
              , ("Gimp", "gimp")
              , ("Skype", "skype")
              , ("Eclipse", "eclipse")
              , ("MPC", myTerminal ++ " -e ncmpcpp")
              , ("VirtualBox", "virtualbox")
              -- , ("Ranger", rangerExec)
              -- , ("Boardspace", myBrowser ++ " http://boardspace.net/cgi-bin/login.cgi?pname=HokieGeek&language=English")
              , ("Cheese", "cheese")
              , ("VLC", "vlc")
              , ("PlayOnLinux", "/usr/bin/playonlinux")
              , ("gVim", "gvim")
              , ("Alsa Mixer", myTerminal ++ " -e alsamixer")
              -- , ("Boardspace", "firefox http://boardspace.net/cgi-bin/login.cgi?pname=HokieGeek&language=English")
              -- , ("Steam", "/usr/share/playonlinux/playonlinux --run \"Steam\" %F")
              -- , ("Deluge", "deluge")
              -- , ("minicom", myTerminal ++ " -e minicom") -- specific menu for the two configs
              ]
-- gamesMenu = [
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
-- Don't forget to update keybindings-help.txt
myKeys =    [ ((modm, xK_q), spawn "~/.xmonad/restart")
            , ((modm, xK_a), spawn "dmenu_run")
            , ((mod1Mask, xK_F4), kill)
            , (((modm .|. controlMask .|. shiftMask), xK_slash), spawn "xmessage -file $HOME/.xmonad/keybindings-help.txt")
            , (((controlMask .|. shiftMask), xK_Escape), spawn (myTerminal ++ " -e htop"))
            , ((0, xF86XK_Sleep), spawn "sudo /usr/sbin/pm-suspend")
            , ((modm, xK_w), spawn "$HOME/.bin/rotate-wallpaper $HOME/.look/bgs")
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
            , (((modm .|. mod1Mask), xK_n), moveTo Next EmptyWS <+> spawn myBrowser)
            , (((modm .|. controlMask), xK_n), moveTo Next EmptyWS <+> spawn myTerminal)
            -- Window helpers
            , (((modm .|. shiftMask), xK_BackSpace), nextMatch History (return True)) -- Is this necessary?!
            , (((modm .|. shiftMask), xK_h), sendMessage MirrorShrink) -- shrink the master area
            , (((modm .|. shiftMask), xK_l), sendMessage MirrorExpand) -- expand the master area
            , (((modm .|. controlMask), xK_j), rotSlavesDown)
            , (((modm .|. controlMask), xK_k), rotSlavesUp)
            -- Backlight
            , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 20")
            , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 20")
            -- Volume
            , ((0, xF86XK_AudioRaiseVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 3+"))
            , ((shiftMask, xF86XK_AudioRaiseVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 100"))
            , ((0, xF86XK_AudioLowerVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 3-"))
            , ((shiftMask, xF86XK_AudioLowerVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 30"))
            , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
            , ((shiftMask, xF86XK_AudioMute), spawn (myTerminal ++ " -e alsamixer"))
            , ((0, xF86XK_AudioMicMute), spawn "amixer set Capture toggle")
            -- PrintScreen
            , ((0, xK_Print), spawn "scrot")
            , ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s")
            -- Media
            , ((shiftMask, xF86XK_AudioPlay), spawn (myBrowser ++ " --new-window https://play.google.com/music/listen#/ap/queue"))
            , ((controlMask, xF86XK_AudioPlay), spawn (myBrowser ++ " --new-window http://pandora.com"))
            -- ((0, xF86XK_AudioStop), spawn "???")
            -- ((0, xF86XK_AudioPrev), spawn "???")
            -- ((0, xF86XK_AudioNext), spawn "???")

            -- , ((modm, xK_F10), addWorkspace "y")
            -- , (((modm .|. shiftMask), xK_F10), removeEmptyWorkspace)

            -- ThinkPad-specific binding (the black button)
            , ((0, xF86XK_Launch1), spawn myTerminal)
            , ((shiftMask, xF86XK_Launch1), spawn myBrowser)
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
barheight = "16"
screenwidth_cmd = "xrandr | grep '*' | awk '{ print $1 }' | cut -dx -f1"
workspaceStatusBar = "sleep 3s; dzen2 -fn '" ++ font ++ "' -x '0' -y '0' -h '" ++ barheight ++ "' -w '280' -fg '#FFFFFF' -bg '" ++ colorBackground ++ "' -ta l"
conkyStatusBar = "~/.conky/statusbar.py --color-fg '" ++ colorForeground ++ "' > /tmp/xmonad.conkyrc && conky -b -c /tmp/xmonad.conkyrc | dzen2 -y '0' -x '0' -w `" ++ screenwidth_cmd ++ "` -h '" ++ barheight ++ "' -ta 'r' -bg '" ++ colorBackground ++ "' -fg '#FFFFFF' -fn '" ++ font ++ "'"
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
