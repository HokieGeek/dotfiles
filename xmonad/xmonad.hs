-- Imports {{{
import Data.IORef

import Control.Monad(liftM2)

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.ExtraTypes.XorgDefault

import System.IO

import XMonad
import XMonad.Core
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation -- historyHook
import XMonad.Actions.RotSlaves
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Promote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
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
import XMonad.Util.Dmenu
import XMonad.Util.Run
import XMonad.Util.EZConfig

import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- }}}

-- Local Variables {{{

-- unmuteAllChannels = "amixer -q set Master unmute ; amixer -q set Speaker unmute ; amixer -q set Headphone unmute ; "
unmuteAllChannels = "amixer -q set Master unmute ; "
xF86XK_AudioMicMute :: KeySym
xF86XK_AudioMicMute = 0x1008ffb2

modm            = mod4Mask
myTerminal      = "st"
myBrowser       = "google-chrome-stable"
colorForeground = "#005f00"
colorBackground = "#1b1d1e"
font            = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*"

dmenuArgs = ["-fn", font, "-nb", colorBackground, "-nf", "#FFFFFF", "-sb", colorForeground]

myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0","-","="]
myWorkspaceKeys = [xK_1..xK_9] ++ [xK_0,xK_minus,xK_equal]

myDmenuMap = M.fromList
              [ ("browser", myBrowser)
              , ("netflix", myBrowser ++ " --new-window http://netflix.com")
              , ("irssi", myTerminal ++ " -e irssi")
              , ("skype", "apulse32 skype")
              , ("mpc", myTerminal ++ " -e ncmpcpp")
              -- , ("ranger", ("export EDITOR=vim; " ++ myTerminal ++ " -e ranger"))
              , ("steam", "playonlinux --run \"Steam\" %F")
              -- , ("minicom", myTerminal ++ " -e minicom")
              ]
--}}}

-- Local Methods {{{
surroundInQuotes :: String -> String
surroundInQuotes str = "'" ++ str ++ "'"
-- }}}

-- Hooks {{{
-- Manage {{{
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Xmessage"   --> doCenterFloat
    , className =? "Gimp"       --> viewShift "-"
    , className =? "VASSAL-launch-ModuleManager"    --> doFloat <+> doShift "="
    , className =? "VASSAL-launch-Player"           --> doFloat <+> doShift "="
    -- , className =? "st-256color" --> (ask >>= \w -> liftX (setOpacity w 0x11111111) >> idHook)
    , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat -- Hangouts
    ] <+> manageDocks
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        -- doTrans i = ask >>= \w -> liftX (setOpacity w i) >> idHook
    -- , className =? "st-256color"         --> doTrans 0x99999999
-- }}}
-- Log {{{
myLogHook h = (dynamicLogWithPP (myDzen h)) <+> historyHook
                                            >> fadeInactiveLogHook 0.4
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
            , ((modm, xK_a), safeSpawn "dmenu_run" dmenuArgs)
            , ((modm, xK_Return), promote)
            , ((mod1Mask, xK_F4), kill)
            , (((modm .|. controlMask .|. shiftMask), xK_slash), spawn "xmessage -file $HOME/.xmonad/keybindings-help.txt")
            , (((controlMask .|. shiftMask), xK_Escape), spawn (myTerminal ++ " -e htop"))
            , ((0, xF86XK_Sleep), spawn "sudo /usr/sbin/pm-suspend")
            , ((modm, xK_w), spawn "$HOME/.bin/rotate-wallpaper $HOME/.look/bgs")
            , (((modm .|. shiftMask), xK_w), spawn ("$HOME/.bin/schemecolor --colors | dmenu " ++ unwords(map surroundInQuotes dmenuArgs) ++ "| xargs $HOME/.bin/schemecolor"))
            -- Launcher menus
            , ((modm, xK_z), menuMapArgs "dmenu" dmenuArgs myDmenuMap >>= flip whenJust spawn)
            , ((modm, xK_x), goToSelected defaultGSConfig)
            , (((modm .|. shiftMask), xK_x), gotoMenu)
            -- Workspace helpers
            , (((modm .|. mod1Mask), xK_k), prevWS)
            , (((modm .|. mod1Mask), xK_j), nextWS)
            , (((modm .|. shiftMask), xK_k), moveTo Prev NonEmptyWS)
            , (((modm .|. shiftMask), xK_j), moveTo Next NonEmptyWS)
            , ((modm, xK_grave), toggleWS)
            , ((modm, xK_n), moveTo Next EmptyWS)
            , (((modm .|. shiftMask), xK_n), shiftTo Next EmptyWS)
            , (((modm .|. mod1Mask), xK_n), moveTo Next EmptyWS <+> spawn myBrowser)
            , (((modm .|. controlMask), xK_n), moveTo Next EmptyWS <+> spawn myTerminal)
            -- Window helpers
            , (((modm .|. shiftMask), xK_h), sendMessage MirrorShrink) -- shrink the slave area
            , (((modm .|. shiftMask), xK_l), sendMessage MirrorExpand) -- expand the slave area
            , (((modm .|. controlMask), xK_j), rotSlavesDown)
            , (((modm .|. controlMask), xK_k), rotSlavesUp)
            -- Backlight
            , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
            , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
            , ((shiftMask, xF86XK_MonBrightnessUp), spawn "xbacklight -set 100")
            , ((shiftMask, xF86XK_MonBrightnessDown), spawn "xbacklight -set 20")
            -- Volume
            , ((0, xF86XK_AudioRaiseVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 3+"))
            , ((shiftMask, xF86XK_AudioRaiseVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 100"))
            , ((0, xF86XK_AudioLowerVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 3-"))
            , ((shiftMask, xF86XK_AudioLowerVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 30"))
            , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
            , ((shiftMask, xF86XK_AudioMute), spawn (myTerminal ++ " -e alsamixer"))
            , ((0, xF86XK_AudioMicMute), spawn "amixer set Capture toggle")
            -- Media
            , ((shiftMask, xF86XK_AudioPlay), spawn (myBrowser ++ " --new-window https://play.google.com/music/listen#/ap/queue"))
            , ((controlMask, xF86XK_AudioPlay), spawn (myBrowser ++ " --new-window http://pandora.com"))
            -- ((0, xF86XK_AudioStop), spawn "???")
            -- ((0, xF86XK_AudioPrev), spawn "???")
            -- ((0, xF86XK_AudioNext), spawn "???")
            -- Other
            , ((0, xK_Print), spawn "scrot")
            , ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s")
            , (((mod1Mask .|. controlMask), xK_l), spawn "slock")
            -- , ((0, xF86XK_WebCam), spawn "$HOME/.bin/toggle-bluetooth")

            -- ThinkPad-specific binding (the black button)
            , ((0, xF86XK_Launch1), spawn myTerminal)
            , ((shiftMask, xF86XK_Launch1), spawn myBrowser)

            -- , ((modm, xK_F10), addWorkspace "GIMP")
            -- , (((modm .|. shiftMask), xK_F10), removeEmptyWorkspace)
            ]
            ++
            [((m .|. modm, k), windows $ f i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys
                , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
            ++
            [((modm .|. controlMask, k), windows $ swapWithCurrent i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys]
--}}}

-- Main {{{
compmgr   = "xcompmgr"
barheight = "14"
screenwidth_cmd    = "xrandr | grep '*' | awk '{ print $1 }' | cut -dx -f1"
workspaceStatusBar = "sleep 2s; dzen2 -fn '" ++ font ++ "' -x '0' -y '0' -h '" ++ barheight ++ "' -w '280' -fg '#FFFFFF' -bg '" ++ colorBackground ++ "' -ta l"
conkyStatusBar     = "~/.conky/statusbar.py --color-fg '" ++ colorForeground ++ "' > /tmp/xmonad.conkyrc && conky -b -c /tmp/xmonad.conkyrc | dzen2 -y '0' -x '0' -w `" ++ screenwidth_cmd ++ "` -h '" ++ barheight ++ "' -ta 'r' -bg '" ++ colorBackground ++ "' -fg '#FFFFFF' -fn '" ++ font ++ "'"
main = do
        compMgrStart <- spawn compmgr
        dzenRightBar <- spawn conkyStatusBar
        dzenLeftBar  <- spawnPipe workspaceStatusBar
        xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", colorForeground, "-xs", "1"] }
               $ defaultConfig
            { workspaces        = myWorkspaces
            , terminal          = myTerminal
            , modMask           = modm
            , focusFollowsMouse = False
            , borderWidth       = 0
            , manageHook        = myManageHook
            , layoutHook        = myLayoutHook
            , logHook           = myLogHook dzenLeftBar
            , handleEventHook   = myHandleEventHook
            }
            `additionalKeys` myKeys
--}}}

-- vim: set foldmethod=marker number relativenumber:
