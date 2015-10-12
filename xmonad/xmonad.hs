-- Imports {{{
import Control.Monad(liftM2)

import Data.IORef
import Data.Maybe
import Data.Ratio ((%))

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.ExtraTypes.XorgDefault

import System.IO

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation -- historyHook
-- import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops --fullScreenEventHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows -- fadeWindowEventHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.IM
import XMonad.Layout.MagicFocus
import XMonad.Layout.Minimize
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- }}}

-- Local Variables {{{
modm            = mod4Mask
myTerminal      = "st"
myBrowser       = "google-chrome-unstable"
colorForeground = "#af5f00"
colorBackground = "#1b1d1e"
termFont        = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*"

unmuteAllChannels = "amixer -q set Master unmute ; "

myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0","-","="]
myWorkspaceKeys = [xK_1..xK_9] ++ [xK_0,xK_minus,xK_equal]

dmenuArgs  = ["-fn", termFont, "-nb", colorBackground, "-nf", "#FFFFFF", "-sb", colorForeground]
randomCmdsMenu = M.fromList
              [ ("browser", spawn myBrowser)
              , ("netflix", spawn (myBrowser ++ " --new-window http://netflix.com"))
              , ("irssi", spawn (myTerminal ++ " -e irssi"))
              , ("skype", spawn "apulse32 skype")
              , ("mpc", spawn (myTerminal ++ " -e ncmpcpp"))
              , ("steam", spawn "playonlinux --run \"Steam\" %F")
              , ("ranger", spawn (myTerminal ++ " -e ranger"))
              ]
--}}}

-- Local Methods {{{
xF86XK_AudioMicMute :: KeySym
xF86XK_AudioMicMute = 0x1008ffb2

surroundInQuotes :: String -> String
surroundInQuotes str = "'" ++ str ++ "'"

removeExtraWs :: X() -> X()
removeExtraWs c = removeEmptyWorkspaceAfterExcept myWorkspaces c
-- }}}

-- Hooks {{{
-- Manage {{{
myManageHook = composeAll
    [
      isFullscreen --> doFullFloat
    , className =? "Xmessage"   --> doCenterFloat
    , className =? "Gimp"       --> shiftToNew "gimp"
    , className =? "Skype"      --> shiftToNew "skype"
    , className =? "VASSAL-launch-ModuleManager"        --> doFloat <+> doShift "="
    , className =? "VASSAL-launch-Player"               --> doFloat <+> doShift "="
    , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat
    ] <+> manageDocks
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        shiftToNew ws = liftX (addWorkspace ws) >> viewShift ws
-- }}}
-- Layout{{{
myLayoutHook = avoidStruts
               $ onWorkspace "1" (boringWindows (minimize (reflectHoriz $ withIM (12%1) (ClassName "crx_nckgahadagoaajjgafhacjanaoiihapd") (ResizableTall 1 incDelta (1/6) []) ||| Full)))
               $ onWorkspace "2" (boringWindows (minimize (magicFocus (Mirror (TwoPane incDelta (2/3)) ||| Full))))
               $ onWorkspace "gimp" (boringWindows (minimize ((ResizableTall 2 incDelta (1/6) []) ||| Full)))
               $ onWorkspace "skype" (boringWindows (minimize Full))
               $ toggleLayouts (boringWindows (minimize (twoPanes ||| Mirror twoPanes ||| big ||| Full)))
               $ myDefaultLayout
    where
        incDelta = 3/100
        tiledStd = ResizableTall 1 incDelta (1/2) [] -- # masters, % to inc when resizing, % of screen used by master, slaves
        stacked = StackTile 1 incDelta (1/2)
        twoPanes = TwoPane incDelta (1/2)
        big = OneBig (3/4) (3/4)
        myDefaultLayout = boringWindows (minimize (tiledStd ||| Mirror tiledStd ||| stacked ||| Full))

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
        dzenWorkspaceSymbol x
              | notElem x myWorkspaces = x
              | otherwise = "^r(4x4)^fg(" ++ colorBackground ++ ")^r(5x1)"
-- }}}
-- HandleEvent {{{
myHandleEventHook = fadeWindowsEventHook <+> fullscreenEventHook
-- }}}
-- }}}

-- Keybindings {{{
-- Don't forget to update keybindings-help.txt
myKeys =    [
            -- Misc {{{
              ((modm, xK_q), spawn "~/.xmonad/restart")
            , ((modm, xK_Return), promote)
            , ((mod1Mask, xK_F4), kill)
            , ((0, xF86XK_Sleep), spawn "systemctl suspend")
            , ((shiftMask, xF86XK_Sleep), spawn "systemctl hibernate")
            , (((modm .|. controlMask .|. shiftMask), xK_slash), spawn "xmessage -file $HOME/.xmonad/keybindings-help.txt")
            , (((controlMask .|. shiftMask), xK_Escape), spawn (myTerminal ++ " -e htop"))
            , ((modm, xK_b), spawn "$HOME/.bin/rotate-wallpaper $HOME/.look/bgs")
            , (((modm .|. shiftMask), xK_b), spawn ("$HOME/.bin/schemecolor --colors | dmenu " ++ unwords(map surroundInQuotes dmenuArgs) ++ "| xargs $HOME/.bin/schemecolor"))
            -- }}}
            -- Launcher menus {{{
            , ((modm, xK_a), safeSpawn "dmenu_run" dmenuArgs)
            , ((modm, xK_z), menuMapArgs "dmenu" dmenuArgs randomCmdsMenu >>= fromMaybe (return ()))
            , ((modm, xK_x), goToSelected defaultGSConfig)
            , (((modm .|. shiftMask), xK_x), gotoMenuArgs dmenuArgs)
            -- }}}
            -- Workspace helpers {{{
            , (((modm .|. controlMask), xK_space), sendMessage ToggleLayout)

            , (((modm .|. mod1Mask), xK_k), removeExtraWs prevWS)
            , (((modm .|. mod1Mask), xK_j), removeExtraWs nextWS)
            , (((modm .|. controlMask), xK_k), removeExtraWs (moveTo Prev NonEmptyWS))
            , (((modm .|. controlMask), xK_j), removeExtraWs (moveTo Next NonEmptyWS))
            , ((modm, xK_Tab), removeExtraWs toggleWS)
            , ((modm, xK_n), removeExtraWs (moveTo Next EmptyWS))
            , (((modm .|. shiftMask), xK_n), shiftTo Next EmptyWS)

            , (((modm .|. mod1Mask), xK_n), removeExtraWs (moveTo Next EmptyWS) <+> spawn myBrowser)
            , (((modm .|. controlMask), xK_n), removeExtraWs (moveTo Next EmptyWS) <+> spawn myTerminal)
            -- }}}
            -- Window helpers {{{
            , ((modm, xK_j), focusDown)
            , ((modm, xK_k), focusUp)
            , ((modm, xK_m), focusMaster)
            , (((modm .|. controlMask), xK_apostrophe), withFocused minimizeWindow)
            , (((modm .|. shiftMask), xK_apostrophe), sendMessage RestoreNextMinimizedWin)
            , (((modm .|. shiftMask), xK_h), sendMessage MirrorShrink) -- shrink the slave area
            , (((modm .|. shiftMask), xK_l), sendMessage MirrorExpand) -- expand the slave area
            , (((modm .|. controlMask), xK_j), rotSlavesDown)
            , (((modm .|. controlMask), xK_k), rotSlavesUp)
            , ((modm, xK_v), windows copyToAll)
            , (((modm .|. shiftMask), xK_v), killAllOtherCopies)
            -- }}}
            -- Backlight {{{
            , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
            , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
            , ((shiftMask, xF86XK_MonBrightnessUp), spawn "xbacklight -set 100")
            , ((shiftMask, xF86XK_MonBrightnessDown), spawn "xbacklight -set 20")
            -- Volume
            , ((0, xF86XK_AudioRaiseVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 1+"))
            , ((shiftMask, xF86XK_AudioRaiseVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 100"))
            , ((0, xF86XK_AudioLowerVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 1-"))
            , ((shiftMask, xF86XK_AudioLowerVolume), spawn (unmuteAllChannels ++ " amixer set Master playback 30"))
            , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
            , ((shiftMask, xF86XK_AudioMute), spawn (myTerminal ++ " -e alsamixer"))
            , ((0, xF86XK_AudioMicMute), spawn "amixer set Capture toggle")
            -- Other
            , ((0, xK_Print), spawn "scrot")
            , ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s")
            , (((mod1Mask .|. controlMask), xK_l), spawn "slock")
            , ((0, xF86XK_WebCam), spawn "$HOME/.bin/toggle-bluetooth")

            -- ThinkPad-specific binding (the black button)
            , ((0, xF86XK_Launch1), spawn myTerminal)
            , ((shiftMask, xF86XK_Launch1), spawn myBrowser)
            -- }}}
            ]
            ++
            [((m .|. modm, k), windows $ f i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys
                , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
            -- zip (zip (repeat (modm)) myWorkspaceKeys) (map ((removeEmptyWorkspaceAfterExcept myWorkspaces) . withNthWorkspace W.greedyView) [0..])
            -- ++
            -- zip (zip (repeat (modm .|. shiftMask)) myWorkspaceKeys) (map (withNthWorkspace W.shift) [0..])
            ++
            [((modm .|. controlMask, k), windows $ swapWithCurrent i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys]
            -- ++
            -- [((modm .|. mask, key), f sc)
                -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                -- , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
--}}}

-- Main {{{
compmgr   = "xcompmgr"
barheight = "14"
screenwidth_cmd    = "xrandr | grep '*' | awk '{ print $1 }' | cut -dx -f1"
conkyStatusBar     = "~/.xmonad/statusbar/statusbar.sh --width `" ++ screenwidth_cmd ++ "` --height '" ++ barheight ++ "' --bg '" ++ colorBackground ++ "' --fg '" ++ colorForeground ++ "' --font '" ++ termFont ++ "'"
workspaceStatusBar = "sleep 2s; dzen2 -fn '" ++ termFont ++ "' -x '0' -y '0' -h '" ++ barheight ++ "' -w '280' -fg '#FFFFFF' -bg '" ++ colorBackground ++ "' -ta l"
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
