-- Imports {{{
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
colorForeground="#9F0AC4"
colorBackground="#1B1D1E"
font = "-*-terminus-bold-r-*-*-12-*-*-*-*-*-*-*"

myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]
myWorkspaceKeys = [xK_1..xK_9] ++ [xK_0,xK_minus,xK_equal]

myAppGSMenu = [ ("Chromium", "chromium")
              , ("Terminal", myTerminal)
              , ("VLC", "vlc")
              , ("Skype", "skype")
              , ("gVim", "gvim")
              , ("Eclipse", "eclipse")
              , ("MPC", myTerminal ++ " -e ncmpcpp")
              , ("Ranger", rangerExec)
              , ("Gimp", "gimp")
              , ("Irssi", myTerminal ++ " -e irssi")
              , ("Alsa Mixer", myTerminal ++ " -e alsamixer")
              , ("VirtualBox", "virtualbox")
              , ("Steam", "/usr/share/playonlinux/playonlinux --run \"Steam\" %F")
              , ("Deluge", "deluge")
              -- , ("minicom", myTerminal ++ " -e minicom") -- specific menu for the two configs
              ]
--}}}

-- Local Methods {{{
-- http://ixti.net/software/2013/09/07/xmonad-action-gridselect-spawnselected-with-nice-titles.html
mySpawnSelected :: [(String, String)] -> X()
mySpawnSelected lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig

focusTest :: X()
-- focusTest = spawn "xmessage 'testing'"
focusTest = windows W.focusMaster
    -- where curr = windows W.index !! 0
-- focusTest = spawn "xmessage 'test'"
-- focusTest = spawn "xmessage '" ++ (windows W.peek) ++ "'"
-- focusTest windows W.peek == windows W.index !! 0 = spawn "xmessage 'testing'"
-- focusTest Eq windows W.peek windows W.index !! 0 = spawn "xmessage 'testing'"
            -- nextMatch History (return True)
-- focusTest = spawn msg
--     where msg = "xmessage '" ++ myTerminal ++ "'"
    -- where currentWin = W.index W.current
-- W.current.stack
            -- , ((modm, xK_m), windows W.focusMaster)
            -- , (((modm .|. shiftMask), xK_m), nextMatch History (return True))
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
    ] <+> manageDocks
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        -- vassalClassnames = ["VASSAL-launch-ModuleManager", "VASSAL-launch-Player"]
        -- floatingInOne = ["crx_nckgahadagoaajjgafhacjanaoiihapd",  -- Hangouts
                         -- "crx_hmjkmjkepdijhoojdojkdfohbdgmmhki"] -- Google Keep
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
        -- dzenWorkspaceSymbol x
            -- | useWorkspaceName = x
            -- | otherwise = "^i(/home/andres/.xmonad/imgs/workspace.xbm)"
        dzenWorkspaceSymbol x = "^i(/home/andres/.xmonad/imgs/workspace.xbm)"
        -- dzenWorkspaceSymbol x = "^i(\\\\$HOME/.xmonad/imgs/workspace.xbm)"
-- useWorkspaceName = False
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
myKeys =
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
            [ ((modm, xK_q), spawn "~/.xmonad/restart")
            , ((modm, xK_a), spawn "dmenu_run")
            -- , ((modm, xK_e), spawn rangerExec)
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
            , (((modm .|. mod1Mask), xK_l), moveTo Next NonEmptyWS)
            , (((modm .|. mod1Mask), xK_h), moveTo Prev NonEmptyWS)
            , ((modm, xK_n), moveTo Next EmptyWS)
            , ((modm, xK_BackSpace), toggleWS)
            -- Window helpers
            , (((modm .|. shiftMask), xK_BackSpace), nextMatch History (return True))
            , (((modm .|. mod1Mask), xK_space), windows W.swapMaster)
            , (((modm .|. shiftMask), xK_h), sendMessage MirrorShrink) -- shrink the focused window
            , (((modm .|. shiftMask), xK_l), sendMessage MirrorExpand) -- expand the focused window
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
            -- ((0, xF86XK_AudioPlay), spawn "mcpd ???")
            -- ((0, xF86XK_AudioStop), spawn "mcpd ???")
            -- ((0, xF86XK_AudioPrev), spawn "mcpd ???")
            -- ((0, xF86XK_AudioNext), spawn "mcpd ???")

            , ((0, xK_F10), addWorkspace "y")
            , ((shiftMask, xK_F10), removeEmptyWorkspace)
            -- , ((0, xK_F3), useWorkspaceName = True)
            -- , ((0, xK_F4), useWorkspaceName = False)
            ]
            -- (keysym 0xff67, Menu)
            -- ((0, xF86XK_Display), spawn "???")
            -- ++
            -- zip (zip (repeat (modm)) myWorkspaceKeys) (map (withNthWorkspace W.greedyView) [0..])
                -- | (i, k) <- zip myWorkspaces myWorkspaceKeys
                -- | (i, k) <- zip (XMonad.workspaces conf) myWorkspaceKeys
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
workspaceStatusBar = "sleep 2s; dzen2 -fn '" ++ font ++ "' -x '1440' -y '0' -h '16' -w '280' -fg '#FFFFFF' -bg '" ++ colorBackground ++ "' -ta l"
conkyStatusBar = "~/.conky/statusbar.py --color-fg '" ++ colorForeground ++ "' > /tmp/xmonad.conkyrc && conky -b -c /tmp/xmonad.conkyrc | dzen2 -y '0' -x '2732' -w '1366' -h '16' -ta 'r' -bg '" ++ colorBackground ++ "' -fg '#FFFFFF' -fn '" ++ font ++ "'"
main = do
        compMgrStart <- spawn compmgr
        dzenLeftBar  <- spawnPipe workspaceStatusBar
        dzenRightBar <- spawnPipe conkyStatusBar
        xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "#9F0AC4", "-xs", "1"] }
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
            -- , keys = myKeys
            }
            `additionalKeys` myKeys
--}}}

-- vim: set foldmethod=marker number relativenumber:
