-- Imports {{{
import Control.Monad(liftM2)

import Data.Default
import Data.IORef
import Data.Maybe
import Data.Ratio ((%))

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.ExtraTypes.XorgDefault

import System.Process
import System.IO

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.GroupNavigation -- historyHook
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Actions.WithAll
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops --fullScreenEventHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows -- fadeWindowEventHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ToggleHook
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
import XMonad.Prompt
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- }}}

-- Local Functions {{{
modm                = mod4Mask
colorForeground     = "#af5f00"
colorBackground     = "#1b1d1e"
colorWhite          = "#cdcdcd"
termFont            = "-*-terminus-*-*-*-*-10-*-*-*-*-*-*-*"
statusbarHeight     = 15
workspaceBarWidth   = 310

myTerminal        = "st"
-- myTerminal = runProcessWithInput "get-terminal" [] []
myBrowser  = runProcessWithInput "get-browser" [] []

pulseAudioCtl = "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 "

myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0"]
myWorkspaceKeys = [xK_1..xK_9] ++ [xK_0]

dmenuArgs  = ["-nb", colorBackground, "-nf", colorWhite, "-sb", colorForeground]
dzenArgs  = ["-bg", colorBackground, "-fg", colorWhite, "-fn", termFont]

myXPConfig = defaultXPConfig {
                  font = termFont
                , bgColor = colorBackground
                , fgColor = colorWhite
                , bgHLight = colorForeground
                , fgHLight = colorWhite
                , borderColor = colorBackground
                , position = Top
                , height = statusbarHeight
                , showCompletionOnTab = True
             }

-- xF86XK_AudioMicMute :: KeySym
-- xF86XK_AudioMicMute = 0x1008ffb2

surroundInQuotes :: String -> String
surroundInQuotes str = "'" ++ str ++ "'"

removeExtraWs :: X() -> X()
removeExtraWs c = removeEmptyWorkspaceAfterExcept myWorkspaces c
-- }}}

-- Hooks {{{
-- Manage {{{
myManageHook = insertPosition Master Newer <+> composeAll
    [
      isFullscreen --> doFullFloat
    , className =? "Xmessage" --> doCenterFloat
    , className =? "Gimp"     --> shiftToNew "gimp"
    , className =? "Skype"    --> shiftToNew "skype"
    , className =? "VASSAL-launch-ModuleManager"        --> doFloat <+> doShift "="
    , className =? "VASSAL-launch-Player"               --> doFloat <+> doShift "="
    , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat
    ] <+> manageDocks <+> toggleHook "float" doFloat
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        shiftToNew ws = liftX (addWorkspace ws) >> viewShift ws
-- }}}
-- Layout{{{
myLayoutHook = avoidStruts
               $ onWorkspace "gimp" (boringWindows (minimize ((ResizableTall 2 incDelta (1/6) []) ||| Full)))
               $ onWorkspace "skype" (boringWindows (minimize Full))
               $ toggleLayouts (boringWindows (minimize (Mirror tiledStd ||| stacked ||| Mirror twoPanes ||| Full))) -- alternate
               $ boringWindows (minimize (tiledStd ||| twoPanes ||| big ||| Full)) -- default
    where
        incDelta = 3/100
        tiledStd = ResizableTall 1 incDelta (1/2) [] -- # masters, % to inc when resizing, % of screen used by master, slaves
        stacked = StackTile 1 incDelta (1/2)
        twoPanes = TwoPane incDelta (1/2)
        big = OneBig (3/4) (3/4)

-- }}}
-- Log {{{
myLogHook h = (dynamicLogWithPP (myDzen h)) <+> historyHook
                                            >> fadeInactiveCurrentWSLogHook 0.4
                                            >> updatePointer (1, 1) (0, 0)
                                            >> setWMName "LG3D"

myDzen h = def
    {
        ppCurrent         = dzenColor colorForeground colorBackground . pad . dzenWorkspaceSymbol
      , ppVisible         = dzenColor "#cdcdcd" colorBackground . pad . dzenWorkspaceSymbol
      , ppHidden          = dzenColor "#474747" colorBackground . pad . dzenWorkspaceSymbol
      , ppHiddenNoWindows = dzenColor "#282828" colorBackground . pad . dzenWorkspaceSymbol
      , ppUrgent          = dzenColor "#ff0000" colorBackground . pad . dzenAlertWorkspaceSymbol
      , ppLayout          = (\x -> "")
      , ppTitle           = (\x -> "")
      , ppSep             = ""
      , ppWsSep           = ""
      , ppOutput          = hPutStrLn h
    }
    where
        dzenWorkspaceSymbol x
              | notElem x myWorkspaces = x
              | otherwise = "^r(4x4)^fg(" ++ colorBackground ++ ")^r(5x1)"
        dzenAlertWorkspaceSymbol x
              | otherwise = "^r(2x2)^fg(" ++ colorBackground ++ ")^r(5x1)"
-- }}}
-- HandleEvent {{{
myHandleEventHook = fadeWindowsEventHook <+> fullscreenEventHook
-- }}}
-- Startup {{{

myStartupHook = do
    spawn "xcompmgr"
    spawn conkyStatusBar
    where
        screenwidth_cmd = "$(expr $(xrandr | awk -Fx '/\\*/ { print $1; exit }') - " ++ show workspaceBarWidth ++ ")"
        conkyStatusBar = "~/.xmonad/statusbar/statusbar.sh --width " ++ screenwidth_cmd ++ " --height '" ++ show statusbarHeight ++ "' --bg '" ++ colorBackground ++ "' --fg '" ++ colorForeground ++ "' --font '" ++ termFont ++ "' --xpos '" ++ show workspaceBarWidth ++ "'"
-- }}}
-- }}}

-- Keybindings {{{
genericKeys =    [
            -- General {{{
              ((modm, xK_q), spawn "~/.xmonad/restart") --        Restart xmonad
            -- %HELP% mod-Shift-q      Quit xmonad

            , ((modm, xK_b), spawn "rotate-wallpaper $HOME/.look/bgs") --        Cycle wallpapers
            , (((modm .|. shiftMask), xK_b), unsafeSpawn ("schemecolor --colors | dmenu " ++ unwords(map surroundInQuotes dmenuArgs) ++ "| xargs schemecolor")) --  Select a new color scheme

            -- , ((0, xF86XK_Sleep), spawn "systemctl suspend") -- %SKIPHELP%
            -- , ((shiftMask, xF86XK_Sleep), spawn "systemctl hibernate") -- %SKIPHELP%
            , (((modm .|. shiftMask), xK_slash), spawn ("$HOME/.xmonad/display-keybindings.sh " ++ show statusbarHeight ++ " 1600 62 " ++ unwords(map surroundInQuotes dzenArgs))) -- %SKIPHELP%
                                                                        -- %SKIPHELP%
            , ((0, xF86XK_AudioRaiseVolume), spawn (pulseAudioCtl ++ " +2%")) -- %SKIPHELP%
            , ((controlMask, xF86XK_AudioRaiseVolume), spawn (pulseAudioCtl ++ " +10%")) --  Raise volume at 10% steps
            , ((shiftMask, xF86XK_AudioRaiseVolume), spawn (pulseAudioCtl ++ " 100%")) -- Raise volume to 100%
            , ((0, xF86XK_AudioLowerVolume), spawn (pulseAudioCtl ++ " -2%")) -- %SKIPHELP%
            , ((controlMask, xF86XK_AudioLowerVolume), spawn (pulseAudioCtl ++ " -10%")) --  Lower volume at 10% steps
            , ((shiftMask, xF86XK_AudioLowerVolume), spawn (pulseAudioCtl ++ " -30%")) -- Lower volume to 30%
            , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle") -- %SKIPHELP%
            , ((0, xF86XK_AudioMicMute), spawn "amixer set Capture toggle") -- %SKIPHELP%

            -- }}}
            -- Launchers {{{
            -- %HELP% mod-Shift-Enter  Launch terminal
            , ((shiftMask, xF86XK_AudioMute), spawn (myTerminal ++ " -e pulsemixer")) --   Launch Alsa mixer

            -- }}}
            -- Window helpers {{{
            , ((modm, xK_m), focusMaster) -- Switch to master window but ignores minimized

            -- %HELP% mod-Shift-[1..9,0]    Move window to indicated workspace
            , (((modm .|. shiftMask), xK_n), shiftTo Next EmptyWS) --         Move window to next empty workspace

            , ((modm, xK_Return), promote) --  Swap the focused window and the master window

            -- , (((modm .|. controlMask), xK_apostrophe), withFocused minimizeWindow) --  Minimize current window
            -- , (((modm .|. shiftMask), xK_apostrophe), sendMessage RestoreNextMinimizedWin) -- Restore a minimized window

            , ((modm, xK_v), windows copyToAll) --       Make active window sticky across all desktops
            , (((modm .|. shiftMask), xK_v), killAllOtherCopies) -- Remove sticky from active window

            -- %HELP% mod-Shift-c     Close/kill the focused window

            -- }}}
            -- Workspace helpers {{{
            -- %HELP% mod-[1..9,0]       Switch to indicated workspace
            -- %HELP% mod-Ctrl-[1..9,0]  Swap current workspace with indicated workspace

            -- , (((modm .|. mod1Mask), xK_k), removeExtraWs prevWS) --   Previous workspace
            -- , (((modm .|. mod1Mask), xK_j), removeExtraWs nextWS) --   Next workspace
            , ((modm, xK_Tab), removeExtraWs toggleWS) --     Switch to last selected workspace

            , ((modm, xK_u), removeExtraWs (moveTo Next EmptyWS)) --       Switch to next empty workspace
            , (((modm .|. mod1Mask), xK_u), removeExtraWs (moveTo Next EmptyWS) <+> spawn myTerminal) --   Switch to next empty workspace and launch terminal
            , (((modm .|. controlMask), xK_u), do { removeExtraWs (moveTo Next EmptyWS); liftIO myBrowser >>= spawn }) --  Switch to next empty workspace and launch browser

            -- %HELP% mod-space            Rotate through the available layout algorithms
            -- %HELP% mod-Shift-space      Reset the layouts on the current workSpace to default
            , (((modm .|. controlMask), xK_space), sendMessage ToggleLayout) --   Toggle alternate layouts
            -- }}}
            -- Screen helpers {{{
            -- %HELP% mod-{w,e,r}          Switch to physical/Xinerama screens 1, 2, or 3
            -- %HELP% mod-Shift-{w,e,r}    Move client to screen 1, 2, or 3

            , (((modm .|. controlMask), xK_w), swapNextScreen) -- Swap current screen with next screen

            -- }}}
            -- Floating window helpers {{{
            , ((modm, xK_g), withFocused $ windows . W.sink) -- Push window back into tiling
            , (((modm .|. shiftMask), xK_g), withAll' W.sink) -- Sink all floating windows on the current workspace
            , (((modm .|. controlMask), xK_g), toggleHookAllNew "float" >> runLogHook) --  Toggle floating all new windows

            -- , ((modm, xK_y), withFocused (keysMoveWindow (-10,0))) --       Move floating window left
            -- , (((modm .|. controlMask), xK_y), withFocused (keysResizeWindow (10,0) (1,1))) --  Expand left edge of floating window
            -- , (((modm .|. shiftMask), xK_y), withFocused (keysResizeWindow (-10,0) (1,1))) -- Shrink right edge of floating window
            -- , ((modm, xK_u), withFocused (keysMoveWindow (0,10))) --       Move floating window down
            -- , (((modm .|. controlMask), xK_u), withFocused (keysResizeWindow (10,10) (1,1))) --  Expand top-left corner of floating window
            -- , ((modm, xK_i), withFocused (keysMoveWindow (0,-10))) --       Move floating window up
            -- , (((modm .|. controlMask), xK_i), withFocused (keysResizeWindow (0,10) (1,1))) --  Expand upper edge of floating window
            -- , (((modm .|. shiftMask), xK_i), withFocused (keysResizeWindow (0,-10) (1,1))) -- Shrink upper edge of floating window
            -- , ((modm, xK_o), withFocused (keysMoveWindow (10,0))) --       Move floating window right
            -- , (((modm .|. controlMask), xK_o), withFocused (keysResizeWindow (-10,-10) (1,1))) --  Shrink top-left corner of floating window

            -- %HELP% mod-button1     Set the window to floating mode and move by dragging
            -- %HELP% mod-button2     Raise the window to the top of the stack
            -- %HELP% mod-button3     Set the window to floating mode and resize by dragging

            -- }}}
            -- Other {{{ -- %SKIPHELP%
            , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5") -- %SKIPHELP%
            , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5") -- %SKIPHELP%
            -- , ((shiftMask, xF86XK_MonBrightnessUp), spawn "xbacklight -set 100") -- %SKIPHELP%
            -- , ((shiftMask, xF86XK_MonBrightnessDown), spawn "xbacklight -set 20") -- %SKIPHELP%
            , ((0, xK_Find), spawn "xbacklight -inc 5") -- %SKIPHELP%
            , ((0, xK_Help), spawn "xbacklight -dec 5") -- %SKIPHELP%
            -- , ((shiftMask, xK_Find), spawn "xbacklight -set 100") -- %SKIPHELP%
            -- , ((shiftMask, xK_Help), spawn "xbacklight -set 20") -- %SKIPHELP%

            , ((0, xK_Print), spawn "scrot") -- %SKIPHELP%
            , ((mod1Mask, xK_Print), spawn "scrot -d 1 -s") -- %SKIPHELP%

            , (((mod1Mask .|. controlMask), xK_l), spawn "slock") -- %SKIPHELP%

            , ((0, xK_Cancel), spawn "sctw -250") -- %SKIPHELP%
            , ((controlMask, xK_Cancel), spawn "sctw -100") -- %SKIPHELP%
            , ((0, xK_Undo), spawn "sctw +250") -- %SKIPHELP%
            , ((controlMask, xK_Undo), spawn "sctw +100") -- %SKIPHELP%
                                                                  -- %SKIPHELP%
            --- ThinkPad-specific -- %SKIPHELP%
            -- , ((0, xF86XK_WebCam), spawn "toggle-bluetooth") -- %SKIPHELP%
            -- , ((0, xF86XK_Display), spawn "toggle-screens") -- %SKIPHELP%
            -- }}}
            ]
            ++
            [((m .|. modm, k), windows $ f i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys
                , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]] -- %SKIPHELP%

qwertyKeys =    [
            -- Launchers {{{
              ((modm, xK_f), safeSpawn "dmenu_run_mfu" (dmenuArgs ++ ["--threshold", "2", "--terminal", myTerminal])) --        Launch dmenu command launcher
            , (((modm .|. controlMask), xK_f), gotoMenuArgs (dmenuArgs ++ ["-l", "25"])) --        Launch dmenu of open windows
            -- }}}

            -- Window helpers {{{
            , ((modm, xK_j), focusDown) -- Switch to next window in layout order but ignoring minimized
            , ((modm, xK_k), focusUp) -- Switch to previous window in layout order but ignoring minimized

            -- %HELP% mod-Shift-j     Swap the focused window with the next window
            -- %HELP% mod-Shift-k     Swap the focused window with the previous window
            , (((modm .|. controlMask), xK_j), rotSlavesDown) --  Move current slave window down
            , (((modm .|. controlMask), xK_k), rotSlavesUp) --  Move current slave window up
            -- }}}
            -- Workspace helpers {{{
            , (((modm .|. mod1Mask), xK_k), removeExtraWs prevWS) --   Previous workspace
            , (((modm .|. mod1Mask), xK_j), removeExtraWs nextWS) --   Next workspace

            , (((modm .|. shiftMask .|. controlMask), xK_k), removeExtraWs (moveTo Prev NonEmptyWS)) -- Switch to previous non-empty workspace
            , (((modm .|. shiftMask .|. controlMask), xK_j), removeExtraWs (moveTo Next NonEmptyWS)) -- Switch to next non-empty workspace

            -- %HELP% mod-h           Shrink the master area
            -- %HELP% mod-l           Expand the master area
            , (((modm .|. shiftMask), xK_h), sendMessage MirrorShrink) -- Shrink the slave area
            , (((modm .|. shiftMask), xK_l), sendMessage MirrorExpand) -- Expand the slave area
            -- }}}
            -- Screen helpers {{{
            -- %HELP% mod-{w,e,r}          Switch to physical/Xinerama screens 1, 2, or 3
            -- %HELP% mod-Shift-{w,e,r}    Move client to screen 1, 2, or 3

            , (((modm .|. mod1Mask), xK_h), prevScreen) --  Previous screen
            , (((modm .|. mod1Mask), xK_l), nextScreen) --  Next screen
            -- }}}
            ]

colemakKeys =    [
            -- Launchers {{{
              ((modm, xK_t), safeSpawn "dmenu_run_mfu" (dmenuArgs ++ ["--threshold", "2", "--terminal", myTerminal])) --        Launch dmenu command launcher
            , (((modm .|. controlMask), xK_t), gotoMenuArgs (dmenuArgs ++ ["-l", "25"])) --        Launch dmenu of open windows
            -- }}}

            -- Window helpers {{{
            , ((modm, xK_n), focusDown) -- Switch to next window in layout order but ignoring minimized
            , ((modm, xK_e), focusUp) -- Switch to previous window in layout order but ignoring minimized

            , (((modm .|. shiftMask), xK_n), shiftTo Next EmptyWS) --         Move window to next empty workspace

            , (((modm .|. controlMask), xK_n), rotSlavesDown) --  Move current slave window down
            , (((modm .|. controlMask), xK_e), rotSlavesUp) --  Move current slave window up

            -- }}}
            -- Workspace helpers {{{
            , (((modm .|. mod1Mask), xK_e), removeExtraWs prevWS) --   Previous workspace
            , (((modm .|. mod1Mask), xK_n), removeExtraWs nextWS) --   Next workspace

            , (((modm .|. shiftMask .|. controlMask), xK_e), removeExtraWs (moveTo Prev NonEmptyWS)) -- Switch to previous non-empty workspace
            , (((modm .|. shiftMask .|. controlMask), xK_n), removeExtraWs (moveTo Next NonEmptyWS)) -- Switch to next non-empty workspace

            -- %HELP% mod-h           Shrink the master area
            , ((modm, xK_i), sendMessage Expand) -- Expand the master area
            , (((modm .|. shiftMask), xK_h), sendMessage MirrorShrink) -- Shrink the slave area
            , (((modm .|. shiftMask), xK_i), sendMessage MirrorExpand) -- Expand the slave area
            -- }}}
            ]
            ++
            [((modm .|. controlMask, k), windows $ swapWithCurrent i)
                | (i, k) <- zip myWorkspaces myWorkspaceKeys]
            ++
            [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
                | (key, sc) <- zip [xK_w, xK_f, xK_p] [0..]
                , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
--}}}
-- myKeys = genericKeys ++ qwertyKeys
myKeys = genericKeys ++ colemakKeys

-- Main {{{
workspaceStatusBar = "dzen2 -dock -fn '" ++ termFont ++ "' -x '0' -y '0' -h '" ++ show statusbarHeight ++ "' -w '" ++ show workspaceBarWidth ++ "' -fg '" ++ colorWhite ++ "' -bg '" ++ colorBackground ++ "' -ta l"
main = do
        dzenLeftBar  <- spawnPipe workspaceStatusBar
        xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", colorForeground, "-xs", "1"] }
               $ docks def
            { workspaces        = myWorkspaces
            , terminal          = myTerminal
            , modMask           = modm
            , focusFollowsMouse = False
            , borderWidth       = 0
            , manageHook        = myManageHook
            , layoutHook        = myLayoutHook
            , logHook           = myLogHook dzenLeftBar
            , handleEventHook   = myHandleEventHook
            , startupHook       = myStartupHook
            }
            `additionalKeys` myKeys
--}}}

-- vim: set foldmethod=marker number relativenumber:
