-- Imports {{{
import Control.Monad (liftM2)
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
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
-- }}}

-- TODO: why do these not work?
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]

-- Main {{{
main = do
    xmonad $ defaultConfig
        { workspaces = myWorkspaces
        , modMask = mod4Mask
        , manageHook = myManageHook <+> manageHook defaultConfig -- conky/dzen <+> manageDocks
        , layoutHook = avoidStruts . smartBorders $ layoutHook defaultConfig
        } `additionalKeys`
        [ (((mod4Mask .|. controlMask), xK_Left), prevWS)
        , (((mod4Mask .|. controlMask), xK_Right), nextWS)
        , (((mod4Mask .|. shiftMask), xK_Left), shiftToPrev)
        , (((mod4Mask .|. shiftMask), xK_Right), shiftToNext)
        , (((mod4Mask .|. controlMask .|. shiftMask), xK_Down), moveTo Next EmptyWS)
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask .|. shiftMask, xK_S), spawn "sudo /usr/sbin/pm-suspend")
        , (((mod4Mask .|. controlMask .|. shiftMask), xK_Left), spawn "xbacklight -inc 20")
        , (((mod4Mask .|. controlMask .|. shiftMask), xK_Right), spawn "xbacklight -dec 20")
        ]
--}}}
