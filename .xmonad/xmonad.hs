-- Imports {{{
import Control.Monad (liftM2)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import qualified XMonad.StackSet as W
--}}}

myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]
myManageHook = composeAll
    [ className =? "Gimp"                           --> doFloat
    , className =? "VASSAL-launch-ModuleManager"    --> doFloat
    , className =? "VASSAL-launch-Player"           --> doFloat
    , className =? "VASSAL-launch-ModuleManager"    --> viewShift "9"
    , className =? "VASSAL-launch-Player"           --> viewShift "9"
    , isFullscreen --> doFullFloat
    ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

-- Main {{{
main = do
    xmonad $ defaultConfig
        { workspaces = myWorkspaces
        , manageHook = myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        } 
-- `additionalKeys`
    -- [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    -- , ((0, xK_Print), spawn "scrot")
        -- , ((modMask .|. controlMask, xK_Left), prevWS)
        -- , ((modMask .|. controlMask .|. shiftMask), xK_Right), moveTo Next EmptyWS)
        -- , ((modMask .|. controlMask, xK_Right), nextWS)
        ]
--}}}
