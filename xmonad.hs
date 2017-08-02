import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import XMonad.Actions.CycleWS

modM            = mod4Mask
myFocusedBorderColor = "#ff0000"
myNormalBorderColor  = "#cccccc"
myBorderWidth        = 1
myTerminal           = "terminator"
myIMRosterTitle      = "Buddy List"
myTitleColor         = "#eeeeee"
myTitleLength        = 80
myCurrentWSColor     = "#e6744c"
myVisibleWSColor     = "#c185a7"
myUrgentWSColor      = "#cc0000"
myCurrentWSLeft      = "["
myCurrentWSRight     = "]"
myVisibleWSLeft      = "("
myVisibleWSRight     = ")"
myUrgentWSLeft       = "{"
myUrgentWSRight      = "}"

myWorkspaces         = ["1:Docs",  "2:Dev", "3:Web", "4:Term"]
startupWorkspace     = "1:Docs"

defaultLayouts       = smartBorders(avoidStruts(
  ResizableTall 1 (3/100) (1/2) []

  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  ||| noBorders Full

  ||| Grid))

myKeyBindings =
    [ ((modM, xK_b), sendMessage ToggleStruts)
    , ((modM, xK_a), sendMessage MirrorShrink)
    , ((modM, xK_z), sendMessage MirrorExpand)
    , ((modM, xK_p), spawn "dmenu_run")
    , ((modM, xK_q), spawn "sudo xmonad --recompile; xmonad --restart")
    , ((modM, xK_o), spawn "nautilus")
    , ((modM, xK_c), spawn "google-chrome")
    , ((modM, xK_g), spawn "evince")
    , ((modM, xK_s), spawn "subl")
    , ((modM, xK_t), spawn "texmaker")
    , ((modM, xK_m), spawn "matlab")
    , ((modM, xK_u), focusUrgent)
    , ((modM,xK_w), kill)
    , ((modM, xK_Left), prevWS)
    , ((modM, xK_Right), nextWS)
    , ((modM .|. shiftMask, xK_s)   , spawn "xfce4-screenshooter -r")
    , ((modM .|. shiftMask, xK_l)    , spawn "gnome-screensaver-command -l")
    , ((modM .|. shiftMask, xK_space), spawn "terminator")
    , ((modM .|. shiftMask, xK_Right), shiftToNext >> nextWS)
    , ((modM .|. shiftMask, xK_Left) , shiftToPrev >> prevWS)
    , ((0, 0x1008FF12), spawn "amixer -q -D pulse sset Master toggle")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 1%-")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 1%+") ]

myManagementHooks :: [ManageHook]
myManagementHooks = [
  resource =? "synapse" --> doIgnore
  , resource =? "stalonetray" --> doIgnore
  , className =? "rdesktop" --> doFloat
  , (className =? "Komodo IDE") --> doF (W.shift "5:Dev")
  , (className =? "Komodo IDE" <&&> resource =? "Komodo_find2") --> doFloat
  , (className =? "Komodo IDE" <&&> resource =? "Komodo_gotofile") --> doFloat
  , (className =? "Komodo IDE" <&&> resource =? "Toplevel") --> doFloat
  , (className =? "Empathy") --> doF (W.shift "7:Chat")
  , (className =? "Pidgin") --> doF (W.shift "7:Chat")
  , (className =? "Gimp-2.8") --> doF (W.shift "9:Pix")
  ]

numPadKeys =
  [
    xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
    , xK_KP_Left, xK_KP_Begin,xK_KP_Right
    , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
    , xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
  ]

myKeys = myKeyBindings ++
  [
    ((m .|. modM, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numPadKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    focusedBorderColor = myFocusedBorderColor
  , normalBorderColor  = myNormalBorderColor
  , focusFollowsMouse  = False
  , terminal           = myTerminal
  , borderWidth        = myBorderWidth
  , layoutHook         = lessBorders OnlyFloat $ avoidStruts $ defaultLayouts
  , workspaces         = myWorkspaces
  , modMask            = modM
  , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook
  , startupHook        = do
      setWMName "LG3D"
      windows $ W.greedyView startupWorkspace
  , manageHook         = manageHook defaultConfig <+> manageDocks
  , logHook            = takeTopFocus <+> dynamicLogWithPP xmobarPP {
    ppOrder            = \(ws:l:t:_)   -> [ws]
    , ppOutput         = hPutStrLn xmproc
    , ppTitle          = (\str -> "")
    , ppCurrent        = xmobarColor myCurrentWSColor ""
      . wrap myCurrentWSLeft myCurrentWSRight
    , ppVisible        = xmobarColor myVisibleWSColor ""
      . wrap myVisibleWSLeft myVisibleWSRight
    , ppUrgent         = xmobarColor myUrgentWSColor ""
      . wrap myUrgentWSLeft myUrgentWSRight
    , ppHidden         = xmobarColor "#5c7c09" ""
      . wrap myVisibleWSLeft myVisibleWSRight
    , ppHiddenNoWindows= xmobarColor "white" ""
    }
  }
    `additionalKeys` myKeys