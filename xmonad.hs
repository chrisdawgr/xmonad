import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
--import XMonad.Prompt.Shell
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Prompt.XMonad
--import XMonad.Prompt (defaultXPConfig)
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map as M
--import XMonad.Actions.WorkspaceNames
--import Data.Ratio ((%))
import XMonad.Actions.CycleWS

myFont = "xft:Fira Mono For Powerline:size=16"

myXpConfig :: XPConfig
myXpConfig = def {
    borderColor     = "DarkOrange"
    , bgColor       = "black"
    , fgColor       = "#F46D43" --orange
    , bgHLight      = "#42CBF5" --blue
    , fgHLight      = "#f8f8f8"
    , position      = Bottom
    , font          = myFont
    , height        = 24
    , defaultText   = []
}

modM                 = mod4Mask
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
curWSLeft            = "["
curWSRight           = "]"
myVisibleWSLeft      = ""
myVisibleWSRight     = ""
myUrgentWSLeft       = "{"
myUrgentWSRight      = "}"
--myWorkspaces         = ["1:Docs",  "2:Dev", "3:Web", "4:Term"]
myWorkspaces         = ["1:Surf",  "2:Web", "3:Dev", "4:Term","5:Pdf"]
startupWorkspace     = "1:Docs"

defaultLayouts       = smartBorders(avoidStruts(
  ResizableTall 1 (3/100) (1/2) []

  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  ||| noBorders Full

  ||| Grid))

myKeyBindings =
    [ ((modM, xK_b), sendMessage ToggleStruts)
    , ((modM, xK_p), spawn "dmenu_run")
    , ((modM, xK_o), spawn "nautilus")
    , ((modM, xK_c), spawn "google-chrome")
    , ((modM, xK_g), spawn "evince")
    , ((modM, xK_s), spawn "subl")
    , ((modM, xK_t), spawn "texmaker")
    , ((modM, xK_x), shellPrompt defaultXPConfig)
    , ((modM,xK_w), kill)
    , ((modM, xK_Left), prevWS)
    , ((modM, xK_Right), nextWS)
    , ((modM .|. shiftMask, xK_r), renameWorkspace myXpConfig)
    , ((modM .|. shiftMask, xK_s)    , spawn "xfce4-screenshooter -r")
    --, ((modM .|. shiftMask, xK_v)    , removeWorkspace)
    --, ((modM .|. shiftMask, xK_a)    , addWorkspace "tes_workspace")
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
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
  xmonad $ defaultConfig {
    focusedBorderColor = myFocusedBorderColor
  ,logHook             = dynamicLogWithPP $ xmobarPP {
    ppOrder            = \(ws:l:t:_)   -> [ws]
    , ppOutput         = hPutStrLn xmproc
    , ppTitle          = (\str -> "")
    , ppCurrent        = xmobarColor myCurrentWSColor "" . wrap "[" "]"
    , ppVisible        = xmobarColor myVisibleWSColor "" . wrap "" ""
    , ppUrgent         = xmobarColor myUrgentWSColor  "" . wrap "{" "}"
    , ppHidden         = xmobarColor "#5c7c09" ""        . wrap "" ""
    , ppHiddenNoWindows= xmobarColor "white" "" }
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusFollowsMouse  = False
  , terminal           = myTerminal
  , borderWidth        = myBorderWidth
  , layoutHook         = lessBorders OnlyFloat $ avoidStruts $ defaultLayouts
  , modMask            = modM
  , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook

  {-
  , startupHook        = do
      setWMName "LG3D"
      windows $ W.greedyView startupWorkspace
  -}
  , manageHook         = manageHook defaultConfig <+> manageDocks
  }
    `additionalKeys` myKeys