{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

import           System.IO
import           System.Environment -- idea: envirnoment variable for display
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.EwmhDesktops -- steam
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.NoBorders
import           XMonad.Util.EZConfig         (additionalKeysP, removeMouseBindings)
import           XMonad.StackSet

import           Data.Monoid                  (All)
import           Data.Word                    (Word32)

main :: IO ()
main = xmonad =<< statusBar myBar myPP myToggleStrutsKey myConfig

myBar :: String
myBar = "xmobar ~/.xmonad/.xmobarrc"

myPP :: PP
myPP = xmobarPP
    { ppHiddenNoWindows = xmobarColor "grey" ""
    , ppOrder =
        \case
                x:y:_ -> [x, y]
                _ -> []
    }

myToggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
myToggleStrutsKey XConfig { modMask = mask } = (mask, xK_t)

myKeyBindings :: [(String, X ())]
myKeyBindings =
    [ ("M-<Backspace>", spawn "pamixer -t")
    , ("M-=",           spawn "pamixer -i 2")
    , ("M--",           spawn "pamixer -d 2")
    , ("M-S-=",         spawn "light -A 5")
    , ("M-S--",         spawn "light -U 5")
    , ("M-f",           spawn "firefox")
    , ("M-i",           spawn "firefox --private-window")
    , ("M-S-l",         spawn "physlock -d")
    , ("M-S-f",         spawn "scrot ~/Pictures/screenshots/%Y-%m-%d-%T.png")
    , ("M-h",           spawn "headphones")
    , ("M-S-h",         spawn "headset")
    , ("M-z",           spawn "zoom")
    , ("M-v",           spawn $ myTerminal ++ " --command ~/.config/vifm/scripts/vifmrun ~ ~/Documents")
    ]

myConfig :: XConfig MyLayout
myConfig = docks $ ewmh $
    def
        { terminal           = myTerminal
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , modMask            = myModMask
        , layoutHook         = myLayoutHook
        }
        `additionalKeysP` myKeyBindings `removeMouseBindings` map (myModMask, ) [button1, button2, button3]

myTerminal :: String
myTerminal = "alacritty"

myBorderWidth :: Word32
myBorderWidth = 2
--myBorderWidth = 6

myNormalBorderColor :: String
myNormalBorderColor = "#000000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#3d3d5c"
--myFocusedBorderColor = "#4D4E4F"

myModMask :: KeyMask
myModMask = mod4Mask

type MyLayout = Choose Tall (Choose (Mirror Tall) (ModifiedLayout WithBorder Full))

myLayoutHook :: MyLayout Window
myLayoutHook = tiled ||| Mirror tiled ||| noBorders Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myStartupHook :: X ()
myStartupHook = do
    width <- (rect_width . screenRect . screenDetail . current) <$> (gets windowset)
    undefined

-- #define XF86XK_AudioLowerVolume	0x1008FF11   /* Volume control down        */
-- #define XF86XK_AudioMute	0x1008FF12   /* Mute sound from the system */
-- #define XF86XK_AudioRaiseVolume	0x1008FF13   /* Volume control up          */
-- #define XF86XK_MonBrightnessUp   0x1008FF02  /* Monitor/panel brightness */
-- #define XF86XK_MonBrightnessDown 0x1008FF03  /* Monitor/panel brightness */
