{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

import           System.IO
import           System.Environment
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers   hiding (CW, CCW)
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Layout.LayoutModifier (ModifiedLayout(..), LayoutModifier(..))
import           XMonad.Layout.Dwindle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig         (additionalKeysP, removeMouseBindings)
import           XMonad.StackSet

import           Data.Monoid
import qualified Data.Set                     as S
import           Data.Word                    (Word32)
import           Data.List                    (isPrefixOf)
import           Data.Bits                    ((.|.))

data DPI = HIGH | LOW

main :: IO ()
main = do
    display <- getEnv "CURRENT_DISPLAY"
    let dpi = case display of
                "high" -> HIGH
                "low"  -> LOW
                _      -> LOW
    bar <- statusBar myBar myPP myToggleStrutsKey (myConfig dpi)
    xmonad bar

myBar :: String
myBar = "sam-bar"

myPP :: PP
myPP = def
    { ppHiddenNoWindows = ("#1" ++) . wrap " " " "
    , ppCurrent = ("#2" ++) . wrap "[" "]"
    , ppHidden = ("#0" ++) . wrap " " " "
    , ppWsSep = ""
    , ppSep = ""
    , ppLayout = \str -> "#1" ++
        if | "XXX"               `isPrefixOf` str -> "XXX"
           | "Spacing Dwindle R" `isPrefixOf` str -> "Dwi"
           | "Spacing Dwindle D" `isPrefixOf` str -> "Dwu"
           | otherwise                            -> take 3 str
    , ppOrder = take 2
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
    , ("M-v",           spawn $ myTerminal ++ " --command ~/.config/vifm/scripts/vifmrun ~ ~/Documents")
    , ("M-m",           spawn $ myTerminal ++ " --command mutt")
    , ("M-l",           sendMessage Shrink)
    , ("M-;",           sendMessage Expand)
    ]

applyMyBindings :: XConfig MyLayout -> XConfig MyLayout
applyMyBindings = appKeys . appMouse
    where
        appKeys = flip additionalKeysP myKeyBindings
        appMouse = flip removeMouseBindings $ map (myModMask, ) [button1, button2, button3]

myConfig :: DPI -> XConfig MyLayout
myConfig dpi = docks $ ewmh $ applyMyBindings
    def
        { terminal           = myTerminal
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , modMask            = myModMask
        , layoutHook         = myLayoutHook dpi
        , borderWidth        = case dpi of
                                 HIGH -> 6
                                 LOW  -> 2
        , manageHook         = myManageHook
        }

myTerminal :: String
myTerminal = "alacritty"

myNormalBorderColor :: String
myNormalBorderColor = "#161821"

myFocusedBorderColor :: String
myFocusedBorderColor = "#6b7089"

myModMask :: KeyMask
myModMask = mod4Mask

myManageHook :: ManageHook
myManageHook = className =? "firefox" --> doSink

type MyModifier a = ModifiedLayout AA (ModifiedLayout SmartBorder (ModifiedLayout Spacing a))
type MyModifier' a = ModifiedLayout AA (ModifiedLayout WithBorder a)
type MyLayout = Choose (MyModifier Dwindle) (Choose (MyModifier Dwindle) (MyModifier' Full))

myLayoutHook :: DPI -> MyLayout Window
myLayoutHook dpi = modify dwindle1 ||| modify dwindle2 ||| modify' Full
    where
        b = case dpi of
              HIGH -> 3
              LOW  -> 1
        spaced   = spacingRaw True screenB False windowB True
        modify   = avoid . smartBorders . spaced
        screenB  = Border 0 0 0 0
        windowB  = Border b b b b
        dwindle1 = Dwindle R CW 1 1.1
        dwindle2 = Dwindle D CCW 1 1.1
        modify'  = avoid . noBorders

newtype AA a = AA { unAA :: AvoidStruts a }
    deriving (Read, Show)

avoid :: LayoutClass l a => l a -> ModifiedLayout AA l a
avoid layout = let ModifiedLayout av l = avoidStruts layout in
                   ModifiedLayout (AA av) l

instance LayoutModifier AA a where
    modifyLayout (AA av) = modifyLayout av
    modifierDescription (AA (AvoidStruts set)) = if S.null set then "XXX" else ""
    pureMess (AA av) m = AA <$> pureMess av m
    hook _ = asks config >>= logHook
