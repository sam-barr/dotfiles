{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

import           System.Environment
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers   hiding (CW)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Layout.LayoutModifier (ModifiedLayout(..), LayoutModifier(..))
import           XMonad.Layout.Dwindle
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig         (additionalKeysP, removeMouseBindings)
import           XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as Stack
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Man
import           XMonad.Prompt.Unicode


import qualified Data.Set                     as S
import           Data.List                    (isPrefixOf, isInfixOf)
import           Data.Char

data DPI = HIGH | LOW

main :: IO ()
main = do
    displayType <- getEnv "CURRENT_DISPLAY"
    let dpi = case displayType of
                "high" -> HIGH
                "low"  -> LOW
                _      -> LOW
    bar <- statusBar myBar myPP myToggleStrutsKey (myConfig dpi)
    xmonad bar

myBar :: String
myBar = "sam-bar"

myPP :: PP
myPP = namedScratchpadFilterOutWorkspacePP $ def
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
    , ("M-l",           sendMessage Shrink)
    , ("M-;",           sendMessage Expand)
    , ("M-d",           gotoDiscord)
    , ("M-p M-k",       namedScratchpadAction scratchpads "kalk")
    , ("M-p M-g",       namedScratchpadAction scratchpads "ghci")
    ] ++ [("M-p M-" ++ k, p myXPConfig) | (k, p) <- promptList]

gotoDiscord :: X ()
gotoDiscord = flip raiseMaybe (className =? "discord") $ do
    windows $ Stack.greedyView "D"
    spawn "discord"


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
        , workspaces         = myWorkspaces
        }

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "D"]

myTerminal :: String
myTerminal = "alacritty"

myNormalBorderColor :: String
myNormalBorderColor = "#161821"

myFocusedBorderColor :: String
myFocusedBorderColor = "#6b7089"

myModMask :: KeyMask
myModMask = mod4Mask

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "firefox" --> doSink
    , className =? "discord" --> doShift "D"
    ] <+> namedScratchpadManageHook scratchpads

type MyModifier a = ModifiedLayout SmartBorder (ModifiedLayout Spacing a)
type MyModifier' a = ModifiedLayout WithBorder a
type MyLayout = ModifiedLayout AA (Choose (MyModifier Dwindle) (Choose (MyModifier Dwindle) (MyModifier' Full)))

myLayoutHook :: DPI -> MyLayout Window
myLayoutHook dpi = avoid (modifier dwindle1 ||| modifier dwindle2 ||| modifier' Full)
    where
        b = case dpi of
              HIGH -> 3
              LOW  -> 1
        spaced    = spacingRaw True screenB False windowB True
        modifier  = smartBorders . spaced
        screenB   = Border 0 0 0 0
        windowB   = Border b b b b
        dwindle1  = Dwindle R CW 1 1.1
        dwindle2  = Dwindle D CCW 1 1.1
        modifier' = noBorders

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

promptList :: [(String, XPConfig -> X ())]
promptList =
    [ ("p", shellPrompt)
    , ("m", manPrompt)
    , ("u", myUnicodePrompt)
    ]

myXPConfig :: XPConfig
myXPConfig = def
    { font = "xft:Hasklug Nerd Font:dpi=336:size=8:style=bold"
    , bgColor = "#0F1117"
    , fgColor = "#D2D4DE"
    , bgHLight = "#6B7089"
    , fgHLight = "#0F1117"
    , borderColor = "#D4D2DE"
    , height = 100
    , position = Top
    , alwaysHighlight = True
    , maxComplRows = Just 1
    , historySize = 0
    }

myUnicodePrompt :: XPConfig -> X ()
myUnicodePrompt config = typeUnicodePrompt unicodeData $ config
    { searchPredicate = isInfixOf
    , sorter = lower
    , maxComplRows = Just 1
    }
    where
        unicodeData = "/usr/share/unicode/UnicodeData.txt"
        lower = const $ map (map toLower)

scratchpads :: NamedScratchpads
scratchpads = map makeNS [ "kalk", "ghci" ]
    where
        makeNS p = NS p (makeCmd p) (title =? p) scratchpadHook
        makeCmd p = unwords
            [ myTerminal
            , "--title", p
            , "--config-file ~/.config/scratchpad.yml"
            , "--command", p
            ]
        scratchpadHook = customFloating $ Stack.RationalRect 0 0 1 (1/15)
