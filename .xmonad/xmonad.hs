{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE LambdaCase            #-}

import           System.Environment
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers   hiding (CW)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Layout.LayoutModifier (ModifiedLayout(..), LayoutModifier(..))
import           XMonad.Layout.NoBorders
import           XMonad.Util.EZConfig         (additionalKeysP, removeMouseBindings)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.PureX as PX
import qualified XMonad.StackSet as Stack
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Man

import qualified Data.Set                     as S
import           Data.List                    (isInfixOf)
import           Data.Char
import           Data.Monoid                  (Any(..))

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
myPP = filterOutWsPP ["NSP"] $ def
    { ppHiddenNoWindows = ("#1" ++) . wrap " " " "
    , ppCurrent = ("#2" ++) . wrap "[" "]"
    , ppHidden = ("#0" ++) . wrap " " " "
    , ppWsSep = ""
    , ppSep = ""
    , ppLayout = \str -> "#1" ++ (take 3 str)
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
    , ("M-S-v",         spawn "screen-recorder-manager")
    , ("M-h",           spawn "headphones")
    , ("M-S-h",         spawn "headset")
    , ("M-v",           spawn $ myTerminal ++ " --command vifm ~ ~/Documents")
    , ("M-d",           myMoveWorkspace "D")
    , ("M-[",           pipFocused)
    , ("M-]",           unpip)
    , ("M-S-[",         pipRotate)
    , ("M-S-]",         pipToggleHide)
    , ("M-p M-k",       namedScratchpadAction scratchpads "kalk")
    , ("M-p M-g",       namedScratchpadAction scratchpads "ghci")
    ] ++ [("M-p M-" ++ k, p myXPConfig) | (k, p) <- promptList]
      ++ [("M-" ++ i, myMoveWorkspace i) | i <- map show [(1::Int)..9]]

pipFocused :: X ()
pipFocused = withFocused $ \w -> do
    pip <- XS.get
    case pip of
        NPiP -> do
            windows $ Stack.float w $ piplRR BR
            XS.put $ PiP w BR False
        PiP _ _ _ -> return ()

unpip :: X ()
unpip = do
    pip <- XS.get
    case pip of
        NPiP -> return ()
        PiP w _ _ -> do
            windows $ Stack.sink w
            XS.put NPiP

pipRotate :: X ()
pipRotate = XS.get >>= \case
    NPiP -> return ()
    PiP w pipl True -> XS.put $ PiP w (piplNext pipl) True
    PiP w pipl False -> do
        let pipl' = piplNext pipl
        windows $ Stack.float w $ piplRR pipl'
        XS.put $ PiP w pipl' False

-- TODO: firefox doesn't like this implementation
pipToggleHide :: X ()
pipToggleHide = XS.get >>= \case
    NPiP -> return ()
    PiP w pipl False -> do
        windows $ Stack.float w $ Stack.RationalRect 1 1 0.4 0.4
        XS.put $ PiP w pipl True
    PiP w pipl True -> do
        windows $ Stack.float w $ piplRR pipl
        XS.put $ PiP w pipl False

-- purex stuff is to limit the number of refreshes here
myMoveWorkspace :: String -> X ()
myMoveWorkspace workspace = PX.defile $ do
    pip <- XS.get
    b1 <- case pip of
        NPiP -> return $ Any False
        PiP w _ _ -> shiftWinPureX workspace w
    b2 <- PX.greedyView workspace
    return $ b1 <> b2

shiftWinPureX :: PX.XLike m => String -> Window -> m Any
shiftWinPureX tag w = do
    mtag <- gets $ Stack.findTag w . windowset
    PX.whenJust' mtag $ \wtag ->
        PX.when' (tag /= wtag) $ do
            PX.modifyWindowSet' $ Stack.shiftWin tag w
            ntag <- gets $ Stack.findTag w . windowset
            return $ Any $ mtag /= ntag

data PiPLocation = TL | TR | BL | BR deriving (Read, Show)

piplNext :: PiPLocation -> PiPLocation
piplNext TL = TR
piplNext TR = BR
piplNext BL = TL
piplNext BR = BL

piplRR :: PiPLocation -> Stack.RationalRect
piplRR TL = Stack.RationalRect 0   0   0.4 0.4
piplRR TR = Stack.RationalRect 0.6 0   0.4 0.4
piplRR BL = Stack.RationalRect 0   0.6 0.4 0.4
piplRR BR = Stack.RationalRect 0.6 0.6 0.4 0.4

data PictureInPicture = PiP Window PiPLocation Bool | NPiP deriving (Read, Show)

instance ExtensionClass PictureInPicture where
    initialValue = NPiP
    extensionType = PersistentExtension

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
        , layoutHook         = myLayoutHook
        , borderWidth        = case dpi of
                                 HIGH -> 6
                                 LOW  -> 2
        , manageHook         = myManageHook
        , workspaces         = myWorkspaces
        }

myWorkspaces :: [String]
myWorkspaces = map show [(1::Int)..9] ++ ["D"]

myTerminal :: String
myTerminal = "alacritty"

myNormalBorderColor :: String
myNormalBorderColor = "#161821"

myFocusedBorderColor :: String
myFocusedBorderColor = "#6B7089"

myModMask :: KeyMask
myModMask = mod4Mask

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "firefox" <&&> (not <$> pip) --> doSink
    , pip --> doRectFloat (Stack.RationalRect 0 0 0.4 0.4)
    , className =? "obs" --> doSink
    , className =? "discord" --> doShift "D"
    ] <+> namedScratchpadManageHook scratchpads
    where pip = stringProperty "WM_ICON_NAME" =? "Picture-in-Picture"

--- Layouts ---

type Tall1 = ModifiedLayout SmartBorder Tall
type Tall2 = ModifiedLayout SmartBorder (Mirror Tall)
type Full' = ModifiedLayout WithBorder Full
type MyLayout = ModifiedLayout AA (Choose Tall1 (Choose Tall2 Full'))

myLayoutHook :: MyLayout Window
myLayoutHook = avoid (tall ||| mtall ||| noBorders Full)
    where
        tall = smartBorders $ Tall nmaster delta ratio
        mtall = smartBorders $ Mirror $ Tall nmaster delta ratio
        nmaster = 1
        delta = 3 / 100
        ratio = 1 / 2

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

--- Prompts ---

promptList :: [(String, XPConfig -> X ())]
promptList =
    [ ("p", shellPrompt)
    , ("m", manPrompt)
    , ("o", paperPrompt)
    ]

myXPConfig :: XPConfig
myXPConfig = def
    { font = "xft:Source Code Pro:dpi=336:size=8:style=bold"
    , bgColor = "#161821"
    , fgColor = "#6B7089"
    , bgHLight = "#6B7089"
    , fgHLight = "#161821"
    , borderColor = "#6B7089"
    , promptBorderWidth = 6
    , height = 100
    , position = Top
    , alwaysHighlight = True
    , maxComplRows = Just 1
    , historySize = 0
    }

-- Paper Prompt --

data Paper = Paper

instance XPrompt Paper where
    showXPrompt Paper = "Paper: "
    commandToComplete Paper s = s
    nextCompletion Paper = getNextCompletion

paperPath :: FilePath
paperPath = "/home/sam-barr/Documents/Papers"

findPapers :: String -> String -> X String
findPapers pattern format = runProcessWithInput "find"
    [ paperPath
    , "-name", pattern
    , "-printf", format
    ] ""

paperPrompt :: XPConfig -> X ()
paperPrompt conf = do
        papers <- myPapers
        mkXPrompt Paper conf' (paperCompl papers) openPaper
    where
        conf' = conf {maxComplRows=Nothing}
        paperCompl _ "" = return []
        paperCompl papers search = return $ searchPapers papers search 
        openPaper paper = do
            filePath <- findPapers paper "%p"
            safeSpawn "zathura" [filePath]

myPapers :: X [String]
myPapers = lines <$> findPapers "*.pdf" "%f\n"

searchPapers :: [String] -> String -> [String]
searchPapers papers search = filter go papers
    where
        searchWords = words $ map toLower search
        go pName = all (`isInfixOf` map toLower pName) searchWords

--- Named Scratchpads ---

scratchpads :: NamedScratchpads
scratchpads = map makeNS [
        ("kalk", ["-i", "/home/sam-barr/.config/init.kalk"]),
        ("ghci", [])
    ]
    where
        makeNS a@(p, _) = NS p (makeCmd a) (title =? p) scratchpadHook
        makeCmd (p, args) = unwords $ [ myTerminal , "-t", p , "-e", p ] ++ args
        scratchpadHook = customFloating $ Stack.RationalRect (1/4) (1/4) (1/2) (1/2)
