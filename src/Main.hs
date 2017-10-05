{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (show', replace, replaceWith, escape, filterOnIndex, versionStr, (>$>))

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as B (ByteString, readFile, writeFile)
import qualified Data.ByteString.Char8 as B8 (lines, unlines)
import Data.FileEmbed (embedFile)
import Data.IOData (IOData)
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Time (getCurrentTime, formatTime)
import Data.Time.Format (defaultTimeLocale)
import Lens.Micro.Platform (view, over)
import Options.Applicative hiding (Mod)
import qualified Options.Applicative (Mod)
import System.Directory (getPermissions, setPermissions, setOwnerExecutable, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeExtension)
import System.IO (hPutStrLn, stderr)
#ifdef mingw32_HOST_OS
import System.Process (callCommand)
#endif

import Ahk (printAhk, toAhk)
import FileType (FileType(..))
import JsonComments (removeJsonComments)
import JsonPretty (Config(..), encodePretty')
import Keylayout (KeylayoutConfig(..), printKeylayout, toKeylayout)
import Klc (KlcConfig(..), printKlcData, toKlcData)
import KlcParse (parseKlcLayout)
import Layout.Layout (isEmptyLayout, layoutOrd, layoutDelims, applyModLayout, removeEmptyLetters, unifyShiftstates)
import Layout.Mod (isEmptyMod, applyInverseMod)
import Layout.Types
import Pkl (printPklData, toPklData, printLayoutData, toLayoutData)
import PklParse (parsePklLayout)
import Stream (Stream(..), toFname, readStream, writeStream, writeFileStream, writeFileStream')
import Tmk (toTmkKeymap, printTmkKeymap)
import Xkb (XkbConfig(..), printSymbols, printTypes, printKeycodes, printXCompose, getFileAndVariant, parseXkbLayoutVariant)

data Options = Options
    { __inputType ∷ Maybe FileType
    , __inputs ∷ [Stream]
    , __outputs ∷ [Output]
    , __extraOptions ∷ [ExtraOption]
    } deriving (Show, Read)

data Output
    = Output FileType Stream
    | OutputAll Stream
    deriving (Show, Read)

data ExtraOption
    = RemoveShiftstates [Int]
    | RemoveEmptyLetters
    | CombineMods
    | UnifyShiftstates

    | KlcChainedDeads

    | PklCompact

    | XkbCustomShortcuts
    | XkbRedirectAll
    | XkbRedirectClearsExtend

    | KeylayoutCustomShortcuts
    deriving (Eq, Show, Read)

execOptions ∷ Options → IO ()
execOptions (Options _ [] _ _) = fail "no input files."
execOptions o@(Options Nothing (Standard:_) _ _) = execOptions o { __inputType = Just Json}
execOptions o@(Options Nothing (File fname:_) _ _) =
  case takeExtension fname of
    ".json"      → execOptions o { __inputType = Just Json }
    ""           → execOptions o { __inputType = Just Xkb }
    ".ini"       → execOptions o { __inputType = Just Pkl }
    ".klc"       → execOptions o { __inputType = Just Klc }
    ".keylayout" → execOptions o { __inputType = Just Keylayout }
    ".c"         → execOptions o { __inputType = Just Tmk }
    ".ahk"       → execOptions o { __inputType = Just Ahk }
    xs → fail ("unknown layout type ‘" ⊕ xs ⊕ "’.")
execOptions o@(Options _ _ [] _) = execOptions o { __outputs = [Output Json Standard] }
execOptions (Options (Just inputType) inputs outputs extraOptions) = printLog $ do
    layout' ← mconcat <$> traverse (input inputType) inputs
    let layout = execExtraOptions extraOptions ∘ layout'
    forM_ outputs (\out → output out extraOptions layout)

input ∷ (Logger m, MonadIO m) ⇒ FileType → Stream → m (FileType → Layout)
input Json = parseWith (\fname → bimap ((fname ⊕ ": ") ⊕) pure ∘ eitherDecode ∘ removeJsonComments)
input Xkb =
    getFileAndVariant ∘ toFname >>> (\(fname, variant) →
    parseWith (parseXkbLayoutVariant variant) (File fname)) >$> const
input Pkl = parseWith parsePklLayout >$> const
input Klc = parseWith parseKlcLayout >$> const
input Keylayout = fail "importing from a keylayout file is not supported"
input Tmk = fail "importing from a TMK file is not supported"
input Ahk = fail "importing from a AHK file is not supported"

parseWith ∷ (Logger m, MonadIO m) ⇒ IOData α ⇒ (String → α → Either String (m β)) →
    Stream → m β
parseWith parser stream = flip ($) stream $
    readStream >=>
    parser (toFname stream) >>>
    either (fail ∘ ("parse fail in " ⊕) ∘ dropWhileEnd (≡'\n')) id

output ∷ (Logger m, MonadIO m) ⇒ Output → [ExtraOption] → (FileType → Layout) → m ()
output (OutputAll Standard) _ = const (fail "everything as output must be written to a directory")
output (OutputAll (File dir)) extraOptions = \layout → do
    let name = view (_info ∘ _name) ∘ layout
    let output' t fname
          | isEmptyLayout (layout t) = tell ["ignoring empty layout for " ⊕ show' t]
          | otherwise = output (Output t (File fname)) extraOptions layout
    let fnameJson
          | null (name Json) = fail "the layout has an empty name when exported to JSON"
          | otherwise = name Json
    output' Json (dir </> fnameJson <.> "json")
    output' Xkb (dir </> "xkb")
    output' Pkl (dir </> "pkl")
    output' Klc (dir </> "klc")
    output' Keylayout (dir </> "keylayout")
    output' Tmk (dir </> "tmk")
    output' Ahk (dir </> "ahk")
output (Output Json stream) _ = ($ Json) >>>
    writeStream stream ∘ encodePretty' (Config 4 layoutOrd layoutDelims)
output (Output Xkb Standard) _ = const (fail "XKB as output must be written to a directory")
output (Output Xkb (File dir)) extraOptions = ($ Xkb) >>> \layout → do
    let name = replaceWith (not ∘ isAlphaNum) '_' $ view (_info ∘ _name) layout
    let description = replace '\n' ' ' $ fromMaybe (view (_info ∘ _fullName) layout) (view (_info ∘ _description) layout)
    let mods = (\(Mod modName _) → replaceWith (not ∘ isAlphaNum) '_' modName) <$> view _mods layout
    when (null name) (fail "the layout has an empty name when exported to XKB")
    let xkbConfig = liftA3 XkbConfig (XkbCustomShortcuts ∈) (XkbRedirectAll ∈) (XkbRedirectClearsExtend ∈) extraOptions
    writeFileStream (dir </> "symbols" </> name) =<< runReaderT (printSymbols layout) xkbConfig
    writeFileStream' (dir </> "types" </> name) =<< runReaderT (printTypes layout) xkbConfig
    writeFileStream' (dir </> "keycodes" </> name) =<< printKeycodes layout
    writeFileStream' (dir </> "XCompose") (printXCompose layout)
    let replaceLayout = replaceVar "layout" name
    let replaceDescription = replaceVar "description" description
    let replaceMods = replaceVar "mods" (intercalate " " mods)
    sessionFile   ← liftIO $ B.readFile "xkb/run-session.sh" <|> pure defXkbSession
    systemFile    ← liftIO $ B.readFile "xkb/install-system.sh" <|> pure defXkbSystem
    usystemFile   ← liftIO $ B.readFile "xkb/uninstall-system.sh" <|> pure defXkbUSystem
    xcomposeFile  ← liftIO $ B.readFile "xkb/scripts/install-xcompose.sh" <|> pure defXkbXCompose
    uxcomposeFile ← liftIO $ B.readFile "xkb/scripts/uninstall-xcompose.sh" <|> pure defXkbUXCompose
    xmlFile       ← liftIO $ B.readFile "xkb/scripts/add-layout-to-xml.py" <|> pure defXkbXml
    removeXmlFile ← liftIO $ B.readFile "xkb/scripts/remove-layout-from-xml.py" <|> pure defXkbRemoveXml
    functionsFile ← liftIO $ B.readFile "xkb/scripts/functions.sh" <|> pure defXkbFunctions
    liftIO $ B.writeFile (dir </> "run-session.sh") (replaceLayout sessionFile)
    liftIO $ B.writeFile (dir </> "install-system.sh") ((replaceMods ∘ replaceDescription ∘ replaceLayout) systemFile)
    liftIO $ B.writeFile (dir </> "uninstall-system.sh") ((replaceMods ∘ replaceDescription ∘ replaceLayout) usystemFile)
    liftIO $ createDirectoryIfMissing True (dir </> "scripts")
    liftIO $ B.writeFile (dir </> "scripts/install-xcompose.sh") (replaceLayout xcomposeFile)
    liftIO $ B.writeFile (dir </> "scripts/uninstall-xcompose.sh") (replaceLayout uxcomposeFile)
    liftIO $ B.writeFile (dir </> "scripts/add-layout-to-xml.py") xmlFile
    liftIO $ B.writeFile (dir </> "scripts/remove-layout-from-xml.py") removeXmlFile
    liftIO $ B.writeFile (dir </> "scripts/functions.sh") functionsFile
    liftIO $ makeExecutable (dir </> "run-session.sh")
    liftIO $ makeExecutable (dir </> "install-system.sh")
    liftIO $ makeExecutable (dir </> "uninstall-system.sh")
    liftIO $ makeExecutable (dir </> "scripts/install-xcompose.sh")
    liftIO $ makeExecutable (dir </> "scripts/uninstall-xcompose.sh")
    liftIO $ makeExecutable (dir </> "scripts/add-layout-to-xml.py")
    liftIO $ makeExecutable (dir </> "scripts/remove-layout-from-xml.py")
    liftIO $ makeExecutable (dir </> "scripts/functions.sh")
output (Output Pkl Standard) _ = const (fail "PKL as output must be written to a directory")
output (Output Pkl (File dir)) extraOptions = ($ Pkl) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (fail "the layout has an empty name when exported to PKL")
    let isCompact = PklCompact ∈ extraOptions
    today ← liftIO $ formatTime defaultTimeLocale "%F %T" <$> getCurrentTime
    let printedPklData l = printPklData isCompact <$> toPklData l l
    let printedLayoutData l = printLayoutData today <$> toLayoutData l
    let layoutFile modName l
          | isCompact = dir </> ("layout" ⊕ modName) <.> "ini"
          | otherwise = dir </> "layouts" </> view (_info ∘ _name) l </> "layout" <.> "ini"
    forM_ ((∅) : view _mods layout) $ \layoutMod@(Mod nameM _) → do
        let nameM' = bool ('_':nameM) "" (isEmptyMod layoutMod)
        let layout' = applyModLayout layoutMod layout
        writeFileStream (dir </> ("pkl" ⊕ nameM') <.> "ini") =<< printedPklData layout'
        writeFileStream (layoutFile nameM' layout') =<< printedLayoutData layout'
    pklFile ← liftIO $ B.readFile "pkl/pkl.exe" <|> pure defPklFile
    liftIO $ B.writeFile (dir </> "pkl.exe") pklFile
output (Output Klc Standard) extraOptions = ($ Klc) >>>
    let klcConfig = KlcConfig (KlcChainedDeads ∈ extraOptions)
    in  writeStream Standard ∘ printKlcData <=< flip runReaderT klcConfig ∘ toKlcData
output (Output Klc (File dir)) extraOptions = ($ Klc) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (fail "the layout has an empty name when exported to KLC")
    let klcConfig = KlcConfig (KlcChainedDeads ∈ extraOptions)
    let fname l = dir </> view (_info ∘ _name) l <.> "klc"
    forM_ ((∅) : view _mods layout) $ \layoutMod → do
        let layout' = applyModLayout layoutMod layout
        writeFileStream (fname layout') ∘ printKlcData =<< runReaderT (toKlcData layout') klcConfig
output (Output Keylayout Standard) extraOptions = ($ Keylayout) >>>
    toKeylayout (KeylayoutConfig (KeylayoutCustomShortcuts ∈ extraOptions)) >=>
    writeStream Standard ∘ printKeylayout
output (Output Keylayout (File dir)) extraOptions = ($ Keylayout) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (fail "the layout has an empty name when exported to keylayout")
    let keylayoutConfig = KeylayoutConfig (KeylayoutCustomShortcuts ∈ extraOptions)
    let fname l = dir </> view (_info ∘ _name) l <.> "keylayout"
    forM_ ((∅) : view _mods layout) $ \layoutMod → do
        let layout' = applyModLayout layoutMod layout
        writeFileStream (fname layout') ∘ printKeylayout =<< toKeylayout keylayoutConfig layout'
    let replaceLayout = replaceVar "layout" name
    userFile   ← liftIO $ B.readFile "keylayout/install-user.sh"   <|> pure defKeylayoutUser
    systemFile ← liftIO $ B.readFile "keylayout/install-system.sh" <|> pure defKeylayoutSystem
    liftIO $ B.writeFile (dir </> "install-user.sh")   (replaceLayout userFile)
    liftIO $ B.writeFile (dir </> "install-system.sh") (replaceLayout systemFile)
    liftIO $ makeExecutable (dir </> "install-user.sh")
    liftIO $ makeExecutable (dir </> "install-system.sh")
output (Output Tmk Standard) _ = const (fail "TMK as output must be written to a directory")
output (Output Tmk (File dir)) _ = ($ Tmk) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (error "the layout has an empty name when exported to TMK")
    writeStream (File $ dir </> "unimap.c") ∘ printTmkKeymap =<< toTmkKeymap layout
output (Output Ahk Standard) _ = ($ Ahk) >>>
    writeStream Standard ∘ printAhk <=< toAhk id
output (Output Ahk (File dir)) _ = ($ Ahk) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (fail "the layout has an empty name when exported to AHK")
    let fname l = dir </> view (_info ∘ _name) l <.> "ahk"
    forM_ ((∅) : view _mods layout) $ \layoutMod → do
        let layout' = applyModLayout layoutMod layout
        let getOrigPos = applyInverseMod layoutMod
        writeFileStream (fname layout') ∘ printAhk =<< toAhk getOrigPos layout'

replaceVar ∷ B.ByteString → String → B.ByteString → B.ByteString
replaceVar var val = B8.unlines ∘ replace before after ∘ B8.lines
  where
    before = var ⊕ "=\"\""
    after  = var ⊕ "=" ⊕ T.encodeUtf8 (T.pack (escape val))

makeExecutable ∷ FilePath → IO ()
makeExecutable fname =
    getPermissions fname >>=
    setPermissions fname ∘ setOwnerExecutable True

defPklFile,
    defXkbSession, defXkbSystem, defXkbUSystem, defXkbXCompose, defXkbUXCompose, defXkbXml, defXkbRemoveXml, defXkbFunctions,
    defKeylayoutUser, defKeylayoutSystem ∷ B.ByteString
defPklFile = $(embedFile "files/pkl/pkl.exe")
defXkbSession   = $(embedFile "files/xkb/run-session.sh")
defXkbSystem    = $(embedFile "files/xkb/install-system.sh")
defXkbUSystem   = $(embedFile "files/xkb/uninstall-system.sh")
defXkbXCompose  = $(embedFile "files/xkb/scripts/install-xcompose.sh")
defXkbUXCompose = $(embedFile "files/xkb/scripts/uninstall-xcompose.sh")
defXkbXml       = $(embedFile "files/xkb/scripts/add-layout-to-xml.py")
defXkbRemoveXml = $(embedFile "files/xkb/scripts/remove-layout-from-xml.py")
defXkbFunctions = $(embedFile "files/xkb/scripts/functions.sh")
defKeylayoutUser   = $(embedFile "files/keylayout/install-user.sh")
defKeylayoutSystem = $(embedFile "files/keylayout/install-system.sh")

execExtraOptions ∷ [ExtraOption] → Layout → Layout
execExtraOptions = flip (foldr execExtraOption)

execExtraOption ∷ ExtraOption → Layout → Layout
execExtraOption (RemoveShiftstates cols) =
    over (_keys ∘ traverse)
        (over _letters (filterOnIndex (∉ cols)) ∘
         over _shiftlevels (filterOnIndex (∉ cols)))
execExtraOption RemoveEmptyLetters = over (_keys ∘ traverse) removeEmptyLetters
execExtraOption CombineMods =
    over _mods (map mconcat ∘ tail ∘ subsequences)
execExtraOption UnifyShiftstates = over _keys (fst ∘ unifyShiftstates)
execExtraOption _ = id

printLog ∷ WriterT [String] IO () → IO ()
printLog =
    execWriterT >=>
    traverse_ (\xs → hPutStrLn stderr $ "klfc: warning: " ⊕ xs ⊕ ".") ∘ nub

main ∷ IO ()
main =
#ifdef mingw32_HOST_OS
    callCommand "chcp 65001" >>
#endif
    (execParser opts >>= execOptions) `catch` handler
  where
    opts = info (helper <*> versionOption <*> options)
      ( fullDesc
      ⊕ header "Keyboard Layout Files Creator - export a keyboard layout to different formats"
      )
    versionOption = infoOption versionStr (long "version" ⊕ hidden ⊕ help "Show version")
    handler e
      | isUserError e = putStrLn ("klfc: " ⊕ ioeGetErrorString e)
      | otherwise = ioError e

options ∷ Parser Options
options = Options
    <$> (usageText "IMPORT TYPE" *> headerStr "\n\b\bImport types:" *> optional parseInputType)
    <*> (headerStr "\n\b\bImport files:" *> some parseInput)
    <*> (usageText "OUTPUTS" *> headerStr "\n\b\bOutput files:" *> many parseOutput)
    <*> (usageText "OPTIONS" *> headerStr "\n\b\bExtra Options:" *> (many ∘ asum)
            [ parseExtraOption
            , headerStr "\b\bKLC:" *> parseKlcOption
            , headerStr "\b\bPKL:" *> parsePklOption
            , headerStr "\b\bXKB:" *> parseXkbOption
            , headerStr "\b\bKeylayout:" *> parseKeylayoutOption
            ]
        )

parseInputType ∷ Parser FileType
parseInputType = asum
    [ flag' Json (long "from-json" ⊕ hidden ⊕ help "Read from a JSON file")
    , flag' Xkb (long "from-xkb" ⊕ hidden ⊕ help "Import from a XKB symbols file. To read a variant, append it in parenthesis (e.g. to read the Colemak variant of the us symbols file, use \"us(colemak)\").")
    , flag' Pkl (long "from-pkl" ⊕ hidden ⊕ help "Import from a PKL layout file")
    , flag' Klc (long "from-klc" ⊕ hidden ⊕ help "Import from a KLC file")
    ]

parseInput ∷ Parser Stream
parseInput =
    argument (parseStream <$> str) (metavar "FILE..." ⊕ help "Files to read (‘-’ for stdin). If multiple files are read, the corresponding layouts will be put together. This is useful when a file only specifies a part of the layout (e.g. only the letters at a few shiftstates).")

parseOutput ∷ Parser Output
parseOutput = asum
    [ Output Json <$> streamOption (long "json" ⊕ metavar "FILE" ⊕ hidden ⊕ help "Save to a JSON file (‘-’ for stdout)")
    , Output Xkb <$> streamOption (long "xkb" ⊕ metavar "DIRECTORY" ⊕ hidden ⊕ help "Export to a XKB directory")
    , Output Pkl <$> streamOption (long "pkl" ⊕ metavar "DIRECTORY" ⊕ hidden ⊕ help "Export to a PKL directory")
    , Output Klc <$> streamOption (long "klc" ⊕ metavar "DIRECTORY" ⊕ hidden ⊕ help "Export to a KLC directory (‘-’ for printing the base layout to stdout)")
    , Output Keylayout <$> streamOption (long "keylayout" ⊕ metavar "DIRECTORY" ⊕ hidden ⊕ help "Export to a keylayout directory (‘-’ for printing the base layout to stdout)")
    , Output Tmk <$> streamOption (long "tmk" ⊕ metavar "DIRECTORY" ⊕ hidden ⊕ help "Export to a TMK directory (‘-’ for printing the base layout to stdout)")
    , Output Ahk <$> streamOption (long "ahk" ⊕ metavar "DIRECTORY" ⊕ hidden ⊕ help "Export to a AHK directory (‘-’ for printing the base layout to stdout)")
    , OutputAll <$> streamOption (long "output" ⊕ short 'o' ⊕ metavar "DIRECTORY" ⊕ hidden ⊕ help "Export to all file types")
    ]

parseExtraOption ∷ Parser ExtraOption
parseExtraOption = asum
    [ RemoveShiftstates <$> option (str >>= parseList) (long "remove-shiftstates" ⊕ metavar "INDEX" ⊕ hidden ⊕ help "Remove one or more shiftstates with their letters. The shiftstates are identified with their index (starting with 0). Multiple indices are seperated with a comma.")
    , flag' RemoveEmptyLetters (long "remove-empty-letters" ⊕ hidden ⊕ help "Remove empty letters at the end of each key")
    , flag' CombineMods (long "combine-mods" ⊕ hidden ⊕ help "Combine all the mods in the layout. For example, if the layout has the mods ‘Wide’ and ‘Angle’, a new mod ‘WideAngle’ will be created.")
    , flag' UnifyShiftstates (long "unify-shiftstates" ⊕ hidden ⊕ help "Change the shiftstates of all keys such that all keys have the same shiftstates")
    ]

parseKlcOption ∷ Parser ExtraOption
parseKlcOption =
    flag' KlcChainedDeads (long "klc-chained-deads" ⊕ hidden ⊕ help "Use chained dead keys in KLC. This requires alternative compilation, see <http://archives.miloush.net/michkap/archive/2011/04/16/10154700.html>.")

parsePklOption ∷ Parser ExtraOption
parsePklOption =
    flag' PklCompact (long "pkl-compact" ⊕ hidden ⊕ help "Set PKL to compact mode")

parseXkbOption ∷ Parser ExtraOption
parseXkbOption = asum
    [ flag' XkbCustomShortcuts (long "xkb-custom-shortcuts" ⊕ hidden ⊕ help "Use the shortcut positions from the ‘shortcutPos’ attributes for shortcuts in XKB")
    , flag' XkbCustomShortcuts (long "xkb-custom-shortcut-positions" ⊕ hidden)
    , flag' XkbRedirectAll (long "xkb-redirect-all" ⊕ hidden ⊕ help "Always use the ‘redirect’ action in XKB, if possible. This may help some programs detect special actions on different layers.")
    , flag' XkbRedirectClearsExtend (long "xkb-redirect-clears-extend" ⊕ hidden ⊕ help "Clear the extend modifier (LevelFive) in redirect actions. This may help some programs detect special actions on the extend layer.")
    , flag' XkbRedirectClearsExtend (long "xkb-redirect-if-extend" ⊕ hidden)
    ]

parseKeylayoutOption ∷ Parser ExtraOption
parseKeylayoutOption =
    flag' KeylayoutCustomShortcuts (long "keylayout-custom-shortcuts" ⊕ hidden ⊕ help "Use the shortcut positions from the ‘shortcutPos’ attributes for shortcuts in keylayout")

usageText ∷ String → Parser ()
usageText s = option disabled (value () ⊕ metavar s)

headerStr ∷ String → Parser ()
headerStr s = option disabled (value () ⊕ metavar s ⊕ hidden ⊕ helpDoc (pure (∅)))

streamOption ∷ Options.Applicative.Mod OptionFields Stream → Parser Stream
streamOption = option (parseStream <$> str)

parseList ∷ (Monad m, Read α) ⇒ String → m [α]
parseList s = either fail pure $ readEither ("[" ⊕ s ⊕ "]")

parseStream ∷ String → Stream
parseStream "-" = Standard
parseStream s   = File s
