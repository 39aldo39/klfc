{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (show', replace, filterOnIndex, (>$>))

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (execWriterT, mapWriterT, tell)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as B (ByteString, readFile, writeFile)
import qualified Data.ByteString.Char8 as B8 (lines, unlines)
import Data.FileEmbed (embedFile)
import Data.Functor.Identity (runIdentity)
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Time (getCurrentTime, formatTime)
import Data.Time.Format (defaultTimeLocale)
import Lens.Micro.Platform (view, over)
import Options.Applicative hiding (Mod)
import qualified Options.Applicative (Mod)
import System.Directory (getPermissions, setPermissions, setOwnerExecutable)
import System.FilePath ((</>), (<.>), takeExtension)
import System.IO (hPutStrLn, stderr)

import FileType (FileType(..))
import JsonComments (removeJsonComments)
import JsonPretty (Config(Config), encodePretty')
import Keylayout (printKeylayout, toKeylayout)
import Klc (printKlcData, toKlcData)
import KlcParse (parseKlcLayout)
import Layout.Layout (isEmptyLayout, layoutOrd, layoutDelims, applyModLayout, removeEmptyLetters, unifyShiftstates)
import Layout.Mod (isEmptyMod)
import Layout.Types
import Pkl (printPklData, toPklData, printLayoutData, toLayoutData)
import PklParse (parsePklLayout)
import Stream (Stream(..), toFname, ReadStream(..), WriteStream(..))
import Xkb (XkbConfig(XkbConfig), printSymbols, printTypes, printKeycodes, printXCompose, getFileAndVariant, parseXkbLayoutVariant)

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

    | PklCompact

    | XkbCustomShortcuts
    | XkbRedirectAll
    | XkbRedirectIfExtend
    deriving (Eq, Show, Read)

execOptions ∷ Options → IO ()
execOptions (Options _ [] _ _) = error "no input files."
execOptions o@(Options Nothing (Standard:_) _ _) = execOptions o { __inputType = Just Json}
execOptions o@(Options Nothing (File fname:_) _ _) =
  case takeExtension fname of
    ".json"      → execOptions o { __inputType = Just Json }
    ""           → execOptions o { __inputType = Just Xkb }
    ".ini"       → execOptions o { __inputType = Just Pkl }
    ".klc"       → execOptions o { __inputType = Just Klc }
    ".keylayout" → execOptions o { __inputType = Just Keylayout }
    xs → error ("unknown layout type ‘" ⊕ xs ⊕ "’.")
execOptions o@(Options _ _ [] _) = execOptions o { __outputs = [Output Json Standard] }
execOptions (Options (Just inputType) inputs outputs extraOptions) = printLog $ do
    layout' ← mconcat <$> traverse (input inputType) inputs
    let layout = execExtraOptions extraOptions ∘ layout'
    forM_ outputs (\out → output out extraOptions layout)

input ∷ FileType → Stream → LoggerT IO (FileType → Layout)
input Json = parseWith (\fname → bimap ((fname ⊕ ": ") ⊕) pure ∘ eitherDecode ∘ removeJsonComments)
input Xkb =
    getFileAndVariant ∘ toFname >>> (\(fname, variant) →
    parseWith' (parseXkbLayoutVariant variant) (File fname)) >$> const
input Pkl = parseWith parsePklLayout >$> const
input Klc = parseWith parseKlcLayout >$> const
input Keylayout = error "importing from a keylayout file is not supported"

parseWith ∷ ReadStream α ⇒ (String → α → Either String (Logger β)) →
    Stream → LoggerT IO β
parseWith = parseWith' ∘ fmap3 (mapWriterT (pure ∘ runIdentity))
  where fmap3 = fmap ∘ fmap ∘ fmap

parseWith' ∷ ReadStream α ⇒ (String → α → Either String (LoggerT IO β)) →
    Stream → LoggerT IO β
parseWith' parser stream = flip ($) stream $
    liftIO ∘ readStream >=>
    parser (toFname stream) >>>
    either (error ∘ ("parse error in " ⊕) ∘ dropWhileEnd (≡'\n')) id

output ∷ Output → [ExtraOption] → (FileType → Layout) → LoggerT IO ()
output (OutputAll Standard) _ = const (error "everything as output must be written to a directory")
output (OutputAll (File dir)) extraOptions = \layout → do
    let name = view (_info ∘ _name) ∘ layout
    let output' t fname
          | isEmptyLayout (layout t) = tell ["ignoring empty layout for " ⊕ show' t]
          | otherwise = output (Output t (File fname)) extraOptions layout
    let fnameJson
          | null (name Json) = error "the layout has an empty name when exported to JSON"
          | otherwise = name Json
    output' Json (dir </> fnameJson <.> "json")
    output' Xkb (dir </> "xkb")
    output' Pkl (dir </> "pkl")
    output' Klc (dir </> "klc")
    output' Keylayout (dir </> "keylayout")
output (Output Json stream) _ = ($ Json) >>>
    liftIO ∘ writeStream stream ∘ encodePretty' (Config 4 layoutOrd layoutDelims)
output (Output Xkb Standard) _ = const (error "XKB as output must be written to a directory")
output (Output Xkb (File dir)) extraOptions = ($ Xkb) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (error "the layout has an empty name when exported to XKB")
    let xkbConfig = liftA3 XkbConfig (XkbCustomShortcuts ∈) (XkbRedirectAll ∈) (XkbRedirectIfExtend ∈) extraOptions
    printIOLogger (dir </> "symbols" </> name) (runReaderT (printSymbols layout) xkbConfig)
    printIOLogger (dir </> "types" </> name) (runReaderT (printTypes layout) xkbConfig)
    printIOLogger (dir </> "keycodes" </> name) (printKeycodes layout)
    printIOLogger (dir </> "XCompose") (pure (printXCompose layout))
    let name' = T.encodeUtf8 (T.pack name)
    let before = "layout=\"\""
    let after  = "layout=\"" ⊕ name' ⊕ "\""
    let replaceLayoutName = B8.unlines ∘ replace before after ∘ B8.lines
    localFile  ← liftIO $ B.readFile "install-local.sh"  <|> pure defXkbLocal
    systemFile ← liftIO $ B.readFile "install-system.sh" <|> pure defXkbSystem
    liftIO $ B.writeFile (dir </> "install-local.sh")  (replaceLayoutName localFile)
    liftIO $ B.writeFile (dir </> "install-system.sh") (replaceLayoutName systemFile)
    liftIO $ makeExecutable (dir </> "install-local.sh")
    liftIO $ makeExecutable (dir </> "install-system.sh")
  where
    makeExecutable fname =
        getPermissions fname >>=
        setPermissions fname ∘ setOwnerExecutable True
output (Output Pkl Standard) _ = const (error "PKL as output must be written to a directory")
output (Output Pkl (File dir)) extraOptions = ($ Pkl) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (error "the layout has an empty name when exported to PKL")
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
        printIOLogger (dir </> ("pkl" ⊕ nameM') <.> "ini") (printedPklData layout')
        printIOLogger (layoutFile nameM' layout') (printedLayoutData layout')
    pklFile ← liftIO $ B.readFile "pkl.exe" <|> pure defPklFile
    liftIO $ B.writeFile (dir </> "pkl.exe") pklFile
output (Output Klc Standard) _ = ($ Klc) >>>
    printIOLoggerStream Standard ∘ fmap printKlcData ∘ toKlcData
output (Output Klc (File dir)) _ = ($ Klc) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (error "the layout has an empty name when exported to KLC")
    let fname l = dir </> view (_info ∘ _name) l <.> "klc"
    forM_ ((∅) : view _mods layout) $ \layoutMod → do
        let layout' = applyModLayout layoutMod layout
        printIOLogger (fname layout') (printKlcData <$> toKlcData layout')
output (Output Keylayout Standard) _ = ($ Keylayout) >>>
    printIOLoggerStream Standard ∘ fmap printKeylayout ∘ toKeylayout
output (Output Keylayout (File dir)) _ = ($ Keylayout) >>> \layout → do
    let name = view (_info ∘ _name) layout
    when (null name) (error "the layout has an empty name when exported to keylayout")
    let fname l = dir </> view (_info ∘ _name) l <.> "keylayout"
    forM_ ((∅) : view _mods layout) $ \layoutMod → do
        let layout' = applyModLayout layoutMod layout
        printIOLogger (fname layout') (printKeylayout <$> toKeylayout layout')

defPklFile, defXkbLocal, defXkbSystem ∷ B.ByteString
defPklFile   = $(embedFile "files/pkl.exe")
defXkbLocal  = $(embedFile "files/install-local.sh")
defXkbSystem = $(embedFile "files/install-system.sh")

execExtraOptions ∷ [ExtraOption] → Layout → Layout
execExtraOptions = flip (foldr execExtraOption)

execExtraOption ∷ ExtraOption → Layout → Layout
execExtraOption (RemoveShiftstates cols) =
    over (_keys ∘ traverse)
        (over _letters (filterOnIndex (∉ cols)) ∘
         over _shiftstates (filterOnIndex (∉ cols)))
execExtraOption RemoveEmptyLetters = over (_keys ∘ traverse) removeEmptyLetters
execExtraOption CombineMods =
    over _mods (map mconcat ∘ tail ∘ subsequences)
execExtraOption UnifyShiftstates = over _keys (fst ∘ unifyShiftstates)
execExtraOption _ = id

printIOLogger ∷ WriteStream α ⇒ FilePath → Logger α → LoggerT IO ()
printIOLogger = printIOLoggerStream ∘ File

printIOLoggerStream ∷ WriteStream α ⇒ Stream → Logger α → LoggerT IO ()
printIOLoggerStream stream =
    mapWriterT (pure ∘ runIdentity) >=>
    liftIO ∘ writeStream stream

printLog ∷ LoggerT IO () → IO ()
printLog =
    execWriterT >=>
    traverse_ (\xs → hPutStrLn stderr $ "klfc: warning: " ⊕ xs ⊕ ".") ∘ nub

main ∷ IO ()
main = execParser opts >>= execOptions
  where
    opts = info (helper <*> options)
      ( fullDesc
      ⊕ header "Keyboard Layout Files Creator - export a keyboard layout to different formats"
      )

options ∷ Parser Options
options = Options
    <$> (usageText "IMPORT TYPE" *> helpHeader "\n\b\bImport types:" *> optional parseInputType)
    <*> (helpHeader "\n\b\bImport files:" *> some parseInput)
    <*> (usageText "OUTPUTS" *> helpHeader "\n\b\bOutput files:" *> many parseOutput)
    <*> (usageText "OPTIONS" *> helpHeader "\n\b\bExtra Options:" *> (many ∘ asum)
            [ parseExtraOption
            , helpHeader "\b\bPKL:" *> parsePklOption
            , helpHeader "\b\bXKB:" *> parseXkbOption
            ]
        )

parseInputType ∷ Parser FileType
parseInputType = asum
    [ flag' Json (long "from-json" ⊕ hidden ⊕ help "Read from a JSON file")
    , flag' Xkb (long "from-xkb" ⊕ hidden ⊕ help "Import from a XKB symbols file")
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
    , OutputAll <$> streamOption (long "output" ⊕ short 'o' ⊕ metavar "DIRECTORY" ⊕ hidden ⊕ help "Export to all file types")
    ]

parseExtraOption ∷ Parser ExtraOption
parseExtraOption = asum
    [ RemoveShiftstates <$> option (str >>= parseList) (long "remove-shiftstates" ⊕ metavar "INDEX" ⊕ hidden ⊕ help "Remove one or more shiftstates with their letters. The shiftstates are identified with their index (starting with 0). Multiple indices are seperated with a comma.")
    , flag' RemoveEmptyLetters (long "remove-empty-letters" ⊕ hidden ⊕ help "Remove empty letters at the end of each key")
    , flag' CombineMods (long "combine-mods" ⊕ hidden ⊕ help "Combine all the mods in the layout. For example, if the layout has the mods ‘Wide’ and ‘Angle’, a new mod ‘WideAngle’ will be created.")
    , flag' UnifyShiftstates (long "unify-shiftstates" ⊕ hidden ⊕ help "Change the shiftstates of all keys such that all keys have the same shiftstates")
    ]

parsePklOption ∷ Parser ExtraOption
parsePklOption =
    flag' PklCompact (long "pkl-compact" ⊕ hidden ⊕ help "Set PKL to compact mode")

parseXkbOption ∷ Parser ExtraOption
parseXkbOption = asum
    [ flag' XkbCustomShortcuts (long "xkb-custom-shortcut-positions" ⊕ hidden ⊕ help "Use the shortcut positions from the ‘shortcutPos’ attributes for shortcuts in XKB")
    , flag' XkbRedirectAll (long "xkb-redirect-all" ⊕ hidden ⊕ help "Always use the ‘redirect’ action in XKB, if possible. This may help some programs detect special actions on different layers.")
    , flag' XkbRedirectIfExtend (long "xkb-redirect-if-extend" ⊕ hidden ⊕ help "Always use the ‘redirect’ action in XKB if the extend modifier (LevelFive) is active, if possible. This may help some programs detect special actions on the extend layer.")
    ]

usageText ∷ String → Parser ()
usageText s = option disabled (value () ⊕ metavar s)

helpHeader ∷ String → Parser ()
helpHeader s = option disabled (value () ⊕ metavar s ⊕ hidden ⊕ helpDoc (pure (∅)))

streamOption ∷ Options.Applicative.Mod OptionFields Stream → Parser Stream
streamOption = option (parseStream <$> str)

parseList ∷ (Monad m, Read α) ⇒ String → m [α]
parseList s = either fail pure $ readEither ("[" ⊕ s ⊕ "]")

parseStream ∷ String → Stream
parseStream "-" = Standard
parseStream s   = File s
