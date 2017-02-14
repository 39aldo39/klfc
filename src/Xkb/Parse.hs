{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Xkb.Parse where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, lookupR, whenNothing, (>$>))
import qualified WithPlus as WP (fromList, singleton)

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Writer (writer, runWriterT, tell)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Lens.Micro.Platform (set, over, _2, _Left)
import System.Directory (doesFileExist)
import System.FilePath ((</>), splitFileName)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Prim (MonadParsec)
import qualified Text.Megaparsec.String as S (Parser)
import Text.Megaparsec.Text.Lazy (Parser)

import Layout.Key (Key(Key))
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Linux

layout ∷ (Logger m, MonadIO m) ⇒ FilePath → Parser (String, m Layout)
layout dir = liftA2 (,)
    (manyTill (void xkbName <|> ws1) (char '"') *> xkbName <* char '"')
    (ws *> char '{' *> ws *> xkbLines dir <* char '}' <* ws <* char ';' <* ws)

xkbLines ∷ (Logger m, MonadIO m) ⇒ FilePath → Parser (m Layout)
xkbLines dir = fmap mconcat ∘ sequence <$> many (xkbLine dir)

xkbLine ∷ (Logger m, MonadIO m) ⇒ FilePath → Parser (m Layout)
xkbLine dir = pure (∅) <$ keySetting
    <|> fmap (flip (set _keys) (∅) ∘ maybeToList) ∘ writer <$> runWriterT key
    <|> include dir
    <|> pure ∘ flip (set (_info ∘ _fullName)) (∅) <$> groupName
    <|> pure (∅) <$ modifierMap
    <|> pure (∅) <$ virtualModifiers

groupName ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m String
groupName = keyDescription (string' "name") *> char '"' *> manyTill anyChar (char '"') <* ws <* char ';' <* ws

key ∷ (Logger m, MonadParsec e s m, Token s ~ Char) ⇒ m (Maybe Key)
key = do
    void $ optional (string' "replace" *> ws) *>
           optional (string' "override" *> ws) *>
           string' "key" *> ws
    pos ← position
    void $ char '{' *> ws
    letters ← fromMaybe [] ∘ asum <$> xkbLetters `sepBy` comma
    let shiftstates' = shiftstates letters
    let letters' = catMaybes letters
    void $ char '}' *> ws *> char ';' *> ws
    case pos of
        Left  s → Nothing <$ tell [s]
        Right p → pure ∘ Just $ Key p Nothing shiftstates' letters' Nothing

shiftstates ∷ [Maybe Letter] → [Shiftstate]
shiftstates = catMaybes ∘ zipWith (<$)
    [ (∅)
    , WP.singleton M.Shift
    , WP.singleton M.AltGr
    , WP.fromList [M.AltGr, M.Shift]
    , WP.singleton M.Extend
    , WP.fromList [M.Extend, M.Shift]
    , WP.fromList [M.Extend, M.AltGr]
    , WP.fromList [M.Extend, M.AltGr, M.Shift]
    ]

xkbLetters ∷ (Logger m, MonadParsec e s m, Token s ~ Char) ⇒ m (Maybe [Maybe Letter])
xkbLetters = asum
  [ Just <$> symbols
  , Just <$> (keyDescription (string' "symbols") *> symbols)
  , Nothing <$ (keyDescription (string' "actions") *> actions)
  , Nothing <$ (keyDescription (string' "type") *>
        char '"' *> many (noneOf "\"") <* char '"' <* ws)
  , Nothing <$ (keyDescription (string' "locks" <|> string' "locking" <?> "\"locks\"") *>
        boolean <* ws)
  , Nothing <$ (keyDescription (string' "repeat" <|> string' "repeats" <|> string' "repeating" <?> "repeat") *>
        (void boolean <|> void (string' "default")) <* ws)
  , Nothing <$ (keyDescription (string' "vmods" <|> string' "virtualmods" <|> string' "virtualmodifiers" <?> "\"vmods\"") *>
        xkbName)
  , Nothing <$ (keyDescription (string' "overlay" *> some digitChar) *>
        xkbName)
  ]

keyDescription ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m α → m α
keyDescription s = s <* ws <*
                   optional (char '[' *> ws *> xkbName <* ws <* char ']') <*
                   ws <* char '=' <* ws

symbols ∷ (Logger m, MonadParsec e s m, Token s ~ Char) ⇒ m [Maybe Letter]
symbols =
    char '[' *> ws *> keysym `sepBy` comma <* char ']' <* ws

actions ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m [(String, String)]
actions = char '[' *> ws *> action `sepBy` comma <* char ']' <* ws

action ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m (String, String)
action = liftA2 (,)
         (some alphaNumChar)
         (char '(' *> many (alphaNumChar <|> oneOf " _,+-=<>!") <* char ')' <* ws)

comma ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m ()
comma = char ',' *> ws

position ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m (Either String Pos)
position = parsePos <$> xkbName <?> ""

parsePos ∷ String → Either String Pos
parsePos s = maybe (Left ("unknown position ‘" ⊕ s ⊕ "’")) Right $
    lookupR s posAndKeycode

keysym ∷ (Logger m, MonadParsec e s m, Token s ~ Char) ⇒ m (Maybe Letter)
keysym = xkbName >>= parseLetter

parseLetter ∷ Logger m ⇒ String → m (Maybe Letter)
parseLetter "NoSymbol" = pure Nothing
parseLetter "VoidSymbol" = pure (Just LNothing)
parseLetter [x] = pure ∘ Just $ Char x
parseLetter ('U':xs)
    | Just c ← readMaybe ('0':'x':xs)
    = pure ∘ Just $ Char (chr c)
parseLetter ('0':'x':xs)
    | Just c ← readMaybe ('0':'x':xs)
    , c' ← fromMaybe c $ mfilter (≥ 0) (Just (c - 0x1000000))
    = pure ∘ Just $ Char (chr c')
parseLetter xs = whenNothing (tell ["unknown letter ‘" ⊕ xs ⊕ "’"]) $
    asum [ Char <$> lookupR xs charAndString
         , Dead <$> lookupR xs deadKeysAndLinuxDeadKeys
         , Action <$> lookupR xs (map (over _2 __symbol) actionAndLinuxAction)
         , uncurry Modifiers ∘ over _2 (:[]) <$> lookupR xs modifierAndSymbol
         , parseString xs
         ]

include ∷ (Logger m, MonadIO m) ⇒ FilePath → Parser (m Layout)
include dir = fmap (set _info (∅)) ∘ getInclude dir <$>
    (string' "include" *> ws *> char '"' *> some (noneOf "\"") <* char '"' <* ws)

getInclude ∷ (Logger m, MonadIO m) ⇒ FilePath → String → m Layout
getInclude dir name
  | Just singletonKeys ← parseSingletonKey name = pure $
        set _singletonKeys singletonKeys (∅)
  | Just (file, variant) ← parseXkbFilePath name = do
        fnameMaybe ← liftIO $ getFilePath file
        case fnameMaybe of
          Just fname → do
            text ← liftIO $ L.readFile fname
            either (\s → (∅) <$ tell [s]) id $ parseXkbLayoutVariant variant fname text
          Nothing → (∅) <$ tell ["could not find the symbols file ‘" ⊕ file ⊕ "’"]
  | otherwise =
        (∅) <$ tell ["could not parse include string ‘" ⊕ name ⊕ "’"]
  where
    getFilePath file = listToMaybe <$> filterM doesFileExist (dirs file)
    dirs file = map (</> file) [dir, "/usr/share/X11/xkb/symbols"]

parseSingletonKey ∷ String → Maybe [SingletonKey]
parseSingletonKey xs = asum
    [ pure <$> lookupR xs' singleIncludes
    , (\(x,y) → [x,y]) <$> lookupR xs' doubleIncludes
    , [] <$ lookupR xs' modMappingIncludes
    ]
  where xs' = "include \"" ⊕ xs ⊕ "\""

keySetting ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m ()
keySetting = string' "key." *> manyTill anyChar (char ';') *> ws *> pure ()

modifierMap ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m (String, [String])
modifierMap = liftA2 (,)
              (string' "modifier_map" *> ws *> xkbName <* ws)
              (char '{' *> ws *> xkbName `sepBy` comma <* char '}' <* ws <* char ';' <* ws)

virtualModifiers ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m [String]
virtualModifiers = string' "virtual_modifiers" *> ws *> xkbName `sepBy` comma <* char ';' <* ws

xkbName ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m String
xkbName = some (alphaNumChar <|> oneOf "_+-<>()") <* ws

ws ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m ()
ws = void $ many singleSpace

ws1 ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m ()
ws1 = void (some singleSpace) <?> "white space"

comment ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m ()
comment = void $ (string "//" <|> string "#") *> manyTill anyChar eol

singleSpace ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m ()
singleSpace = void (some spaceChar) <|> comment <?> ""

boolean ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m Bool
boolean = True <$ (string' "true" <|> string' "yes" <|> string' "on")
   <|> False <$ (string' "false" <|> string' "no" <|> string' "off")

parseXkbFilePath ∷ String → Maybe (String, String)
parseXkbFilePath = parseMaybe $ liftA2 (,)
    (many (noneOf "()") ∷ S.Parser String)
    (fromMaybe "basic" <$> optional (char '(' *> many (noneOf "()") <* char ')'))

getFileAndVariant ∷ FilePath → (FilePath, String)
getFileAndVariant fname = (dir </> file, variant)
  where
    (dir, name) = splitFileName fname
    (file, variant) = fromMaybe (fname, "basic") $ parseXkbFilePath name

parseXkbLayoutVariant ∷ (Logger m, MonadIO m) ⇒ String → FilePath → L.Text → Either String (m Layout)
parseXkbLayoutVariant variant fname =
    parse (many (layout dir) <* eof) fname >>>
    over _Left parseErrorPretty >$>
    fromMaybe unknownVariant ∘ lookup variant >$>
    set (_info ∘ _name) (file ⧺ variant')
  where
    (dir, file) = splitFileName fname
    variant'
      | variant ≡ "basic" = ""
      | otherwise = '-' : variant
    unknownVariant = (∅) <$ tell [fname ⊕ ": unknown layout variant ‘" ⊕ variant ⊕ "’"]
