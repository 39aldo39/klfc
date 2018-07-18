{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Xkb.Parse where

import BasePrelude hiding (many, some)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, lookupR, whenNothing, (>$>))

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Writer (writer, runWriterT, tell)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.Void (Void)
import Lens.Micro.Platform (set, over, _2, _Left)
import System.Directory (doesFileExist)
import System.FilePath ((</>), splitFileName)
import Text.Megaparsec hiding (Pos, many, some)
import Text.Megaparsec.Char

import Layout.Key (Key(Key))
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Linux

type Parser = MonadParsec Void L.Text

layout ∷ (Logger m, MonadIO m, Parser p) ⇒ FilePath → p (String, m Layout)
layout dir = liftA2 (,)
    (manyTill (void xkbName <|> ws1) (char '"') *> xkbName <* char '"')
    (ws *> char '{' *> ws *> xkbLines dir <* char '}' <* ws <* char ';' <* ws)

xkbLines ∷ (Logger m, MonadIO m, Parser p) ⇒ FilePath → p (m Layout)
xkbLines dir = fmap mconcat ∘ sequence <$> many (xkbLine dir)

xkbLine ∷ (Logger m, MonadIO m, Parser p) ⇒ FilePath → p (m Layout)
xkbLine dir = pure (∅) <$ keySetting
    <|> fmap (flip (set _keys) (∅) ∘ maybeToList) ∘ writer <$> runWriterT key
    <|> include dir
    <|> pure ∘ flip (set (_info ∘ _fullName)) (∅) <$> groupName
    <|> pure (∅) <$ modifierMap
    <|> pure (∅) <$ virtualModifiers

groupName ∷ Parser m ⇒ m String
groupName = keyDescription (string' "name") *> char '"' *> manyTill anyChar (char '"') <* ws <* char ';' <* ws

key ∷ (Logger m, Parser m) ⇒ m (Maybe Key)
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

shiftstates ∷ [Maybe Letter] → [Shiftlevel]
shiftstates = catMaybes ∘ zipWith (<$)
    [ M.empty
    , M.singleton M.Shift
    , M.singleton M.AltGr
    , M.fromList [M.AltGr, M.Shift]
    , M.singleton M.Extend
    , M.fromList [M.Extend, M.Shift]
    , M.fromList [M.Extend, M.AltGr]
    , M.fromList [M.Extend, M.AltGr, M.Shift]
    ]

xkbLetters ∷ (Logger m, Parser m) ⇒ m (Maybe [Maybe Letter])
xkbLetters = asum
  [ Just <$> symbols
  , Just <$> (keyDescription (string' "symbols") *> symbols)
  , Nothing <$ (keyDescription (string' "actions") *> actions)
  , Nothing <$ (keyDescription (string' "type") *>
        char '"' *> many (noneOf ['"']) <* char '"' <* ws)
  , Nothing <$ (keyDescription (string' "locks" <|> string' "locking" <?> "\"locks\"") *>
        boolean <* ws)
  , Nothing <$ (keyDescription (string' "repeat" <|> string' "repeats" <|> string' "repeating" <?> "repeat") *>
        (void boolean <|> void (string' "default")) <* ws)
  , Nothing <$ (keyDescription (string' "vmods" <|> string' "virtualmods" <|> string' "virtualmodifiers" <?> "\"vmods\"") *>
        xkbName)
  , Nothing <$ (keyDescription (string' "overlay" *> some digitChar) *>
        xkbName)
  ]

keyDescription ∷ Parser m ⇒ m α → m α
keyDescription s = s <* ws <*
                   optional (char '[' *> ws *> xkbName <* ws <* char ']') <*
                   ws <* char '=' <* ws

symbols ∷ (Logger m, Parser m) ⇒ m [Maybe Letter]
symbols =
    char '[' *> ws *> keysym `sepBy` comma <* char ']' <* ws

actions ∷ Parser m ⇒ m [(String, String)]
actions = char '[' *> ws *> action `sepBy` comma <* char ']' <* ws

action ∷ Parser m ⇒ m (String, String)
action = liftA2 (,)
         (some alphaNumChar)
         (char '(' *> many (alphaNumChar <|> oneOf [' ','_',',','+','-','=','<','>','!']) <* char ')' <* ws)

comma ∷ Parser m ⇒ m ()
comma = char ',' *> ws

position ∷ Parser m ⇒ m (Either String Pos)
position = parsePos <$> xkbName <?> ""

parsePos ∷ String → Either String Pos
parsePos s = maybe (Left ("unknown position ‘" ⊕ s ⊕ "’")) Right $
    lookupR s posAndKeycode

keysym ∷ (Logger m, Parser m) ⇒ m (Maybe Letter)
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

include ∷ (Logger m, MonadIO m, Parser p) ⇒ FilePath → p (m Layout)
include dir = fmap (set _info (∅)) ∘ getInclude dir <$>
    (string' "include" *> ws *> char '"' *> some (noneOf ['"']) <* char '"' <* ws)

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
    ]
  where xs' = "include \"" ⊕ xs ⊕ "\""

keySetting ∷ Parser m ⇒ m ()
keySetting = string' "key." *> manyTill anyChar (char ';') *> ws *> pure ()

modifierMap ∷ Parser m ⇒ m (String, [String])
modifierMap = liftA2 (,)
              (string' "modifier_map" *> ws *> xkbName <* ws)
              (char '{' *> ws *> xkbName `sepBy` comma <* char '}' <* ws <* char ';' <* ws)

virtualModifiers ∷ Parser m ⇒ m [String]
virtualModifiers = string' "virtual_modifiers" *> ws *> xkbName `sepBy` comma <* char ';' <* ws

xkbName ∷ Parser m ⇒ m String
xkbName = some (alphaNumChar <|> oneOf ['_','+','-','<','>','(',')','"']) <* ws

ws ∷ Parser m ⇒ m ()
ws = void $ many singleSpace

ws1 ∷ Parser m ⇒ m ()
ws1 = void (some singleSpace) <?> "white space"

comment ∷ Parser m ⇒ m ()
comment = void $ (string "//" <|> string "#") *> manyTill anyChar eol

singleSpace ∷ Parser m ⇒ m ()
singleSpace = void (some spaceChar) <|> comment <?> ""

boolean ∷ Parser m ⇒ m Bool
boolean = True <$ (string' "true" <|> string' "yes" <|> string' "on")
   <|> False <$ (string' "false" <|> string' "no" <|> string' "off")

parseXkbFilePath ∷ String → Maybe (String, String)
parseXkbFilePath = (parseMaybe ∷ Stream s ⇒ Parsec Void s a → s → Maybe a) $ liftA2 (,)
    (many (noneOf ['(',')']))
    (fromMaybe "basic" <$> optional (char '(' *> many (noneOf ['(',')']) <* char ')'))

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
