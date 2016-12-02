{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}

module Xkb.Parse where

import BasePrelude hiding (try)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, lookupR, whenNothing)
import qualified WithPlus as WP (fromList, singleton)

import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (tell, mapWriterT)
import Data.Functor.Identity (runIdentity)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Lens.Micro.Platform (set, over, _2, _Left)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory, splitFileName)
import Text.Megaparsec hiding (Pos)
import qualified Text.Megaparsec.String as S (Parser)
import Text.Megaparsec.Text.Lazy (Parser)

import Layout.Key (Key(Key))
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Linux
import Xkb.Types (isCapslock)

layout ∷ FilePath → Parser (String, LoggerT IO Layout)
layout dir = liftA2 (,)
    (manyTill (void xkbName <|> ws1) (char '"') *> xkbName <* char '"')
    (ws *> char '{' *> ws *> xkbLines dir <* char '}' <* ws <* char ';' <* ws)

xkbLines ∷ FilePath → Parser (LoggerT IO Layout)
xkbLines dir = fmap mconcat ∘ sequence <$> many (xkbLine dir)

xkbLine ∷ FilePath → Parser (LoggerT IO Layout)
xkbLine dir = pure (∅) <$ keySetting
    <|> fmap (flip (set _keys) (∅) ∘ maybeToList) ∘ mapWriterT (pure ∘ runIdentity) <$> key
    <|> include dir
    <|> pure (∅) <$ groupName
    <|> pure (∅) <$ modifierMap
    <|> pure (∅) <$ virtualModifiers

groupName ∷ Parser String
groupName = keyDescription (string' "name") *> char '"' *> manyTill anyChar (char '"') <* ws <* char ';' <* ws

key ∷ Parser (Logger (Maybe Key))
key = do
    void $ optional (string' "replace" *> ws) *>
           optional (string' "override" *> ws) *>
           string' "key" *> ws
    pos ← position
    void $ char '{' *> ws
    letters ← fromMaybe (pure []) ∘ asum <$> xkbLetters `sepBy` comma
    void $ char '}' *> ws *> char ';' *> ws
    pure $ case pos of
        Left  s → Nothing <$ tell [s]
        Right p → fmap Just $
                    Key
                      <$> pure p
                      <*> pure Nothing
                      <*> (shiftstates <$> letters)
                      <*> (catMaybes <$> letters)
                      <*> (Just ∘ isCapslock' ∘ catMaybes <$> letters)
  where
    isCapslock' (x:x':_) = isCapslock x x'
    isCapslock' _        = False

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

xkbLetters ∷ Parser (Maybe (Logger [Maybe Letter]))
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

keyDescription ∷ Parser α → Parser α
keyDescription s = s <* ws <*
                   optional (char '[' *> ws *> xkbName <* ws <* char ']') <*
                   ws <* char '=' <* ws

symbols ∷ Parser (Logger [Maybe Letter])
symbols = fmap sequence $
    char '[' *> ws *> keysym `sepBy` comma <* char ']' <* ws

actions ∷ Parser [(String, String)]
actions = char '[' *> ws *> action `sepBy` comma <* char ']' <* ws

action ∷ Parser (String, String)
action = liftA2 (,)
         (some alphaNumChar)
         (char '(' *> many (alphaNumChar <|> oneOf " _,+-=<>!") <* char ')' <* ws)

comma ∷ Parser ()
comma = char ',' *> ws

position ∷ Parser (Either String Pos)
position = parsePos <$> xkbName <?> ""

parsePos ∷ String → Either String Pos
parsePos s = maybe (Left ("unknown position ‘" ⊕ s ⊕ "’")) Right $
    lookupR s posAndKeycode

keysym ∷ Parser (Logger (Maybe Letter))
keysym = parseLetter <$> xkbName

parseLetter ∷ String → Logger (Maybe Letter)
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
         , parseString xs
         ]

include ∷ FilePath → Parser (LoggerT IO Layout)
include dir = getInclude dir <$>
    (string' "include" *> ws *> char '"' *> some (noneOf "\"") <* char '"' <* ws)

getInclude ∷ FilePath → String → LoggerT IO Layout
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

keySetting ∷ Parser ()
keySetting = string' "key." *> manyTill anyChar (char ';') *> ws *> pure ()

modifierMap ∷ Parser (String, [String])
modifierMap = liftA2 (,)
              (string' "modifier_map" *> ws *> xkbName <* ws)
              (char '{' *> ws *> xkbName `sepBy` comma <* char '}' <* ws <* char ';' <* ws)

virtualModifiers ∷ Parser [String]
virtualModifiers = string' "virtual_modifiers" *> ws *> xkbName `sepBy` comma <* char ';' <* ws

xkbName ∷ Parser String
xkbName = some (alphaNumChar <|> oneOf "_+-<>()") <* ws

ws ∷ Parser ()
ws = void $ many singleSpace

ws1 ∷ Parser ()
ws1 = void (some singleSpace) <?> "white space"

comment ∷ Parser ()
comment = void $ (string "//" <|> string "#") *> manyTill anyChar eol

singleSpace ∷ Parser ()
singleSpace = void (some spaceChar) <|> comment <?> ""

boolean ∷ Parser Bool
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

parseXkbLayoutVariant ∷ String → FilePath → L.Text → Either String (LoggerT IO Layout)
parseXkbLayoutVariant variant fname =
    parse (many (layout (takeDirectory fname)) <* eof) fname >>>
    over _Left parseErrorPretty >=>
    noLayoutWithName ∘ lookup variant
  where
    noLayoutWithName = maybe (Left $ fname ⊕ ": unknown layout variant ‘" ⊕ variant ⊕ "’") pure
