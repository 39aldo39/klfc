{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Xkb.Parse where

import BasePrelude hiding (try)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, lookupR)
import qualified WithPlus as WP (fromList, singleton)

import Control.Monad.Writer (tell, mapWriterT)
import Data.Functor.Identity (runIdentity)
import qualified Data.Text.Lazy as L (Text)
import Lens.Micro.Platform (set, over, _2)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Text.Lazy (Parser)

import Layout.Key (Key(Key))
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Linux
import Xkb.Types (isCapslock)

layout ∷ Parser (Logger Layout)
layout =
    manyTill (void xkbName <|> ws1) (char '"') *>
    xkbName *> char '"' *>
    ws *> char '{' *> ws *>
    xkbLines <*
    char '}' <* ws <* char ';' <* ws

xkbLines ∷ Parser (Logger Layout)
xkbLines = fmap mconcat ∘ sequence <$> many xkbLine

xkbLine ∷ Parser (Logger Layout)
xkbLine = pure (∅) <$ try keySetting
    <|> fmap (flip (set _keys) (∅) ∘ maybeToList) <$> key
    <|> fmap (flip (set _singletonKeys) (∅)) <$> try singletonKey
    <|> pure (∅) <$ try groupName
    <|> pure (∅) <$ try modifierMap
    <|> pure (∅) <$ try virtualModifiers

groupName ∷ Parser String
groupName = keyDescription (string' "name") *> char '"' *> manyTill anyChar (char '"') <* ws <* char ';' <* ws

key ∷ Parser (Logger (Maybe Key))
key = do
    try (void $ optional (string' "replace" *> ws) *>
           optional (string' "override" *> ws) *>
           string' "key" *> ws)
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
                      <*> letters
                      <*> (Just ∘ isCapslock' <$> letters)
  where
    isCapslock' (x:x':_) = isCapslock x x'
    isCapslock' _        = False

shiftstates ∷ [Letter] → [Shiftstate]
shiftstates = zipWith const
    [ (∅)
    , WP.singleton M.Shift
    , WP.singleton M.AltGr
    , WP.fromList [M.AltGr, M.Shift]
    , WP.singleton M.Extend
    , WP.fromList [M.Extend, M.Shift]
    , WP.fromList [M.Extend, M.AltGr]
    , WP.fromList [M.Extend, M.AltGr, M.Shift]
    ]

xkbLetters ∷ Parser (Maybe (Logger [Letter]))
xkbLetters = Just <$> symbols
      <|> Just <$> (try (keyDescription (string' "symbols")) *> symbols)
      <|> Nothing <$ (try (keyDescription (string' "actions")) *> actions)
      <|> Nothing <$ (try (keyDescription (string' "type")) *>
          char '"' *> many (noneOf "\"") <* char '"' <* ws)
      <|> Nothing <$ try (keyDescription (string' "lock" *> (string' "s" <|> string' "ing") <?> "\"locks\"") *>
          boolean <* ws)
      <|> Nothing <$ try (keyDescription (string' "repeat" *> optional (string' "s" <|> string' "ing")) *>
          (void (try boolean) <|> void (string' "default")) <* ws)
      <|> Nothing <$ try (keyDescription (string' "v" *> optional (string' "irtual") *>
                          string' "mod" *> optional (string "ifier") *> string "s" <?> "\"vmods\"") *>
          xkbName)
      <|> Nothing <$ try (keyDescription (string' "overlay" *> some digitChar) *>
          xkbName)

keyDescription ∷ Parser α → Parser α
keyDescription s = s <* ws <*
                   optional (char '[' *> ws *> xkbName <* ws <* char ']') <*
                   ws <* char '=' <* ws

symbols ∷ Parser (Logger [Letter])
symbols = fmap sequence $
    try (char '[') *> ws *> keysym `sepBy` comma <* char ']' <* ws

actions ∷ Parser [(String, String)]
actions = char '[' *> ws *> action `sepBy` comma <* char ']' <* ws

action ∷ Parser (String, String)
action = liftA2 (,)
         (some alphaNumChar)
         (char '(' *> many (alphaNumChar <|> oneOf "_,+-=<>!") <* char ')' <* ws)

comma ∷ Parser ()
comma = char ',' *> ws *> pure ()

position ∷ Parser (Either String Pos)
position = try (parsePos <$> xkbName) <?> ""

parsePos ∷ String → Either String Pos
parsePos xs = maybe (Left ("unknown position ‘" ⊕ xs ⊕ "’")) Right $
              lookupR xs posAndKeycode

keysym ∷ Parser (Logger Letter)
keysym = parseLetter <$> xkbName

parseLetter ∷ String → Logger Letter
parseLetter "NoSymbol" = pure LNothing
parseLetter [x] = pure (Char x)
parseLetter ('U':xs)
    | isJust unicodeChar = pure (Char (chr (fromJust unicodeChar)))
    where unicodeChar = readMaybe ('0':'x':xs) ∷ Maybe Int
parseLetter ('0':'x':xs)
    | fmap (≥ 0) unicodeChar' ≡ Just True = pure (Char (chr (fromJust unicodeChar')))
    | isJust unicodeChar                  = pure (Char (chr (fromJust unicodeChar)))
    where unicodeChar = readMaybe ('0':'x':xs) ∷ Maybe Int
          unicodeChar' = subtract 0x1000000 <$> unicodeChar
parseLetter xs = maybe (LNothing <$ tell ["unknown letter ‘" ⊕ xs ⊕ "’"]) pure $
    asum [ Char <$> lookupR xs charAndString
         , Dead <$> lookupR xs deadKeysAndLinuxDeadKeys
         , Action <$> lookupR xs (map (over _2 __symbol) actionAndLinuxAction)
         , parseString xs
         ]

singletonKey ∷ Parser (Logger [SingletonKey])
singletonKey = do
    name ← string' "include" *> ws *>
           char '"' *> some (noneOf "\"") <* char '"' <* ws
    pure $ maybe ([] <$ tell ["unknown include ‘" ⊕ name ⊕ "’"]) pure (parseSingletonKey name)

parseSingletonKey ∷ String → Maybe [SingletonKey]
parseSingletonKey xs = asum
    [ pure <$> lookupR xs' singleIncludes
    , (\(x,y) → [x,y]) <$> lookupR xs' doubleIncludes
    , [] <$ lookupR xs' modMappingIncludes
    ]
  where xs' = "include \"" ⊕ xs ⊕ "\""

keySetting ∷ Parser ()
keySetting = try (string' "key.") *> manyTill anyChar (char ';') *> ws *> pure ()

modifierMap ∷ Parser (String, [String])
modifierMap = liftA2 (,)
              (string' "modifier_map" *> ws *> xkbName <* ws)
              (char '{' *> ws *> xkbName `sepBy` comma <* char '}' <* ws <* char ';' <* ws)

virtualModifiers ∷ Parser [String]
virtualModifiers = string' "virtual_modifiers" *> ws *> xkbName `sepBy` comma <* char ';' <* ws

xkbName ∷ Parser String
xkbName = some (alphaNumChar <|> oneOf ("_+-<>()" ∷ String)) <* ws

ws ∷ Parser ()
ws = void $ many singleSpace

ws1 ∷ Parser ()
ws1 = void (some singleSpace) <?> "white space"

comment ∷ Parser ()
comment = void $ (try (string "//") <|> string "#") *> manyTill anyChar eol

singleSpace ∷ Parser ()
singleSpace = void (some spaceChar) <|> try comment <?> ""

boolean ∷ Parser Bool
boolean = True <$ (string' "true" <|> string' "yes" <|> string' "on")
   <|> False <$ (string' "false" <|> string' "no" <|> string' "off")

parseXkbLayout ∷ L.Text → LoggerT IO Layout
parseXkbLayout text =
  case parse layout "" text of
    Right logger → mapWriterT (pure ∘ runIdentity) logger
    Left e → fail (dropWhileEnd (≡'\n') (parseErrorPretty e))

parseXkbLayouts ∷ L.Text → LoggerT IO [Layout]
parseXkbLayouts text =
  case sequence <$> parse (many layout <* eof) "" text of
    Right logger → mapWriterT (pure ∘ runIdentity) logger
    Left e → fail (dropWhileEnd (≡'\n') (parseErrorPretty e))
