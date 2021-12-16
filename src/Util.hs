{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Util where

import BasePrelude
import Prelude.Unicode
import Data.List.Unicode ((∖))
import Data.Monoid.Unicode ((⊕))

import qualified Control.Monad.Fail as Fail (fail)
import Control.Monad.State (MonadState, state)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as H
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Lens.Micro.Platform (Lens')
import Paths_keyboard_layout_files_creator (version)

class HumanReadable α where
    typeName ∷ Proxy α → String

    toString ∷ α → String
    default toString ∷ (Eq α, Enum α, Bounded α) ⇒ α → String
    toString x = fromMaybe e (lookup x stringList)
      where
        e = error ("could not convert a " ⊕ tName ⊕ "’ to a string")
        tName = typeName (Proxy ∷ Proxy α)

    parseString ∷ MonadFail m ⇒ String → m α
    default parseString ∷ (Enum α, Bounded α, MonadFail m) ⇒ String → m α
    parseString s = maybe e pure (lookupByR eq s stringList)
      where
        eq = (≡) `on` map toLower ∘ filter (≢'_')
        e = Fail.fail ("‘" ⊕ s ⊕ "’ is not a valid " ⊕ tName)
        tName = typeName (Proxy ∷ Proxy α)

    stringList ∷ (Enum α, Bounded α) ⇒ [(α, String)]
    stringList = map (id &&& toString) [minBound .. maxBound]

    show' ∷ α → String
    show' x = "the " ⊕ tName ⊕ " ‘" ⊕ toString x ⊕ "’"
      where tName = typeName (Proxy ∷ Proxy α)

    hrToJSON ∷ α → Value
    hrToJSON = String ∘ T.pack ∘ toString

    hrParseJSON ∷ Value → Parser α
    hrParseJSON = withText tName (parseString ∘ T.unpack)
      where tName = typeName (Proxy ∷ Proxy α)

instance HumanReadable Char where
    typeName _ = "char"
    toString c = [c]
    parseString [c] = pure c
    parseString _ = fail "not a single char"

showAsList ∷ String → String → [String] → String
showAsList _ _  []  = "nothing"
showAsList t _  [x] = t ⊕ " " ⊕ x
showAsList _ ts xs  = ts ⊕ " " ⊕ intercalate ", " (init xs) ⊕ " and " ⊕ last xs

expectedKeys ∷ MonadFail m ⇒ [T.Text] → Object → m ()
expectedKeys xs o =
  case H.keys o ∖ xs of
    [] → pure ()
    ks → Fail.fail $ "unknown " ⊕ showAsList "key" "keys" (map (\x → "‘" ⊕ T.unpack x ⊕ "’") ks)

lensWithDefault ∷ (σ → α) → (Maybe α → σ → σ) → (σ → Maybe α) → Lens' σ α
lensWithDefault guess setter getter f x =
    (\y → setter (Just y) x) <$> f (fromMaybe (guess x) (getter x))

lensWithDefault' ∷ α → (Maybe α → σ → σ) → (σ → Maybe α) → Lens' σ α
lensWithDefault' x = lensWithDefault (const x)

(!?) ∷ [α] → Int → Maybe α
(!?) xs i =
  case drop i xs of
    []  → Nothing
    x:_ → Just x

ifNonEmpty ∷ Foldable t ⇒ (t α → t α) → t α → t α
ifNonEmpty _ xs | null xs = xs
ifNonEmpty f xs = f xs

lookupByFrom ∷ MonadFail m ⇒ String → (α → α → Bool) → α → [(α, β)] → m β
lookupByFrom name _ _ [] = Fail.fail $ "Util." ⊕ name ⊕ ": key not found"
lookupByFrom name eq y ((x,x'):xs)
  | eq y x = pure x'
  | otherwise = lookupByFrom name eq y xs

lookupBy ∷ MonadFail m ⇒ (α → α → Bool) → α → [(α, β)] → m β
lookupBy = lookupByFrom "lookupBy"

lookup' ∷ (MonadFail m, Eq α) ⇒ α → [(α, β)] → m β
lookup' = lookupByFrom "lookup'" (≡)

lookupR ∷ (MonadFail m, Eq β) ⇒ β → [(α, β)] → m α
lookupR x = lookupByFrom "lookupR" (≡) x ∘ map swap

lookupByR ∷ MonadFail m ⇒ (β → β → Bool) → β → [(α, β)] → m α
lookupByR eq x = lookupByFrom "lookupByR" eq x ∘ map swap

split ∷ (α → Bool) → [α] → NonEmpty [α]
split _ [] = [] :| []
split p (x:xs)
  | p x = [] :| toList (split p xs)
  | otherwise = let y :| ys = split p xs in (x:y) :| ys

groupWith' ∷ Eq β ⇒ (α → β) → [α] → [(β, [α])]
groupWith' f = map (\(x:|xs) → (f x, x:xs)) ∘ NE.groupWith f

groupSortWith ∷ Ord β ⇒ (α → β) → [α] → [(β, [α])]
groupSortWith f = groupWith' f ∘ sortWith f

replace ∷ Eq α ⇒ α → α → [α] → [α]
replace y = replaceWith (≡ y)

replaceWith ∷ (α → Bool) → α → [α] → [α]
replaceWith p y = map (\x → bool x y (p x))

removeSubList ∷ Eq α ⇒ [α] → [α] → [α]
removeSubList _ [] = []
removeSubList [] xs = xs
removeSubList ys xxs@(x:xs) =
    maybe (x : removeSubList ys xs) (removeSubList ys) (stripPrefix ys xxs)

subsets ∷ Eq α ⇒ Set α → [Set α]
subsets = map S.fromAscList ∘ subsequences ∘ S.toAscList

escapeWithQuotes ∷ String → String
escapeWithQuotes s = "\"" ⊕ escape s ⊕ "\""

escape ∷ String → String
escape = foldr escape' ""
  where
    escape' c
      | c ≤ '\DEL' = showLitChar c
      | otherwise  = (:) c

dec2bin ∷ Int → [Int]
dec2bin = unfoldr f
    where f 0 = Nothing
          f x = Just (swap (divMod x 2))

stripSuffix ∷ Eq α ⇒ [α] → [α] → Maybe [α]
stripSuffix xs = fmap reverse ∘ stripPrefix (reverse xs) ∘ reverse

combineOn ∷ Eq β ⇒ (α → [α] → α) → (α → β) → [α] → [α] → [α]
combineOn f key = combineWith ((:[]) ∘: f) ((≡) `on` key)
  where (∘:) = (∘) ∘ (∘)

combineWith ∷ (α → [α] → [α]) → (α → α → Bool) → [α] → [α] → [α]
combineWith _ _  []     ys = ys
combineWith f eq (x:xs) ys = f x eqToX ⧺ combineWith f eq xs ys'
  where (eqToX, ys') = partition (eq x) ys

combineWithOnM ∷ (Eq β, Applicative f) ⇒ (α → [α] → f α) → (α → β) → [α] → [α] → f [α]
combineWithOnM _ _   []     ys = pure ys
combineWithOnM f key (x:xs) ys = liftA2 (:) (f x eqToX) (combineWithOnM f key xs ys')
  where (eqToX, ys') = partition (on (≡) key x) ys

nubWithOn ∷ Eq β ⇒ (α → [α] → γ) → (α → β) → [α] → [γ]
nubWithOn _ _   []     = []
nubWithOn f key (x:xs) = f x eqToX : nubWithOn f key xs'
  where (eqToX, xs') = partition (on (≡) key x) xs

nubWithOnM ∷ (Eq β, Applicative f) ⇒ (α → [α] → f γ) → (α → β) → [α] → f [γ]
nubWithOnM _ _   []     = pure []
nubWithOnM f key (x:xs) = liftA2 (:) (f x eqToX) (nubWithOnM f key xs')
  where (eqToX, xs') = partition (on (≡) key x) xs

nubOn ∷ Eq β ⇒ (α → β) → [α] → [α]
nubOn f = nubBy ((≡) `on` f)

filterOnFst ∷ (α → Bool) → [(α, β)] → [β]
filterOnFst f = map snd ∘ filter (f ∘ fst)

filterOnSnd ∷ (β → Bool) → [(α, β)] → [α]
filterOnSnd f = map fst ∘ filter (f ∘ snd)

filterOnFstM ∷ Monad m ⇒ (α → m Bool) → [(α, β)] → m [β]
filterOnFstM f = fmap (map snd) ∘ filterM (f ∘ fst)

filterOnSndM ∷ Monad m ⇒ (β → m Bool) → [(α, β)] → m [α]
filterOnSndM f = fmap (map fst) ∘ filterM (f ∘ snd)

filterOnIndex ∷ (Int → Bool) → [α] → [α]
filterOnIndex f = filterOnFst f ∘ zip [0..]

concatMapM ∷ (Monad m, Traversable t) ⇒ (α → m [β]) → t α → m [β]
concatMapM f = fmap concat ∘ traverse f

mconcatMapM ∷ (Monad m, Monoid β) ⇒ (α → m β) → [α] → m β
mconcatMapM f = fmap mconcat ∘ traverse f

mapMaybeM ∷ Applicative m ⇒ (α → m (Maybe β)) → [α] → m [β]
mapMaybeM f = foldr (liftA2 (maybe id (:)) ∘ f) (pure [])

tellMaybeT ∷ MonadWriter w m ⇒ w → MaybeT m α
tellMaybeT x = MaybeT (Nothing <$ tell x)

sequenceTuple ∷ Applicative f ⇒ (f α, f β) → f (α, β)
sequenceTuple = uncurry (liftA2 (,))

whenNothing ∷ Applicative f ⇒ f () → Maybe α → f (Maybe α)
whenNothing s Nothing = Nothing <$ s
whenNothing _ x       = pure x

versionStr ∷ String
versionStr = showVersion version

privateChars ∷ [Char]
privateChars = [chr   0xE000 .. chr   0xF8FF]
             ⧺ [chr  0xF0000 .. chr  0xFFFFD]
             ⧺ [chr 0x100000 .. chr 0x10FFFD]

getPrivateChar ∷ MonadState [Char] m ⇒ m Char
getPrivateChar = state (fromMaybe e ∘ uncons)
  where e = error "too many private Unicode characters needed"

allBounded ∷ (Enum α, Bounded α) ⇒ [α]
allBounded = [minBound .. maxBound]

infixr 1 >$>
(>$>) ∷ Functor f ⇒ (α → f β) → (β → γ) → α → f γ
f >$> g = f >>> fmap g
