{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Permutation
    ( Permutation
    , fromAssocs
    , fromCycles
    , isEmptyPermutation
    , permutate
    , inverse
    , elements
    , assocs
    , toCycles
    , toDisjunctCycles
    ) where

import BasePrelude
import Prelude.Unicode
import Data.List.Unicode ((∖))
import Data.Monoid.Unicode ((⊕))
import Util (HumanReadable(..), sequenceTuple)

import qualified Control.Monad.Fail as Fail
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import Data.Aeson.Types (Value(Array, Object), typeMismatch)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (pack, unpack)
import Lens.Micro.Platform (over, _1)

data Permutation α
    = PAssocs [(α, α)]
    | PCycles [[α]]

fromAssocs ∷ (MonadFail m, Ord α) ⇒ [(α, α)] → m (Permutation α)
fromAssocs xs
  | isValid   = pure (PAssocs xs)
  | otherwise = Fail.fail "Permutation.fromAssocs: invalid permutation"
  where
    isValid = sort (map fst xs) ≡ sort (map snd xs)

fromCycles ∷ [[α]] → Permutation α
fromCycles = PCycles

isEmptyPermutation ∷ Eq α ⇒ Permutation α → Bool
isEmptyPermutation (PAssocs xs) = all (uncurry (≡)) xs
isEmptyPermutation (PCycles xss) = all short xss
  where
    short []  = True
    short [_] = True
    short _   = False

instance Ord α ⇒ Eq (Permutation α) where
    x == y = compare x y == EQ

instance Ord α ⇒ Ord (Permutation α) where
    compare = comparing (sort ∘ assocs)

instance (Show α, Ord α) ⇒ Show (Permutation α) where
    show = show ∘ toCycles

instance (Read α) ⇒ Read (Permutation α) where
    readsPrec z = fmap (over _1 fromCycles) ∘ readsPrec z

instance Eq α ⇒ Semigroup (Permutation α) where
    PAssocs xs1  <> PAssocs xs2  = PAssocs (xs1 `mappendAssocs` xs2)
    PCycles xss1 <> PCycles xss2 = PCycles (xss1 ⧺ xss2)
    p1@(PAssocs _) <> p2@(PCycles _) = p1 ⊕ PAssocs (assocs p2)
    p1@(PCycles _) <> p2@(PAssocs _) = p1 ⊕ PCycles (toCycles p2)

instance Eq α ⇒ Monoid (Permutation α) where
    mempty = PAssocs []
    mappend = (<>)

mappendAssocs ∷ Eq α ⇒ [(α, α)] → [(α, α)] → [(α, α)]
xs1 `mappendAssocs` [] = xs1
xs1 `mappendAssocs` ((x, x'):xs) =
  case break ((≡) x' ∘ fst) xs1 of
    (_, []) → (x, x') : xs1 `mappendAssocs` xs
    (ys, (_, y):ys') → (x, y) : (ys ⧺ ys') `mappendAssocs` xs

instance (ToJSON α, HumanReadable α) ⇒ ToJSON (Permutation α) where
    toJSON (PAssocs xs)  = Object (HM.fromList (map (T.pack ∘ toString *** toJSON) xs))
    toJSON (PCycles xss) = toJSON xss

instance (FromJSON α, HumanReadable α, Ord α) ⇒ FromJSON (Permutation α) where
    parseJSON (Object o) =
        traverse (sequenceTuple ∘ (parseString ∘ T.unpack *** parseJSON)) (HM.toList o) >>=
        fromAssocs ∘ sortBy (comparing fst)
    parseJSON (Array a) = fromCycles <$> traverse parseJSON (toList a)
    parseJSON v = typeMismatch "String or Array" v

permutate ∷ Eq α ⇒ Permutation α → α → α
permutate (PAssocs xs)  = fromMaybe <*> flip lookup xs
permutate (PCycles xss) = flip (foldr (flip cycleLookup)) xss

cycleLookup ∷ Eq α ⇒ α → [α] → α
cycleLookup x [] = x
cycleLookup x yys@(y:_) =
  case dropWhile (≢ x) yys of
    [] → x
    [_] → y
    (_:y':_) → y'

inverse ∷ Permutation α → Permutation α
inverse (PAssocs xs)  = PAssocs (map swap xs)
inverse (PCycles xss) = PCycles (reverse (map reverse xss))

elements ∷ Eq α ⇒ Permutation α → [α]
elements (PAssocs xs) = map fst (filter (uncurry (≢)) xs)
elements p@(PCycles xss) = nub (filter (liftA2 (≢) id (permutate p)) (concat xss))

assocs ∷ Eq α ⇒ Permutation α → [(α, α)]
assocs (PAssocs xs)  = filter (uncurry (≢)) xs
assocs p@(PCycles _) = zip elms (map (permutate p) elms)
  where elms = elements p

toCycles ∷ Eq α ⇒ Permutation α → [[α]]
toCycles p@(PAssocs _) = toDisjunctCycles p
toCycles (PCycles xss) = xss

toDisjunctCycles ∷ Eq α ⇒ Permutation α → [[α]]
toDisjunctCycles = liftA2 toDisjunctCycles' elements id

toDisjunctCycles' ∷ Eq α ⇒ [α] → Permutation α → [[α]]
toDisjunctCycles' [] _ = []
toDisjunctCycles' (x:xs) p =
  case cyc of
    [_] → toDisjunctCycles' (xs ∖ cyc) p
    _   → cyc : toDisjunctCycles' (xs ∖ cyc) p
  where
    cyc = x : (takeWhile (≢ x) ∘ tail ∘ iterate (permutate p) $ x)
