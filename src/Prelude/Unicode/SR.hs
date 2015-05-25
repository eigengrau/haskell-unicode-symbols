{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnicodeSyntax #-}


module Prelude.Unicode.SR (
    (×), (⨯),
    (↑), (⇑),
    (‼),
    (⊕), (⟴), (⬲), (+≫), (≪+),
    (⦷),
    (⁄),
    (↑≪), (≫↑), (⫵),
    (↑+), (↑×),
    (∥), (‖),
    (&), (<&>)
  ) where


import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Arrow
import           Control.Monad
import           GHC.Exts                    (IsList, Item, fromList, toList)


(×) ∷ Num α ⇒ α → α → α
(×) = (*)
infixl 7 ×


(↑) ∷ (Num α, Integral β) ⇒ α → β → α
(↑) = (^)
infixr 8 ↑


(⇑) ∷ (Fractional α, Integral β) ⇒ α → β → α
(⇑) = (^^)
infixr 8 ⇑


(‼) ∷ [α] → Int → α
(‼) = (!!)


-- Element-wise product/sum.
(↑×) ∷ Num α ⇒ [α] → [α] → [α]
(↑×) = zipWith (×)

(↑+) ∷ Num α ⇒ [α] → [α] → [α]
(↑+) = zipWith (+)


-- Fancy fractions.
(⁄) ∷ Fractional α ⇒ α → α → α
(⁄) = (/)
infixl 7 ⁄


-- Vector or cross product.
(⨯) ∷ (IsList α, IsList β, IsList γ, Item γ ~ (Item α, Item β))
    ⇒ α → β → γ
x ⨯ y = fromList $ (,) ⦷ toList x ⊛ toList y


-- Monoid plus.
(⊕) ∷ Monoid μ ⇒ μ → μ → μ
(⊕) = (⟴)
infixr 6 ⊕

(⟴) ∷ Monoid μ ⇒ μ → μ → μ
(⟴) = mappend
infixl 6 ⟴

(⬲) ∷ Monoid μ ⇒ μ → μ → μ
(⬲) = flip mappend
infixl 6 ⬲


-- Alternative/monad plus.
(+≫) ∷ Alternative φ ⇒ φ α → φ α → φ α
(+≫) = (<|>)
infixl 3 +≫

(≪+) ∷ Alternative φ ⇒ φ α → φ α → φ α
(≪+) = flip (<|>)
infixl 3 ≪+


-- Applicative fmap.
(⦷) ∷ Functor φ ⇒ (α → β) → φ α → φ β
(⦷) = (<$>)
infixl 4 ⦷


-- Low-fixity function application. Beware that $ has a special typing
-- rule in GHC when used with runST, which would be lost by doing
-- this. Cf.
-- http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg18923.html
(‖) ∷ (α → β) → α → β
(‖) = ($)
infixr 0 ‖

(∥) ∷ (α → β) → α → β
(∥) = ($)
infixr 0 ∥


-- Arrows ops.
(↑≪) ∷ Arrow τ ⇒ (β → γ) → τ α β → τ α γ
(↑≪) = (^<<)
infixr 1 ↑≪

(≫↑) ∷ Arrow τ ⇒ τ α β → (β → γ) → τ α γ
(≫↑) = (>>^)
infixr 1 ≫↑

(⫵) ∷ Arrow τ ⇒ τ α β → τ α β' → τ α (β, β')
(⫵) = (&&&)
infixr 3 ⫵


-- Reverse function application.
(&) ∷ α → (α → β) → β
(&) = flip ($)
infixl 0 &


-- Reverse infix fmap.
(<&>) ∷ Functor f ⇒ f α → (α → β) → f β
(<&>) = flip (<$>)
infixl 4 <&>
