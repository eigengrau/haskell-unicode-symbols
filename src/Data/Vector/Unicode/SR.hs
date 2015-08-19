{-# LANGUAGE UnicodeSyntax #-}

module Data.Vector.Unicode.SR (
    (⊳), (⊲), (⋈), (∅)
  ) where

import           Data.Vector (Vector)
import qualified Data.Vector as Vector


(⊳) ∷ α → Vector α → Vector α
(⊳) = Vector.cons
infixl 5 ⊳

(⊲) ∷ Vector α → α → Vector α
(⊲) = Vector.snoc
infixr 5 ⊲

(⋈) ∷ Vector α → Vector α → Vector α
(⋈) = (Vector.++)
infixr 5 ⋈

(∅) ∷ Vector α
(∅) = Vector.empty
