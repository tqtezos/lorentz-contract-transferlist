{-# OPTIONS -Wno-orphans #-}

module Michelson.Typed.Value.Missing () where

import Data.Function (id)

import Lorentz hiding (checkSignature, get)
import Michelson.Typed.Annotation
import Michelson.Typed.Scope
import Michelson.Typed.T
import Michelson.Typed.Value
import Michelson.Typed.Instr

instance IsoValue (Value' Instr t) where
  type ToT (Value' Instr t) = t
  toVal = id
  fromVal = id

instance IsoCValue (Value ('Tc ct)) where
  type ToCT (Value ('Tc ct)) = ct
  toCVal (VC xs) = xs
  fromCVal = VC

-- | No `Notes`
instance SingI t => HasTypeAnn (Value' Instr t) where
  getTypeAnn = starNotes

