{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.CSS.Types (
   Property (..), PropertyName, Value (..)
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type PropertyName = String

data Value =
     StringValue String
   | InterpolationValue String
   deriving (Eq, Show)

newtype Property = Property {
   getCSSProperty :: (PropertyName, [Value])
} deriving (Eq, Show)

instance Lift Value where
   lift (StringValue content) =
      [|StringValue content|]

   lift (InterpolationValue content) =
      appE [|StringValue|] $ runInterpolation content

instance Lift Property where
   lift (Property value) =
      [|Property value|]

runInterpolation :: String -> Q Exp
runInterpolation =
   foldl1 appE . map (varE . mkName) . words
