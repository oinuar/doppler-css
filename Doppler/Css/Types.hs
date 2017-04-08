{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.Css.Types where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type CssSelector = String
type CssPropertyName = String
newtype CssProperty = CssProperty (CssPropertyName, [CssPropertyValue]) deriving (Show, Eq)

data Css =
     Block [CssSelector] [CssProperty]
   -- ^ CSS block that contains style properties.
   | Import String
   -- ^ CSS import.
   | MediaBlock [CssSelector] [Css]
   -- ^ CSS media block that contains other definitions.
   deriving (Show, Eq)

data CssPropertyValue =
     Value String
   -- ^ Ordinary textual value.
   | InterpolationValue (Q Exp)
   -- ^ Interpolated value that contains Haskell expression.

-- Class to indicate all possible CSS properties.
class IsCssProperty a where
   -- Converts a type to CSS property value.
   formatProperty :: a ->
                     -- ^ Type to convert.
                     CssPropertyValue
                     -- ^ CSS property value.


instance Monoid CssPropertyValue where
   mempty =
      Value mempty

   mappend (Value lhs) (Value rhs) =
      Value $ lhs `mappend` rhs

   mappend lhs@(InterpolationValue _) (InterpolationValue _) =
      Value $ show lhs

   mappend lhs rhs =
      Value $ show lhs `mappend` show rhs


instance IsCssProperty CssPropertyValue where
   formatProperty = id

instance IsCssProperty Bool where
   formatProperty = formatProperty . show

instance IsCssProperty Double where
   formatProperty = formatProperty . show

instance IsCssProperty Float where
   formatProperty = formatProperty . show

instance IsCssProperty Int where
   formatProperty = formatProperty . show

instance IsCssProperty Char where
   formatProperty = Value . pure

instance IsCssProperty a => IsCssProperty [a] where
   formatProperty = mconcat . map formatProperty


instance Lift Css where
   lift (Block selector properties) =
      [| Block selector properties |]

   lift (Import url) =
      [| Import url |]

   lift (MediaBlock selector blocks) =
      [| MediaBlock selector blocks |]

instance Lift CssProperty where
   lift (CssProperty (key, value)) =
      [| CssProperty (key, value) |]

instance Show CssPropertyValue where
   show (Value content) =
      show content

   show (InterpolationValue _) =
      "${..}"

instance Eq CssPropertyValue where
   (==) (Value lhs) (Value rhs) =
      lhs == rhs

   (==) (InterpolationValue _) (InterpolationValue _) =
      True

   (==) _ _ =
      False

instance Lift CssPropertyValue where
   -- This comes directly from parser, no need to format.
   lift (Value content) =
      [| Value content |]

   -- This is evaluated Haskell syntax that parser has not seen, need to format.
   lift (InterpolationValue expression) =
      appE [| formatProperty |] expression
