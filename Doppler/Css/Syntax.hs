module Doppler.Css.Syntax (
   parseCss, parseCssProperty, parseCssFromString, parseCssPropertiesFromString
) where

import Doppler.Css.Types
import Text.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Parsec.String          (Parser)
import Language.Haskell.TH.Quote   (QuasiQuoter (..))

-- Parser for CSS block.
parseCss :: Parser Css
            -- ^ CSS block parser.
parseCss = many $ do
   selector <- many parseWhitespace *> parseSelector <* many parseWhitespace
   properties <- between (char '{' <* many parseWhitespace) (many parseWhitespace *> char '}') (many parseCssProperty)
   return $ CssBlock selector properties

-- Parser for CSS property.
parseCssProperty :: Parser CssProperty
                      -- ^ CSS property parser.
parseCssProperty = do
   name <- many parseWhitespace *> parsePropertyName <* many parseWhitespace
   _ <- char ':' <* many parseWhitespace
   value <- many parsePropertyValue
   _ <- optional $ char ';' <* many parseWhitespace
   return (name, value)

-- Parses CSS from string.
parseCssFromString :: String ->
                      -- ^ String to parse.
                      [CssBlock]
                      -- ^ Parsed CSS blocks.
parseCssFromString source =
   case parse parseCss source source of
      Right x -> x
      Left x -> error . show $ x

-- Parses CSS properties from string.
parseCssPropertiesFromString :: String ->
                                -- ^ String to parse.
                                [CssProperty]
                                -- ^ Parsed CSS properties.
parseCssPropertiesFromString source =
   case parse (many parseCssProperty) source source of
      Right x -> x
      Left x -> error . show $ x

parseSelector :: Parser CssSelector
parseSelector =
   many1 $ letter <|> digit <|> oneOf "-_"

parsePropertyName :: Parser CssPropertyName
parsePropertyName =
   many1 $ lower <|> char '-'

parsePropertyValue :: Parser CssPropertyValue
parsePropertyValue =
   interpolation <|> value
   where
      interpolation =
         InterpolationValue <$> parseInterpolationExpr

      value =
         Value <$> many1 (do
            x <- optionMaybe (lookAhead $ string "${")
            maybe (noneOf "\n\r;}") unexpected x)

parseWhitespace :: Parser Char
parseWhitespace =
   oneOf "\t\n\r "

mkQExp :: String -> Q Exp
mkQExp =
   foldl1 appE . map (varE . mkName) . words

parseInterpolationExpr :: Parser (Q Exp)
parseInterpolationExpr =
   mkQExp <$> between (string "${") (char '}') (many1 $ noneOf "}")
