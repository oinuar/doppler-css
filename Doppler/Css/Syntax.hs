module Doppler.Css.Syntax (
   parseCss, parseCssProperty, parseCssFromString, parseCssPropertiesFromString
) where

import Doppler.Css.Types
import Text.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Parsec.String          (Parser)
import Language.Haskell.TH.Quote   (QuasiQuoter (..))

-- Parser for CSS definitions.
parseCss :: Parser Css
            -- ^ CSS parser.
parseCss =
   many parseWhitespace *> (try parseImport <|> parseMediaBlock <|> parseBlock)

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
                      [Css]
                      -- ^ Parsed CSS blocks.
parseCssFromString source =
   case parse (many parseCss) source source of
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

parseBlock :: Parser Css
parseBlock = do
   selector <- parseSelectors <* many parseWhitespace
   properties <- between (char '{' <* many parseWhitespace) (many parseWhitespace *> char '}') (many parseCssProperty) <* many parseWhitespace
   return $ Block selector properties

parseImport :: Parser Css
parseImport = do
   _ <- string "@import" <* many1 parseWhitespace
   url <- (    between (char '"') (char '"') (many1 $ noneOf "\"")
           <|> between (char '\'') (char '\'') (many1 $ noneOf "'")
           <|> many1 (noneOf ";")) <* many parseWhitespace
   _ <- char ';' <* many parseWhitespace
   return $ Import url

parseMediaBlock :: Parser Css
parseMediaBlock = do
   _ <- string "@media" <* many1 parseWhitespace
   selector <- parseSelectors <* many parseWhitespace
   blocks <- between (char '{' <* many parseWhitespace) (many parseWhitespace *> char '}') (many parseBlock) <* many parseWhitespace
   return $ MediaBlock selector blocks

parseSelectors :: Parser [CssSelector]
parseSelectors = do
   lhs <- many1 (letter <|> digit <|> oneOf "-_#.") <* many parseWhitespace
   rhs <- optionMaybe parseSelectors
   skipMany parseWhitespace
   maybe (operator lhs <|> pure [lhs]) (return . (:) lhs . (:) " ") rhs
   where
      operator lhs = do
         c <- oneOf ">," <* many parseWhitespace
         rhs <- parseSelectors
         skipMany parseWhitespace
         return $ lhs : pure c : rhs

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
            maybe (endOfLine <|> noneOf ";}") unexpected x)

parseWhitespace :: Parser Char
parseWhitespace =
   space <|> tab <|> endOfLine

mkQExp :: String -> Q Exp
mkQExp =
   foldl1 appE . map (varE . mkName) . words

parseInterpolationExpr :: Parser (Q Exp)
parseInterpolationExpr =
   mkQExp <$> between (string "${") (char '}') (many1 $ noneOf "}")
