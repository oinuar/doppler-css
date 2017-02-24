module Doppler.CSS.Syntax (
   parseProperties, style
) where

import Doppler.CSS.Types
import Text.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Parsec.String         (Parser)
import Language.Haskell.TH.Quote  (QuasiQuoter (..))


-- | Quasiquoter for CSS properties.
style :: QuasiQuoter
style = QuasiQuoter {
   quoteExp = compileStyle,
   quotePat = undefined,
   quoteType = undefined,
   quoteDec = undefined
}

compileStyle :: String -> Q Exp
compileStyle str =
   case parse parseProperties str str of
      Right x -> lift x
      Left err -> fail $ show err

parseProperties :: Parser [Property]
parseProperties =
   parseJunk *> parseProperty `sepEndBy1` delimiter
   where
      delimiter =
         char ';' <* parseJunk

parseProperty :: Parser Property
parseProperty = do
   key <- parsePropertyName
   _ <- lexeme $ char ':'
   value <- parseValue
   return $ Property (key, value)

parsePropertyName :: Parser PropertyName
parsePropertyName =
   parsePropertyName' <?> "CSS property"
   where
      parsePropertyName' = lexeme $ do
         firstLetter <- letter
         restLetters <- many $ alphaNum <|> char '-'
         return $ firstLetter:restLetters

parseValue :: Parser [Value]
parseValue =
   parseValue' <?> "CSS property value"
   where
      parseValue' =
         interpolation <|> characters <|> pure []

      interpolation = do
         _ <- char '$'
         content <- parseInterpolationContent
         rest <- parseValue'
         return $ InterpolationValue content : rest

      characters = do
         content <- many1 (alphaNum <|> oneOf ".,-#%() ")
         rest <- parseValue'
         return $ StringValue content : rest

parseInterpolationContent :: Parser String
parseInterpolationContent =
   between (char '{') (char '}') characters
   where
      characters =
         many1 $ alphaNum <|> char ' '

parseJunk :: Parser String
parseJunk =
   many $ space <|> tab <|> endOfLine

lexeme :: Parser a -> Parser a
lexeme p =
   p <* many (space <|> tab)
