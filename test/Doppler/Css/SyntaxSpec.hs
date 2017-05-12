--{-# LANGUAGE QuasiQuotes #-}

module Doppler.Css.SyntaxSpec where

import Test.Hspec
import Doppler.Css.Types
import Doppler.Css.Syntax
import Language.Haskell.TH.Syntax (lift)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "Properties" $ do
      it "parses property" $
         parseCssPropertiesFromString "name:value;" `shouldBe`
            [CssProperty ("name", [Value "value"])]

      it "parses property without semicolon" $
         parseCssPropertiesFromString "name:value" `shouldBe`
            [CssProperty ("name", [Value "value"])]

      it "parses property with space" $
         parseCssPropertiesFromString "name: value" `shouldBe`
            [CssProperty ("name", [Value "value"])]

      it "parses property name with hypen" $
         parseCssPropertiesFromString "do-or-die: value" `shouldBe`
            [CssProperty ("do-or-die", [Value "value"])]

      it "parses properties separated by semicolon" $
         parseCssPropertiesFromString "foo:value1; bar:value2" `shouldBe`
            [CssProperty ("foo", [Value "value1"]), CssProperty ("bar", [Value "value2"])]

      it "parses property interpolation (pre)" $
         parseCssPropertiesFromString "font-size: ${size}px" `shouldBe`
            [CssProperty ("font-size", [InterpolationValue (lift ""), Value "px"])]

      it "parses property interpolation (post)" $
         parseCssPropertiesFromString "font-size: 12${unit}" `shouldBe`
            [CssProperty ("font-size", [Value "12", InterpolationValue (lift "")])]

      it "parses property interpolation" $
         parseCssPropertiesFromString "text-align: ${align}" `shouldBe`
            [CssProperty ("text-align", [InterpolationValue (lift "")])]

      it "parses property interpolation with semicolon" $
         parseCssPropertiesFromString "text-align: ${align};" `shouldBe`
            [CssProperty ("text-align", [InterpolationValue (lift "")])]

   describe "Blocks" $ do
      it "parses empty block" $
         parseCssFromString "selector{}" `shouldBe`
            [Block ["selector"] []]

      it "parses empty block with multiple selectors" $
         parseCssFromString "selector1,selector2{}" `shouldBe`
            [Block ["selector1", ",", "selector2"] []]

      it "parses empty block with space" $
         parseCssFromString "selector { }" `shouldBe`
            [Block ["selector"] []]

      it "parses empty block with space and multiple selectors" $
         parseCssFromString "selector1, selector2 { }" `shouldBe`
            [Block ["selector1", ",", "selector2"] []]

      it "parses empty block with spaces between and multiple selectors" $
         parseCssFromString "selector1 , selector2 { }" `shouldBe`
            [Block ["selector1", ",", "selector2"] []]

      it "parses class selectors" $
         parseCssFromString "selector.class {}" `shouldBe`
            [Block ["selector.class"] []]

      it "parses pseudo hover selectors" $
         parseCssFromString "selector:hover {}" `shouldBe`
            [Block ["selector:hover"] []]

      it "parses pseudo nth-child(1) selector" $
         parseCssFromString "selector:nth-child(1) {}" `shouldBe`
            [Block ["selector:nth-child(1)"] []]

      it "parses global class selectors" $
         parseCssFromString ".class {}" `shouldBe`
            [Block [".class"] []]

      it "parses child selectors" $
         parseCssFromString "selector > children {}" `shouldBe`
            [Block ["selector", ">", "children"] []]

      it "parses selectors with children" $
         parseCssFromString "selector children {}" `shouldBe`
            [Block ["selector", " ", "children"] []]

      it "parses multiple empty blocks with spaces" $
         parseCssFromString "foo { } bar { }" `shouldBe`
            [Block ["foo"] [], Block ["bar"] []]

      it "parses blocks with one property and semicolon" $
         parseCssFromString "selector { text-align: center; }" `shouldBe`
            [Block ["selector"] [CssProperty ("text-align", [Value "center"])]]

      it "parses multiple blocks with one property" $
         parseCssFromString "foo { text-align: center; } bar { font-size: 12px; }" `shouldBe`
            [Block ["foo"] [CssProperty ("text-align", [Value "center"])], Block ["bar"] [CssProperty ("font-size", [Value "12px"])]]

      it "parses blocks with multiple propertys" $
         parseCssFromString "selector { text-align: center; font-size: 12px; }" `shouldBe`
            [Block ["selector"] [CssProperty ("text-align", [Value "center"]), CssProperty ("font-size", [Value "12px"])]]

      it "parses blocks with property interpolation" $
         parseCssFromString "selector { text-align: ${align}; }" `shouldBe`
            [Block ["selector"] [CssProperty ("text-align", [InterpolationValue (lift "")])]]

   describe "Media blocks" $ do
      it "parses empty blocks" $
         parseCssFromString "@media print{}" `shouldBe`
            [SpecialBlock "media" ["print"] []]

      it "parses empty blocks with space" $
         parseCssFromString "@media print { }" `shouldBe`
            [SpecialBlock "media" ["print"] []]

      it "parses media blocks containing other empty blocks" $
         parseCssFromString "@media print { foo {} }" `shouldBe`
            [SpecialBlock "media" ["print"] [Block ["foo"] []]]

      it "parses media blocks inside and outside" $
         parseCssFromString "foo {} @media print { foo {} bar {} } bar {}" `shouldBe`
            [Block ["foo"] [], SpecialBlock "media" ["print"] [Block ["foo"] [], Block ["bar"] []], Block ["bar"] []]

      it "parses media blocks containing a block with style properties" $
         parseCssFromString "@media print { foo { text-align: center; } }" `shouldBe`
            [SpecialBlock "media" ["print"] [Block ["foo"] [CssProperty ("text-align", [Value "center"])]]]

      it "parses media blocks containing many blocks with style properties" $
         parseCssFromString "@media print { foo { text-align: center; } bar { text-align: right; } }" `shouldBe`
            [SpecialBlock "media" ["print"] [Block ["foo"] [CssProperty ("text-align", [Value "center"])], Block ["bar"] [CssProperty ("text-align", [Value "right"])]]]

   describe "Keyframes blocks" $ do
      it "parses empty keyframes block" $
         parseCssFromString "@keyframes animation{}" `shouldBe`
            [SpecialBlock "keyframes" ["animation"] []]

      it "parses empty keyframes block with space" $
         parseCssFromString "@keyframes animation { }" `shouldBe`
            [SpecialBlock "keyframes" ["animation"] []]

      it "parses keyframes block containing empty block" $
         parseCssFromString "@keyframes animation { 100% { } }" `shouldBe`
            [SpecialBlock "keyframes" ["animation"] [Block ["100%"] []]]

      it "parses keyframes block containing empty blocks" $
         parseCssFromString "@keyframes animation { 0% {} 100% {} }" `shouldBe`
            [SpecialBlock "keyframes" ["animation"] [Block ["0%"] [], Block ["100%"] []]]

      it "parses keyframes block containing empty blocks with magic string" $
         parseCssFromString "@keyframes animation { from {} to {} }" `shouldBe`
            [SpecialBlock "keyframes" ["animation"] [Block ["from"] [], Block ["to"] []]]

      it "parses keyframes block containing blocks" $
         parseCssFromString "@keyframes animation { 0% { margin-left: 4rem; } 100% { margin-left: 0; } }" `shouldBe`
            [SpecialBlock "keyframes" ["animation"] [Block ["0%"] [CssProperty ("margin-left", [Value "4rem"])], Block ["100%"] [CssProperty ("margin-left", [Value "0"])]]]

   describe "Imports" $ do
      it "parses double quoted import" $
         parseCssFromString "@import \"http://only.styles.org\";" `shouldBe`
            [Import "http://only.styles.org"]

      it "parses single quoted import" $
         parseCssFromString "@import \"http://only.styles.org\";" `shouldBe`
            [Import "http://only.styles.org"]
