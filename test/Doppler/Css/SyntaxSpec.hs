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
            [("name", [Value "value"])]

      it "parses property without semicolon" $
         parseCssPropertiesFromString "name:value" `shouldBe`
            [("name", [Value "value"])]

      it "parses property with space" $
         parseCssPropertiesFromString "name: value" `shouldBe`
            [("name", [Value "value"])]

      it "parses property name with hypen" $
         parseCssPropertiesFromString "do-or-die: value" `shouldBe`
            [("do-or-die", [Value "value"])]

      it "parses properties separated by semicolon" $
         parseCssPropertiesFromString "foo:value1; bar:value2" `shouldBe`
            [("foo", [Value "value1"]), ("bar", [Value "value2"])]

      it "parses property interpolation (pre)" $
         parseCssPropertiesFromString "font-size: ${size}px" `shouldBe`
            [("font-size", [InterpolationValue (lift ""), Value "px"])]

      it "parses property interpolation (post)" $
         parseCssPropertiesFromString "font-size: 12${unit}" `shouldBe`
            [("font-size", [Value "12", InterpolationValue (lift "")])]

      it "parses property interpolation" $
         parseCssPropertiesFromString "text-align: ${align}" `shouldBe`
            [("text-align", [InterpolationValue (lift "")])]

      it "parses property interpolation with semicolon" $
         parseCssPropertiesFromString "text-align: ${align};" `shouldBe`
            [("text-align", [InterpolationValue (lift "")])]

   describe "Blocks" $ do
      it "parses empty blocks" $
         parseCssFromString "selector{}" `shouldBe`
            [CssBlock "selector" []]

      it "parses empty blocks with space" $
         parseCssFromString "selector { }" `shouldBe`
            [CssBlock "selector" []]

      it "parses multiple empty blocks with spaces" $
         parseCssFromString "foo { } bar { }" `shouldBe`
            [CssBlock "foo" [], CssBlock "bar" []]

      it "parses blocks with one property and semicolon" $
         parseCssFromString "selector { text-align: center; }" `shouldBe`
            [CssBlock "selector" [("text-align", [Value "center"])]]

      it "parses multiple blocks with one property" $
         parseCssFromString "foo { text-align: center; } bar { font-size: 12px; }" `shouldBe`
            [CssBlock "foo" [("text-align", [Value "center"])], CssBlock "bar" [("font-size", [Value "12px"])]]

      it "parses blocks with multiple propertys" $
         parseCssFromString "selector { text-align: center; font-size: 12px; }" `shouldBe`
            [CssBlock "selector" [("text-align", [Value "center"]), ("font-size", [Value "12px"])]]

      it "parses blocks with property interpolation" $
         parseCssFromString "selector { text-align: ${align}; }" `shouldBe`
            [CssBlock "selector" [("text-align", [InterpolationValue (lift "")])]]
