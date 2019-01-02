module SimpleTyped.SimpleTypedSpec where

import Test.Hspec
import SimpleTyped.SimpleBool
import Data.Either (isRight, fromRight)

spec :: Spec
spec =
  describe "SimpleTyped" $ do
    describe "SimpleBool" $ do
        describe "parser" $ do
            it "true" $ do
                isRight $ parseTerm "true"

            it "false" $ do
                isRight $ parseTerm "false"

            it "if-then-else" $ do
                isRight $ parseTerm "if true then \\x:Bool.x else \\x:Bool.\\y:Bool.x"

            it "\\x: Bool.x" $ do
                isRight $ parseTerm "\\x: Bool.x"

            it "\\x: Bool.\\x: Bool.x" $ do
                isRight $ parseTerm "\\x: Bool.\\x: Bool.x"

        describe "type infer" $ do
            it "true: Bool" $ do 
                typeOf [] (fromRight (error "parse `true` error") $ parseTerm "true") `shouldBe` TyBool 

            it "false: Bool" $ do 
                typeOf [] (fromRight (error "parse `false` error") $ parseTerm "false") `shouldBe` TyBool 
            
            it "if true then true else false : Bool" $ do 
                typeOf [] (fromRight (error "parse `if true then true else false` error") $ parseTerm "if true then true else false") `shouldBe` TyBool 
            
            it "if true then \\x: Bool.x else \\x: Bool.x : Bool -> Bool" $ do 
                typeOf [] (fromRight (error "parse `if true then \\x: Bool.x else \\x: Bool.x` error") $ parseTerm "if true then \\x: Bool.x else \\x: Bool.x") `shouldBe` TyArr TyBool TyBool
            
            