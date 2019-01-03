module SimpleTyped.SimpleTypedSpec where

import Test.Hspec
import SimpleTyped.SimpleBool
import Data.Either (isRight, fromRight)

spec :: Spec
spec =
  describe "SimpleTyped" $ do
    describe "SimpleBool" $ do
        describe "parser" $ do
            it "nat" $ do 
                isRight $ parseTerm "1"

            it "float" $ do 
                isRight $ parseTerm "1.2"

            it "string" $ do 
                isRight $ parseTerm "\"12\""

            it "true" $ do
                isRight $ parseTerm "true"

            it "false" $ do
                isRight $ parseTerm "false"

            it "unit" $ do 
                isRight $ parseTerm "unit"

            it "if-then-else" $ do
                isRight $ parseTerm "if true then \\x:Bool.x else \\x:Bool.\\y:Bool.x"

            it "\\x: Bool.x" $ do
                isRight $ parseTerm "\\x: Bool.x"

            it "\\x: Unit.x" $ do 
                isRight $ parseTerm "\\x: Unit.x"

            it "\\x: Bool.\\x: Bool.x" $ do
                isRight $ parseTerm "\\x: Bool.\\x: Bool.x"

            it "\\x: Bool. \\y:Unit.y" $ do
                isRight $ parseTerm "\\x: Bool.\\x: Unit.x"

        describe "type infer" $ do
            it "true: Bool" $ do 
                typeOf [] (fromRight (error "parse `true` error") $ parseTerm "true") `shouldBe` TyBool 

            it "false: Bool" $ do 
                typeOf [] (fromRight (error "parse `false` error") $ parseTerm "false") `shouldBe` TyBool 
            
            it "unit: Unit" $ do 
                typeOf [] (fromRight (error "parse `unit` error") $ parseTerm "unit") `shouldBe` TyUnit

            it "if true then true else false : Bool" $ do 
                typeOf [] (fromRight (error "parse `if true then true else false` error") $ parseTerm "if true then true else false") `shouldBe` TyBool 
            
            it "\\x: Bool -> Bool.x" $ do 
                typeOf [] (fromRight (error "parse `\\x: Bool -> Bool.x`") $ parseTerm "\\x: Bool -> Bool.x") `shouldBe` TyArr (TyArr TyBool TyBool) (TyArr TyBool TyBool)

            it "if true then \\x: Bool.x else \\x: Bool.x : Bool -> Bool" $ do 
                typeOf [] (fromRight (error "parse `if true then \\x: Bool.x else \\x: Bool.x` error") $ parseTerm "if true then \\x: Bool.x else \\x: Bool.x") `shouldBe` TyArr TyBool TyBool
            
            it "\\x: Unit. true" $ do 
                typeOf [] (fromRight (error "parse `\\x: Unit. true` error") $ parseTerm "\\x: Unit. true") `shouldBe` TyArr TyUnit TyBool

            it "\\x: Unit -> Unit. x" $ do 
                typeOf [] (fromRight (error "parse `\\x: Unit -> Unit. x`") $ parseTerm "\\x: Unit -> Unit. x") `shouldBe` TyArr (TyArr TyUnit TyUnit) (TyArr TyUnit TyUnit)

            it "\\x: String.\\y: Nat. unit" $ do 
                typeOf [] (fromRight (error "parse `\\x: String.\\y: Nat. unit`") $ parseTerm "\\x: String.\\y: Nat. unit") `shouldBe` TyArr TyString (TyArr TyNat TyUnit)

            it "\\x: Nat. 1.2" $ do 
                typeOf [] (fromRight (error "parse `\\x: Nat. 1.2`") $ parseTerm "\\x: Nat. 1.2") `shouldBe` TyArr TyNat TyFloat
