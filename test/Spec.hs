-- file Spec.hs
import Test.Hspec
import Untyped.Lambda


main :: IO ()
main = hspec $ do
  describe "Untyped" $ do
    describe "untyped lambda calculus" $ do 
        it "λx.x" $ do 
            parseTerm "lambda x.x" `shouldBe` Right (Abs (Var 0))
        
        it "λx.λx.x" $ do 
            parseTerm "lambda x.lambda x.x" `shouldBe` Right (Abs (Abs (Var 0)))

        it "λx.λy.x" $ do 
            parseTerm "lambda x.lambda y.x" `shouldBe` Right (Abs (Abs (Var 1)))

        it "λz.λy.λx.z" $ do 
            parseTerm "lambda z.lambda y.lambda x.z" `shouldBe` Right (Abs (Abs (Abs (Var 2))))
     
        it "(λx.x λy.y)" $ do 
            parseTerm "(lambda x.x lambda y.y)" `shouldBe` Right (App (Abs (Var 0)) (Abs (Var 0)))
        
        it "λx.λy.λz.((x z) (y z)) " $ do 
            parseTerm "lambda x.lambda y.lambda z.((x z) (y z)) " `shouldBe` Right (Abs (Abs (Abs (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0))))))

