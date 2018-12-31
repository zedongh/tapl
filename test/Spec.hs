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

    describe "shift" $ do 
        it "shift 2 on λ.λ.(1 (0 2)) => λ.λ.(1 (0 4))" $ do 
            shift 2 (Abs (Abs (App (Var 1) (App (Var 0) (Var 2))))) `shouldBe` (Abs (Abs (App (Var 1) (App (Var 0) (Var 4)))))
            
        it "shift 2 on λ.λ.(0 (1 (λ.(0 (1 2))))) => λ.λ.(0 (1 (λ.(0 (1 2)))))" $ do 
            shift 2 (Abs (Abs (App (Var 0) (App (Var 1) (Abs (App (Var 0) (App (Var 1) (Var 2)))))))) `shouldBe` (Abs (Abs (App (Var 0) (App (Var 1) (Abs (App (Var 0) (App (Var 1) (Var 2))))))))

    describe "apply" $ do 
        it "(λx.x λx.x) => λx.x" $ do 
            eval mkContext (App (Abs (Var 0)) (Abs (Var 0))) `shouldBe` (Abs (Var 0))

        it "((λx.λy.x λx.x) λx.x)" $ do
            eval mkContext (App (App (Abs (Abs (Var 1))) (Abs (Var 0))) (Abs (Var 0))) `shouldBe` (Abs (Var 0))
    
        it "(λx.λy.x λx.x) => λy.λx.x" $ do 
            eval mkContext (App (Abs (Abs (Var 1))) (Abs (Var 0))) `shouldBe` (Abs (Abs (Var 0)))