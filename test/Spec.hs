
import Test.Sandwich
import Relude


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $ do
  it "works" $ pending
