import Control.Monad

import LilylispParser
import LilylispCore

main :: IO ()
main = do
  parseResult <- liftM parseLisp getContents
  print $ liftM (map eval) parseResult
