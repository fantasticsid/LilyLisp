import Control.Monad

import LilylispParser
import LilylispCore

import Data.IORef

main :: IO ()
main = do m <- newIORef initialEnv
          let rootEnv = LispEnv m TopEnv
          forever $ do
            parseResult <- liftM parseLisp getLine
            case parseResult of
              Left error -> print error
              Right exprs -> do result <- mapM (\expr -> eval expr rootEnv) exprs
                                print result
