import LilylispParser
import LilylispCore

import Control.Monad
import Data.List

import Data.IORef
import System.IO

main :: IO ()
main = do m <- newIORef initialEnv
          let rootEnv = LispEnv m TopEnv
          forever $ do
            putStr "LilyLisp >> "
            hFlush stdout
            parseResult <- liftM parseLisp getLine
            case parseResult of
              Left error -> print error
              Right exprs -> do result <- mapM (\expr -> eval expr rootEnv) exprs
                                putStrLn $ concat . intersperse ", " $ map show result
