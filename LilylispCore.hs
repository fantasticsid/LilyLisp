module LilylispCore where

import qualified Data.Map as Map (Map, fromList, lookup)
import Data.Maybe
import Data.IORef

-- value and expressions

data LispVal = String String
             | Int Int
             | Float Float
             | Bool Bool
             | QuotedSymbol String
             | Symbol String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Lambda String [String] [LispExpr] LispEnv -- procName params bodys environment
             | Primitive String ([LispVal] -> LispVal)

data LispEnv = LispEnv (IORef (Map.Map String LispVal)) LispEnv
             | TopEnv

data LispExpr = Value LispVal
              | If LispExpr LispExpr LispExpr
              | Define LispVal LispExpr
              | Assignment String LispExpr
              | Sequence [LispExpr]
              | ListExpr [LispExpr]
              deriving (Show)

showVal :: LispVal -> String
showVal (String s) = show s
showVal (Int i) = show i
showVal (Float f) = show f
showVal (Bool b) = show b
showVal (QuotedSymbol s) = "'" ++ show s
showVal (Symbol s) = show s
showVal (List vals) = show (map showVal vals)
showVal (DottedList vals val) = show (map showVal vals) ++ " . " ++ showVal val
showVal (Lambda procName params _ _) = "Lambda " ++ procName ++ " with parameters: " ++ show(params)
showVal (Primitive procName _) = "Primitive " ++ procName

instance Show LispVal where
  show = showVal

-- primitives

primitiveMul2 :: LispVal -> LispVal -> LispVal
primitiveMul2 (Int a) (Int b) = Int (a * b)
primitiveMul2 (Int a) (Float b) = Float $ (fromIntegral a) * b
primitiveMul2 (Float a) (Int b) = Float $ a * (fromIntegral b)
primitiveMul2 (Float a) (Float b) = Float (a * b)

primitiveMul :: [LispVal] -> LispVal
primitiveMul [] = Int 1
primitiveMul (x:xs) = primitiveMul2 x (primitiveMul xs)

primitiveDiv2 :: LispVal -> LispVal -> LispVal
primitiveDiv2 (Int a) (Int b) = Int $ quot a b
primitiveDiv2 (Int a) (Float b) = Float $ (fromIntegral a) / b
primitiveDiv2 (Float a) (Int b) = Float $ a / (fromIntegral b)
primitiveDiv2 (Float a) (Float b) = Float (a / b)

primitiveDiv :: [LispVal] -> LispVal
primitiveDiv [] = Int 1
primitiveDiv (x:xs) = primitiveDiv2 x (primitiveDiv xs)

primitiveAdd2 :: LispVal -> LispVal -> LispVal
primitiveAdd2 (Int a) (Int b) = Int (a + b)
primitiveAdd2 (Int a) (Float b) = Float $ (fromIntegral a) + b
primitiveAdd2 (Float a) (Int b) = Float $ a + (fromIntegral b)
primitiveAdd2 (Float a) (Float b) = Float $ (a + b)

primitiveAdd :: [LispVal] -> LispVal
primitiveAdd [] = Int 0
primitiveAdd (x:xs) = primitiveAdd2 x (primitiveAdd xs)

primitiveMin2 :: LispVal -> LispVal -> LispVal
primitiveMin2 (Int a) (Int b) = Int (a - b)
primitiveMin2 (Int a) (Float b) = Float $ (fromIntegral a) - b
primitiveMin2 (Float a) (Int b) = Float $ a - (fromIntegral b)
primitiveMin2 (Float a) (Float b) = Float $ (a - b)

primitiveMin :: [LispVal] -> LispVal
primitiveMin [] = Int 0
primitiveMin [x] = primitiveMin2 (Int 0) x
primitiveMin (x:xs) = primitiveMin2 x (primitiveAdd xs)

-- eval

initialEnv :: Map.Map String LispVal
initialEnv = Map.fromList [("*", Primitive "*" primitiveMul),
                       ("/", Primitive "/" primitiveDiv),
                       ("+", Primitive "+" primitiveAdd),
                       ("-", Primitive "-" primitiveMin)]

lookupProc :: LispVal -> LispEnv -> IO LispVal
lookupProc proc@(Symbol procName) (LispEnv mapref parentEnv) = do m <- readIORef mapref
                                                                  case Map.lookup procName m of
                                                                    Nothing -> lookupProc proc parentEnv
                                                                    Just p -> return p
lookupProc (Symbol procName) TopEnv = error $ "Procedure " ++ procName ++ " not found"
lookupProc proc env = return proc -- procedure value

eval :: LispExpr -> LispEnv -> IO LispVal
eval (Value val) env = return val
eval (ListExpr (procExpr:params)) env = do procKey <- eval procExpr env
                                           proc <- lookupProc procKey env
                                           params <- mapM (\param -> eval param env) params
                                           return $ apply proc params


-- apply (lookupProc (eval proc)) (map eval params)

apply :: LispVal -> [LispVal] -> LispVal
apply (Primitive _ fn) params = fn params
apply (Lambda procName fparams bodys env) aparams= String "undefined"
apply _ _ = String "undefined"
