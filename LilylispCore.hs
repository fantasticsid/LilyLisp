module LilylispCore where

import Data.Map (Map)

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

data LispEnv = LispEnv (Map String LispVal)
               deriving (Show)

data LispExpr = Value LispVal
              | If LispExpr LispExpr LispExpr
              | Define LispVal LispExpr
              | Assignment String LispExpr
              | Sequence [LispExpr]
              | ListExpr LispExpr [LispExpr]
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

-- eval

lookupProc :: LispVal -> LispVal
lookupProc (Symbol "*") = Primitive "*" primitiveMul

eval :: LispExpr -> LispVal
eval (Value val) = val
eval (ListExpr proc params) = apply (lookupProc (eval proc)) (map eval params)

apply :: LispVal -> [LispVal] -> LispVal
apply (Primitive _ fn) params = fn params
apply (Lambda procName fparams bodys env) aparams= String "undefined"
apply _ _ = String "undefined"
