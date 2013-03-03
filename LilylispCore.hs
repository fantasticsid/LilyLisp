module LilylispCore where

import qualified Data.Map as Map (Map, fromList, lookup, insert)
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
             | Lambda [String] [LispExpr] LispEnv -- params bodys environment
             | Primitive String ([LispVal] -> LispEnv -> LispVal)

data LispEnv = LispEnv (IORef (Map.Map String LispVal)) LispEnv
             | TopEnv

data LispExpr = Value LispVal
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
showVal (Lambda params _ _) = "Lambda " ++ " with parameters: " ++ show(params)
showVal (Primitive procName _) = "Primitive " ++ procName

instance Show LispVal where
  show = showVal

-- primitives

primitiveMul2 :: LispVal -> LispVal -> LispVal
primitiveMul2 (Int a) (Int b) = Int (a * b)
primitiveMul2 (Int a) (Float b) = Float $ (fromIntegral a) * b
primitiveMul2 (Float a) (Int b) = Float $ a * (fromIntegral b)
primitiveMul2 (Float a) (Float b) = Float (a * b)

primitiveMul :: [LispVal] -> LispEnv -> LispVal
primitiveMul [] env = Int 1
primitiveMul (x:xs) env = primitiveMul2 x (primitiveMul xs env)

primitiveDiv2 :: LispVal -> LispVal -> LispVal
primitiveDiv2 (Int a) (Int b) = Int $ quot a b
primitiveDiv2 (Int a) (Float b) = Float $ (fromIntegral a) / b
primitiveDiv2 (Float a) (Int b) = Float $ a / (fromIntegral b)
primitiveDiv2 (Float a) (Float b) = Float (a / b)

primitiveDiv :: [LispVal] -> LispEnv -> LispVal
primitiveDiv [] env = Int 1
primitiveDiv (x:xs) env = primitiveDiv2 x (primitiveDiv xs env)

primitiveAdd2 :: LispVal -> LispVal -> LispVal
primitiveAdd2 (Int a) (Int b) = Int (a + b)
primitiveAdd2 (Int a) (Float b) = Float $ (fromIntegral a) + b
primitiveAdd2 (Float a) (Int b) = Float $ a + (fromIntegral b)
primitiveAdd2 (Float a) (Float b) = Float $ (a + b)

primitiveAdd :: [LispVal] -> LispEnv -> LispVal
primitiveAdd [] env = Int 0
primitiveAdd (x:xs) env = primitiveAdd2 x (primitiveAdd xs env)

primitiveMin2 :: LispVal -> LispVal -> LispVal
primitiveMin2 (Int a) (Int b) = Int (a - b)
primitiveMin2 (Int a) (Float b) = Float $ (fromIntegral a) - b
primitiveMin2 (Float a) (Int b) = Float $ a - (fromIntegral b)
primitiveMin2 (Float a) (Float b) = Float $ (a - b)

primitiveMin :: [LispVal] -> LispEnv -> LispVal
primitiveMin [] env = Int 0
primitiveMin [x] env = primitiveMin2 (Int 0) x
primitiveMin (x:xs) env = primitiveMin2 x (primitiveAdd xs env)

primitiveEq :: [LispVal] -> LispEnv -> LispVal
primitiveEq [(String s1), (String s2)] env = Bool $ s1 == s2
primitiveEq [(Int i1), (Int i2)] env = Bool $ i1 == i2
primitiveEq [(Float f1), (Float f2)] env = Bool $ f1 == f2
primitiveEq [(QuotedSymbol s1), (QuotedSymbol s2)] env = Bool $ s1 == s2
primitiveEq [(Symbol s1), (Symbol s2)] env = Bool $ s1 == s2

-- eval

initialEnv :: Map.Map String LispVal
initialEnv = Map.fromList [("*", Primitive "*" primitiveMul),
                       ("/", Primitive "/" primitiveDiv),
                       ("+", Primitive "+" primitiveAdd),
                       ("-", Primitive "-" primitiveMin),
                       ("=", Primitive "=" primitiveEq)]

eval :: LispExpr -> LispEnv -> IO LispVal
eval (ListExpr ((Value (Symbol "lambda")):(ListExpr params):body)) env = return $ Lambda (map (\(Value (Symbol s)) -> s) params) body env
eval (ListExpr ((Value (Symbol "define")):(Value (Symbol varName)):[expr])) env@(LispEnv mapref _) = do val <- eval expr env
                                                                                                        modifyIORef mapref (\m -> Map.insert varName val m)
                                                                                                        return val
eval (ListExpr [(Value (Symbol "if")), condition, branchA, branchB]) env = do boolVal <- eval condition env
                                                                              case boolVal of
                                                                                Bool True -> eval branchA env
                                                                                Bool False -> eval branchB env
eval (Value (Symbol atomName)) TopEnv = error $ "No binding for " ++ atomName
eval val@(Value (Symbol atomName)) (LispEnv mapref parentEnv) = do m <- readIORef mapref
                                                                   case Map.lookup atomName m of
                                                                     Nothing -> eval val parentEnv
                                                                     Just value -> return value
eval (Value val) env = return val
eval (ListExpr (procExpr:params)) env = do proc <- eval procExpr env
                                           params <- mapM (\param -> eval param env) params
                                           apply proc params env

-- apply (lookupProc (eval proc)) (map eval params)

apply :: LispVal -> [LispVal] -> LispEnv -> IO LispVal
apply (Primitive _ fn) params env = return $ fn params env
apply (Lambda fparams body env) aparams _ = do extmapref <- newIORef $ Map.fromList $ zip fparams aparams
                                               let extenv = LispEnv extmapref env
                                                 in fmap last $ mapM (\expr -> eval expr extenv) body

--apply _ _ = String "undefined"
