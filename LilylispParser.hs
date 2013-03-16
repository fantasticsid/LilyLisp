module LilylispParser where

import LilylispCore

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

whitespaces :: Parser String
whitespaces = many (space <|> newline)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseInt :: Parser LispVal
parseInt = do digits <- many1 digit
              return $ Int (read digits :: Int)

parseFloat :: Parser LispVal
parseFloat = do part1 <- many1 digit
                char '.'
                part2 <- many1 digit
                return $ Float (read (part1 ++ "." ++ part2) :: Float)

parseBool :: Parser LispVal
parseBool = do char '#'
               v <- (char 't' <|> char 'f')
               return $ case v of
                 't' -> Bool True
                 'f' -> Bool False

parseQuotedSymbol :: Parser LispVal
parseQuotedSymbol = do char '\''
                       first <- letter <|> symbol
                       rest <- many (letter <|> digit <|> symbol)
                       return $ QuotedSymbol (first:rest)

parseSymbol :: Parser LispVal
parseSymbol = do chars <- many1 (letter <|> symbol)
                 return $ Symbol chars

parseAtom :: Parser LispExpr
parseAtom = do atom <- try parseString <|>
                       try parseFloat <|>
                       try parseInt <|>
                       try parseBool <|>
                       try parseQuotedSymbol <|>
                       try parseSymbol
               return $ Value atom

formExpr :: [LispExpr] -> LispExpr
formExpr ((Value (Symbol "set!")):(Value (Symbol varName)):xs) = Assignment varName (xs !! 0)
formExpr xs = ListExpr xs

parseForm :: Parser LispExpr
parseForm = do char '('
               whitespaces
               elems <- endBy1 (parseForm <|> parseAtom) (try $ many space)
               whitespaces
               char ')'
               return $ formExpr elems

parseExpr :: Parser LispExpr
parseExpr = parseAtom <|> parseForm

parseProg :: Parser [LispExpr]
parseProg = do whitespaces
               exprs <- endBy parseExpr whitespaces
               return exprs

parseLisp :: String -> Either ParseError [LispExpr]
parseLisp = parse parseProg "LilyLisp"
