{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (Alternative(..))
import Control.Monad (void, when)
import Control.Monad.Except (MonadError(..))
import Data.Char (isDigit, isSpace)
import Data.List (intercalate)

data Pos = Pos
  { posLine :: !Int
  , posColumn :: !Int
  } deriving (Eq, Ord, Show)

data ParseState = ParseState
  { statePos   :: !Pos
  , stateInput :: String
  } deriving (Show)

data ParseError = ParseError
  { errPos      :: !Pos
  , errMsg      :: [String]
  , errCtx      :: [String]
  } deriving (Eq, Show)

newtype Parser a = Parser
  { rParser :: ParseState -> Either ParseError (a, ParseState)
  }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (a, s') <- rParser p s
    return (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Right (a, s)
  (<*>) pf pa = Parser $ \s -> do
    (f, s') <- rParser pf s
    (a, s'') <- rParser pa s'
    return (f a, s'')

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \s -> do
    (a, s') <- rParser p s
    rParser (f a) s'

instance Alternative Parser where
  empty = Parser $ \s -> Left $ ParseError (statePos s) ["[-] Any value expected"] []
  p <|> q = Parser $ \s -> case rParser p s of
    Left err -> case rParser q s of
      Left err' -> Left $ mErr err err'
      success   -> success
    success -> success

mErr :: ParseError -> ParseError -> ParseError
mErr e1 e2
  | errPos e1 > errPos e2 = e1 { errMsg = errMsg e1 ++ errMsg e2 }
  | errPos e1 < errPos e2 = e2 { errMsg = errMsg e1 ++ errMsg e2 }
  | otherwise = e1
    { errMsg = errMsg e1 ++ errMsg e2
    , errCtx  = errCtx e1 ++ errCtx e2
    }

wContext :: String -> Parser a -> Parser a
wContext ctx p = Parser $ \s -> case rParser p s of
  Left err -> Left $ err { errCtx = ctx : errCtx err }
  success -> success

throwError :: String -> Parser a
throwError msg = Parser $ \s -> Left $ ParseError
  { errPos = statePos s
  , errMsg = [msg]
  , errCtx = []
  }

aPos :: Char -> ParseState -> ParseState
aPos '\n' s = ParseState
  { statePos = Pos { posLine = posLine (statePos s) + 1, posColumn = 1 }
  , stateInput = tail (stateInput s)
  }
aPos _ s = ParseState
  { statePos = Pos 
      { posLine = posLine (statePos s)
      , posColumn = posColumn (statePos s) + 1
      }
  , stateInput = tail (stateInput s)
  }

spaces :: Parser ()
spaces = void $ many (satisfy isSpace)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

char :: Char -> Parser Char
char c = lexeme $ Parser $ \s -> case stateInput s of
  (x:xs) | x == c -> Right (c, aPos c s)
  _ -> Left $ ParseError
    { errPos = statePos s
    , errMsg = ["Expected '" ++ [c] ++ "'"]
    , errCtx = []
    }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case stateInput s of
  (x:xs) | p x -> Right (x, aPos x s)
  _ -> Left $ ParseError
    { errPos = statePos s
    , errMsg = ["unexpected character"]
    , errCtx = []
    }

integer :: Parser Int
integer = wContext "in an integer" $ do
  digits <- some (satisfy isDigit)
  spaces
  let n = read digits
  return n

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  _ <- open
  x <- p
  _ <- close
  spaces
  return x

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Lit Int
  deriving (Show)

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = wContext "in a factor" $
  (Lit <$> integer)
  <|> between (char '(') (char ')') expr

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (char '+' *> pure Add) <|> (char '-' *> pure Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = (char '*' *> pure Mul) <|> (char '/' *> pure Div)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do
      f <- op
      y <- p
      rest (f x y)
      <|> pure x

parse :: String -> Parser a -> Either String a
parse input p = case rParser p (ParseState (Pos 1 1) input) of
  Left err -> Left $ formatErr err
  Right (a, _) -> Right a

formatErr :: ParseError -> String
formatErr err = unlines
  [ "Error in line " ++ show (posLine pos) ++ ", column " ++ show (posColumn pos)
  , "Context:"
  , if null (errCtx err)
      then "  (any context)"
      else intercalate "\n" (map ("  - " ++) (reverse (errCtx err)))
  , "Messages:"
  , intercalate "\n" (map ("  - " ++) (errMsg err))
  ]
  where pos = errPos err

main :: IO ()
main = do
  
  let input = "-- Change Here --"

  case parse input expr of
    Left err -> putStrLn err
    Right e -> print e
