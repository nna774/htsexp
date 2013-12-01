import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim as Prim
import Text.Parsec.Token
import Text.ParserCombinators.Parsec
import Control.Monad

import Prelude hiding (elem)

data HtSExp = Elem String [HtSExp] | Attr String String | Str String | Comment String deriving (Show, Read, Eq)

htSExp :: Parser HtSExp
htSExp = Prim.try elem <|> Prim.try attr <|> Prim.try str <|> Prim.try comment

elem :: Parser HtSExp
elem = do
  char '('
  spaces
  element <- many alphaNum
--  spaces
--  skipMany1 space
  es <- many $ Prim.try (Prim.try spaces >> htSExp)
  spaces
  char ')'
  return $ Elem element es

attr :: Parser HtSExp
attr = do
  char '('
  spaces
  char '@'
  attri <- many alphaNum
  skipMany1 space
  value <- quote
  spaces
  char ')'
  return $ Attr attri value

quote :: Parser String
quote = char '"' >> manyTill anyChar (char '"') 

str :: Parser HtSExp
str = quote >>= (return . Str)

-- | (* Fill comments here *)
comment :: Parser HtSExp
comment = string "(*" >> manyTill anyChar (Prim.try (string "*)")) >>= (return . Comment)

readHtSExp :: String -> String
readHtSExp input = case parse elem "HtSExp" input of
    Left err -> "No match: " ++ show err
    Right val -> convertToHtml val

main :: IO ()
main = getContents >>= (putStrLn . readHtSExp)

convertToHtml :: HtSExp -> String
convertToHtml htSExp = undefined


