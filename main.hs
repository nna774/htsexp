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

convertToHtml :: HtSExp -> String
convertToHtml (Elem e []) = "<" ++ e ++ " />"
convertToHtml (Elem "html" xs) = "<!doctype html>" ++ convertToHtml' (Elem "html" xs) 
convertToHtml e@(Elem _ _) = convertToHtml' e
convertToHtml (Str s) = s --"\"" ++ s ++ "\""
convertToHtml (Comment c) = "<!-- " ++ c ++ "-->"

-- <canvas id="hoge"></canvas> になる不具合
convertToHtml' :: HtSExp -> String
convertToHtml' (Elem e xs) = "<" ++ e ++ concat (map flattenAttr attrs) ++ ">" ++ concat (map convertToHtml others) ++ "</" ++ e ++ ">"
    where (attrs, others) = part isAttr xs
          flattenAttr (Attr attr val) = " " ++ attr ++ "=\"" ++ val ++ "\""

isAttr :: HtSExp -> Bool
isAttr (Attr _ _) = True
isAttr _ = False

-- やばそう
part :: (a -> Bool) -> [a] -> ([a],[a])
part f xs = (filter f xs, filter (not.f) xs)


main :: IO ()
main = getContents >>= (putStrLn . readHtSExp)

