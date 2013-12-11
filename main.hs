import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim as Prim
import Data.List

data HtSExp = Elem String [HtSExp] | Attr String String | Str String | Comment String deriving (Show, Read, Eq)

htSExp :: Parser HtSExp
htSExp = Prim.try element <|> Prim.try attr <|> Prim.try str <|> Prim.try comment

element :: Parser HtSExp
element = do
  char '('
  spaces
  element_ <- many alphaNum
  es <- many $ Prim.try (Prim.try spaces >> htSExp)
  spaces
  char ')'
  return $ Elem element_ es

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

escapedChars :: Parser Char
escapedChars = char '\\' >> oneOf "\\\"nrt" >>= (\x -> return $ case x of 
                                                                  '\\' -> x
                                                                  '"'  -> x
                                                                  'n'  -> '\n'
                                                                  'r'  -> '\r'
                                                                  't'  -> '\t'
                                                )

quote :: Parser String
quote = char '"' >> many (escapedChars <|> noneOf "\"\\") >>= (\x -> char '"' >> return x)

str :: Parser HtSExp
str = quote >>= (return . Str)

-- | (* Fill comments here *)
comment :: Parser HtSExp
comment = string "(*" >> manyTill anyChar (Prim.try (string "*)")) >>= (return . Comment)

readHtSExp :: String -> String
readHtSExp input = case parse element "HtSExp" input of
    Left err -> error $ "No match: " ++ show err
    Right val -> convertToHtml val

convertToHtml :: HtSExp -> String
convertToHtml (Elem e []) = "<" ++ e ++ " />"
convertToHtml (Elem "html" xs) = "<!doctype html>" ++ convertToHtml' (Elem "html" xs) 
convertToHtml e@(Elem _ _) = convertToHtml' e
convertToHtml (Str s) = s --"\"" ++ s ++ "\""
convertToHtml (Comment c) = "<!-- " ++ c ++ "-->"

convertToHtml' :: HtSExp -> String
convertToHtml' (Elem e xs) = case others == [] && e `notElem` elemlist of
                               True  -> "<" ++ e ++ concat (map flattenAttr attrs) ++ " />"
                               False -> "<" ++ e ++ concat (map flattenAttr attrs) ++ ">" ++ concat (map convertToHtml others) ++ "</" ++ e ++ ">"
    where (attrs, others) = partition isAttr xs
          flattenAttr (Attr att val) = " " ++ att ++ "=\"" ++ val ++ "\""

isAttr :: HtSExp -> Bool
isAttr (Attr _ _) = True
isAttr _ = False

elemlist :: [String]
elemlist = ["script"]

main :: IO ()
main = getContents >>= (putStrLn . readHtSExp)

