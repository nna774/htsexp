import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim as Prim
import Data.List
import System.Environment
import System.Console.GetOpt
import System.IO

version :: String
version = "1.0"

versionString :: String
versionString = "This is HtSExp version " ++ version ++ " by nonamea774@nnn77<nonamea774@nna774.net>."

data HtSExp = Elem String [HtSExp] | Attr String String | Str String | Comment String deriving (Show, Read, Eq)

data Options = Help | OutPut String | Version | License deriving (Eq, Show, Read)

options :: [OptDescr Options]
options = [ Option ['h','?'] ["help"] (NoArg Help) "display this help."
          , Option ['o'] ["output"] (ReqArg (OutPut . read) "OUTPUT")  "output file name."
          , Option ['v'] ["version"] (NoArg Version) "display version of this program."
          , Option [] ["license"] (NoArg License) "show license."
          ]

isOutput :: Options -> Bool
isOutput (OutPut _) = True
isOutput _ = False

unOutput :: Options -> String
unOutput (OutPut o) = o
unOutput _ = error "unOutput error"

progName :: String
progName = "htsexpr"

getOpts :: [String] -> IO ([Options], [String])
getOpts args =
  case getOpt Permute options args of
    (o, _, _) | Help `elem` o -> ioError (userError (usageInfo header options))
    (o, _, _) | License `elem` o -> error gpl3
    (o, _, _) | Version `elem` o -> error versionString
    (o, n, []) -> return (o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: " ++ progName ++ " [-o OUTPUT] "


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
readHtSExp input = case parse (element >>= (\x -> space >> eof >> return x) ) "HtSExp" input of
    Left err -> error $ "No match: " ++ show err
    Right val -> convertToHtml val

convertToHtml :: HtSExp -> String
convertToHtml (Elem e []) = "<" ++ e ++ " />"
convertToHtml (Elem "html" xs) = "<!doctype html>" ++ convertToHtml' (Elem "html" xs) 
convertToHtml e@(Elem _ _) = convertToHtml' e
convertToHtml (Str s) = s --"\"" ++ s ++ "\""
convertToHtml (Comment c) = "<!-- " ++ c ++ "-->"

convertToHtml' :: HtSExp -> String
convertToHtml' (Elem e xs) = case null others && e `notElem` elemlist of
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
main = do
  (opts, args) <- getOpts =<< getArgs
  if null args
  then getContents >>= (putStrLn . readHtSExp)
  else do
      let filename = args !! 0 
      readFile filename
      let outputFilename = maybe (if isSuffixOf filename ".htsexp" then "" else "") id (find isOutput opts >>= return . unOutput)
      return ()



gpl3 :: String
gpl3 = "HtSExp -- Convert S-Expression to Html\n" ++
    "Copyright (C) 2013-2014  NoNameA774@nnn77<nonamea7.7.4@gmail.com>\n\n" ++

    "This program is free software: you can redistribute it and/or modify\n" ++
    "it under the terms of the GNU General Public License as published by\n" ++
    "the Free Software Foundation, either version 3 of the License, or\n" ++
    "any later version.\n\n" ++

    "This program is distributed in the hope that it will be useful,\n" ++ 
    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n" ++
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" ++ 
    "GNU General Public License for more details.\n\n" ++ 

    "You should have received a copy of the GNU General Public License\n" ++
    "along with this program.  If not, see <http://www.gnu.org/licenses/>.\n"
