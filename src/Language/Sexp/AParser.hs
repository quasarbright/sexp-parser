module Language.Sexp.AParser where

import Control.Arrow (left)
import Data.Sexp
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

-- parser utilities

type Parser = Parsec String ()

identStart :: Parser Char
identStart = letter <|> oneOf "`',=!@#$%^&*:<>./?~"

lexer :: P.TokenParser ()
lexer =
  P.makeTokenParser $
    emptyDef
      { P.commentStart = "#|",
        P.commentEnd = "|#",
        P.commentLine = ";",
        P.nestedComments = True,
        P.identStart = identStart,
        P.identLetter = identStart <|> digit <|> oneOf "-",
        P.caseSensitive = True
      }

parens :: Parser a -> Parser a
parens = P.parens lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

parensOrBrackets :: Parser a -> Parser a
parensOrBrackets p = parens p <|> brackets p

braces :: Parser a -> Parser a
braces = P.braces lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

identifier :: Parser String
identifier = P.identifier lexer

integer :: Parser Integer
integer = P.integer lexer

-- parser

pList :: Parser ASexp
pList = AList <$> parensOrBrackets (many pASexp) <?> "list"

pString :: Parser ASexp
pString = AString <$> stringLiteral <?> "string literal"

pInteger :: Parser ASexp
pInteger = AInteger <$> integer <?> "integer literal"

pAtomic :: Parser ASexp
pAtomic = AAtom <$> identifier <?> "atom"

pAnti :: Parser ASexp
pAnti = AntiSexp <$> braces (P.identifier haskell) -- anti quote for a haskell identifier

pASexp :: Parser ASexp
pASexp = choice [pList, pString, pInteger, pAnti, pAtomic] <?> "s-expression"

parseASexp :: String -> Either String ASexp
parseASexp s = left show $ parse (spaces *> pASexp <* eof) "" s

parseASexpAt :: (Monad m, MonadFail m) => (String, Int, Int) -> String -> m ASexp
parseASexpAt (file, line, col) s =
  let p = do
        pos <- getPosition
        setPosition $
          flip setSourceName file $
            flip setSourceLine line $
              flip
                setSourceColumn
                col
                pos
        spaces *> pASexp <* eof
   in case runParser p () "" s of
        Left err -> fail (show err)
        Right e -> return e
