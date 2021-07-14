module Language.Sexp.Parser where

import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec
import Data.Sexp
import Control.Arrow (left)

-- parser utilities

type Parser = Parsec String ()

identStart :: Parser Char
identStart = letter <|> oneOf "`',=!@#$%^&*:<>./?~"

lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ emptyDef
    { P.commentStart = "#|"
    , P.commentEnd = "|#"
    , P.commentLine = ";"
    , P.nestedComments = True
    , P.identStart = identStart
    , P.identLetter = identStart <|> digit <|> oneOf "-"
    , P.caseSensitive = True
    }

parens :: Parser a -> Parser a
parens = P.parens lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

parensOrBrackets :: Parser a -> Parser a
parensOrBrackets p = parens p <|> brackets p

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

identifier :: Parser String
identifier = P.identifier lexer

integer :: Parser Integer
integer = P.integer lexer

-- parser

pList :: Parser Sexp
pList = SList <$> parensOrBrackets (many pSexp) <?> "list"

pString :: Parser Sexp
pString = SString <$> stringLiteral <?> "string literal"

pInteger :: Parser Sexp
pInteger = SInteger <$> integer <?> "integer literal"

pAtomic :: Parser Sexp
pAtomic = SAtom <$> identifier <?> "atom"

pSexp :: Parser Sexp
pSexp = choice [pList, pString, pInteger, pAtomic] <?> "s-expression"

parseSexp :: String -> Either String Sexp
parseSexp s = left show $ parse (spaces *> pSexp <* eof) "" s

parseSexpAt :: (Monad m, MonadFail m) => (String, Int, Int) -> String -> m Sexp
parseSexpAt (file, line, col) s =
    let p = do
            pos <- getPosition
            setPosition $
                flip setSourceName file $
                flip setSourceLine line $
                flip setSourceColumn col
                pos
            spaces *> pSexp <* eof
    in case runParser p () "" s of
        Left err -> fail (show err)
        Right e -> return e
