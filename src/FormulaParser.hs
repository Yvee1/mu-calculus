module FormulaParser (parseFormula) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Types

type Parser = Parsec Void String

parseFormula :: String -> Either String Formula
parseFormula s = case parse pFormula "failed in FormulaParser.parseFormula" s of
    Left parseErrorBundle -> Left $ errorBundlePretty (parseErrorBundle :: ParseErrorBundle String Void)
    Right formula -> Right formula

pFormula :: Parser Formula
pFormula = space 
        >> skipMany pComment
        >> space
        >> choice [
            pLiteral,
            pVar,
            pLogical,
            pDiamondBox,
            pFix
         ]

pComment :: Parser ()
pComment = do
    char '%'
    skipManyTill printChar eol
    return ()

pLiteral :: Parser Formula
pLiteral = (string "false" >> return F)
       <|> (string "true"  >> return T)

pVar :: Parser Formula
pVar = Var <$> upperChar

pLogical :: Parser Formula
pLogical = do
    char '('
    space
    f <- pFormula
    space
    junction <- string "&&" <|> string "||"
    space
    g <- pFormula
    space
    char ')'

    return $ if junction == "&&" then And f g else Or f g

pDiamondBox :: Parser Formula
pDiamondBox = do
    start <- char '<' <|> char '['
    action <- pActionName
    char '>' <|> char ']'
    f <- pFormula

    return $ (if start == '<' then Diamond else Box) action f

pActionName :: Parser String
pActionName = do
    c <- lowerChar
    cs <- many (lowerChar <|> digitChar <|> char '_')
    return $ c : cs

pFix :: Parser Formula
pFix = do
    sigma <- string "mu" <|> string "nu"
    space
    x <- upperChar
    char '.'
    f <- pFormula
    
    return $ (if sigma == "mu" then Mu else Nu) x f
