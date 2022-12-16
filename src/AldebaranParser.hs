module AldebaranParser (parseAldebaran, ParsedLTS(..), Transition(..)) where

import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Types

type Parser = Parsec Void String

data Transition = Transition State Label State
    deriving (Show, Eq)
data ParsedLTS = ParsedLTS {
    firstState :: State,
    nrOfTransitions :: Int,
    nrOfStates :: Int,
    transitions :: [Transition]
}
    deriving (Show, Eq)

parseAldebaran :: String -> Either String ParsedLTS
parseAldebaran s = case parse pAldebaran "failed in AldebaranParser.parseAldebaran" s of
    Left parseErrorBundle -> Left $ errorBundlePretty (parseErrorBundle :: ParseErrorBundle String Void)
    Right lts -> Right lts

pAldebaran :: Parser ParsedLTS
pAldebaran = do
    (s0, nt, ns) <- pHeader
    ts <- many pTransition
    return $ ParsedLTS s0 nt ns ts

pHeader :: Parser (State, Int, Int)
pHeader = do
    string "des ("
    s0 <- decimal
    char ','
    nt <- decimal
    char ','
    ns <- decimal
    char ')'
    space

    return (s0, nt, ns)

pTransition :: Parser Transition
pTransition = do
    char '('
    s <- decimal
    char ','
    char '"'
    l <- pLabel
    char '"'
    char ','
    t <- decimal
    char ')'
    space

    return $ Transition s l t

pLabel :: Parser String
pLabel = do
    c <- lowerChar
    cs <- many (lowerChar <|> digitChar <|> char '_')
    return $ c : cs
