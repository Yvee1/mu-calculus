{-# LANGUAGE TupleSections #-}
module Main (main) where

import Algorithms
import AldebaranParser
import Control.Monad
import Lens.Micro.Platform
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import FormulaParser
import FormulaHelpers
import Options.Applicative
import System.CPUTime
import System.Directory
import System.FilePath
import Preprocess
import qualified Data.IntSet as IntSet

data Options = Options
    { ltsFileName :: FilePath
    , muFileName  :: FilePath
    , useNaive    :: Bool }

opts :: Parser Options
opts  =  Options
     <$> argument str (help "Path to file containing the LTS in Aldebaran format" <> metavar "LTS")
     <*> argument str (help "Path to file containing the mu-calculus formula" <> metavar "FORMULA")
     <*> switch (long "naive" <> short 'n' <> help "Use naive model checking algorithm")

optsWithHelp :: ParserInfo Options
optsWithHelp = info (opts <**> helper) $
       fullDesc 
    <> progDesc "Perform model checking of a labeled transition system (LTS) using the mu-calculus. Please provide a file containing the LTS and a file containing the mu-calculus formula. Use option -h or --help for more information."
    <> header "mu-calculus model checker"

-- main = runMaybeT $ runDir "boardgame"
-- main = runMaybeT $ testCases

main :: IO (Maybe ())
main = runMaybeT $ do
    Options ltsFn muFn useNaive <- liftIO $ execParser optsWithHelp
    pLTS <- readAndParse parseAldebaran ltsFn
    f <- readAndParse parseFormula muFn

    liftIO $ do
        let lts = preprocess pLTS
        -- putStrLn "======== FORMULA ========"
        -- print f
        -- putStrLn ""
        if useNaive
            then do putStrLn "====== Naive ======"
                    let (naiveSet, naiveFixIters) = naive lts f
                    putStrLn $ "State 0 satisfies formula: " <> show (0 `IntSet.member` naiveSet)
                    putStrLn $ "Fixpoint iterations: " <> show naiveFixIters
            else do putStrLn "=== Emerson-Lei ==="
                    let (elSet, elFixIters) = emersonLei lts f
                    putStrLn $ "State 0 satisfies formula: " <> show (0 `IntSet.member` elSet)
                    putStrLn $ "Fixpoint iterations: " <> show elFixIters

readAndParse :: (String -> Either String a) -> FilePath -> MaybeT IO a
readAndParse parser fp = do
    s <- liftIO $ readFile fp
    case parser s of
        Left err  -> liftIO (putStrLn err) >> mzero
        Right lts -> return lts

testCases :: MaybeT IO ()
testCases = do
    dirs <- sort <$> (liftIO $ listDirectory "testcases")
    results <- forM dirs $ \dir -> do
        files <- sort <$> (liftIO . listDirectory $ "testcases" </> dir)
        let aut = head $ filter (\f -> takeExtension f == ".aut") files
            mcfs = filter ((==".mcf") . takeExtension) files
        lts <- preprocess <$> readAndParse parseAldebaran ("testcases" </> dir </> aut)
        forM mcfs $ \mcf -> do
            f <- readAndParse parseFormula ("testcases" </> dir </> mcf)
            let (r, _) = naive lts f
            return $ dir <> " " <> mcf <> " " <> show r
    
    liftIO . writeFile "testcases/results.txt" . unlines $ map unlines results

runDir :: FilePath -> MaybeT IO () --[[(String, Int, Bool, (Int, Integer), (Int, Integer))]]
runDir dir = do
    guard =<< liftIO (doesDirectoryExist dir)
    files <- liftIO $ listDirectory dir
    let auts   = filter (\f -> takeExtension f == ".aut") files
        auts_n = map (read . last . splitOn "_" . takeBaseName) auts :: [Int]
        auts__n = sortBy (\(n1, _) (n2, _) -> compare n1 n2) (zip auts_n auts)
        mcfs = sort $ filter ((==".mcf") . takeExtension) files
    forms <- mapM (\mcf -> (mcf,) <$> readAndParse parseFormula (dir </> mcf)) mcfs
    ltss <- mapM (\(n, aut) -> (n, aut,) . preprocess <$> readAndParse parseAldebaran (dir </> aut)) auts__n
    liftIO $ print mcfs
    liftIO $ print auts_n
    xss <- liftIO $ forM forms $ \(mcf, f) -> do
        forM ltss $ \(n, aut, lts) -> do
            t1 <- (lts, f) `deepseq` getCPUTime
            let (rn, nn) = naive lts f
            t2 <- (rn, nn) `deepseq` getCPUTime
            let (re, ne) = emersonLei lts f
            t3 <- (re, ne) `deepseq` getCPUTime
            guard (rn == re)
            return (
                takeBaseName mcf <> " " <> show (nestingDepth f, alternationDepth f, dependentAlternationDepth f), 
                n, 
                0 `IntSet.member` re, 
                (nn, (t2 - t1) `div` 1000000000), 
                (ne, (t3 - t2) `div` 1000000000)
                )
    let content = format xss
    liftIO $ writeFile (dir </> "results.txt") (unlines content)

format :: [[(String, Int, Bool, (Int, Integer), (Int, Integer))]] -> [String]
format xss =
    flip concatMap xss
        (\xs -> [
            head xs ^. _1,
            "naive",
            unwords (map (coords _2 (_4._1)) xs),
            unwords (map (show . (^. _4._2)) xs),
            "emerson-lei",
            unwords (map (coords _2 (_5._1)) xs),
            unwords (map (show . (^. _5._2)) xs),
            "initial state satisfies formula",
            unwords (map (show . (^. _3)) xs),
            ""
            ])


    where coords l1 l2 t = "(" <> show (t ^. l1) <> "," <> show (t ^. l2) <> ")"
