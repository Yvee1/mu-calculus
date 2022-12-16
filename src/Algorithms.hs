{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Algorithms (naive, emersonLei) where

import qualified Control.Monad.State.Strict as MS
import Control.Monad.Trans.Reader
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro.Platform

import FormulaHelpers
import Types

-- Basic generic mu-calculus model checking function, allowing different ways to initialize the variables in a fixpoint.
generic :: AlgState m => (Sign -> Char -> Formula -> m ()) -> (Sign -> m IntSet -> m IntSet) -> Formula -> m IntSet
generic initFixpoint recFixpoint = go
    where --go :: Formula -> m IntSet
          go F             = return $ IntSet.empty
          go T             = states <$> getLTS
          go (Var c)       = (HashMap.! c) <$> getHm
          go (And f g)     = andOr f g IntSet.intersection
          go (Or f g)      = andOr f g IntSet.union
          go (Diamond a f) = diamondBox a f any
          go (Box a f)     = diamondBox a f all
          go (Mu c f)      = muNu MuSign c f
          go (Nu c f)      = muNu NuSign c f

          andOr f g comb = 
            do s1 <- go f
               s2 <- go g
               return $ s1 `comb` s2

          diamondBox a f func = 
            do sf <- go f
               lts <- getLTS
               let pred s = func (`IntSet.member` sf) (lts *!* (s, a))
               return $ IntSet.filter pred (states lts)

          muNu sign c f = do
            initFixpoint sign c f
            hm <- getHm
            fixPoint sign c f (hm HashMap.! c)

          fixPoint sign c f s1 =
            do s2 <- recFixpoint sign (go f)
               inc
               updHm c s2
               if s1 == s2 then return s2 else fixPoint sign c f s2

-- Naive algorithm: simply set a variable to the emptyset or the entire state space, for Mu and Nu signs respectively.
naive :: LTS -> Formula -> (IntSet, Int)
naive lts form = (\(rs, (_, _, n)) -> (rs, n)) . flip MS.runState (lts, HashMap.empty :: HashMap Char IntSet, 0) $ generic initFix (flip const) form
  where initFix MuSign c _ = updHm c IntSet.empty
        initFix NuSign c _ = updHm c =<< (states <$> getLTS)

-- Initialize variables according to the Emerson-Lei algorithm as described in the slides.
emersonLei :: LTS -> Formula -> (IntSet, Int)
emersonLei lts form =
  let (rs, (_, _, n)) = flip MS.runState (lts, initMap lts form, 0) . flip runReaderT Nothing $ generic initFix recFix form
  in (rs, n)
  where initFix sign c f = do
          previousSign <- getSign
          case sign of
            MuSign -> 
              if previousSign /= Just NuSign 
                then return ()
                else mapM_ (\cOpen -> updHm cOpen IntSet.empty) (varsOfOpenSubformulas MuSign (Mu c f))
            
            NuSign ->
              if previousSign /= Just MuSign
                then return ()
                else do lts <- getLTS
                        mapM_ (\cOpen -> updHm cOpen (states lts)) (varsOfOpenSubformulas NuSign (Nu c f))

        recFix sign go = local (const (Just sign)) go

-- Below are the definitions of helper functions.

class Monad m => AlgState m where
  getLTS :: m LTS
  getHm  :: m (HashMap Char IntSet)
  updHm  :: Char -> IntSet -> m ()
  inc    :: m ()

type BasicState = (LTS, HashMap Char IntSet, Int)
type NaiveState = MS.State BasicState

instance AlgState NaiveState where
  getLTS = MS.gets (^. _1)
  getHm = MS.gets (^. _2)
  updHm c s = MS.modify (_2 %~ (HashMap.insert c s)) 
  inc = MS.modify (_3 %~ (+1))

type ELState = ReaderT (Maybe Sign) (MS.State BasicState)

instance AlgState ELState where
  getLTS = MS.gets (^. _1)
  getHm = MS.gets (^. _2)
  updHm c s = MS.modify (_2 %~ (HashMap.insert c s)) 
  inc = MS.modify (_3 %~ (+1))

getSign :: ELState (Maybe Sign)
getSign = ask

initMap :: LTS -> Formula -> HashMap Char IntSet
initMap lts = go HashMap.empty
    where go m F = m
          go m T = m
          go m (Var _) = m
          go m (And f g) = let m' = go m f in go m' g
          go m (Or f g)  = let m' = go m f in go m' g
          go m (Diamond _ f) = go m f
          go m (Box _ f) = go m f
          go m (Mu c f) = go (HashMap.insert c IntSet.empty m) f
          go m (Nu c f) = go (HashMap.insert c (states lts) m) f
