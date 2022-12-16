{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.DeepSeq
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.IntSet (IntSet)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

data Formula = F
             | T
             | Var Char
             | And Formula Formula
             | Or Formula Formula
             | Diamond String Formula
             | Box String Formula
             | Mu Char Formula
             | Nu Char Formula
    deriving (Eq, Show, Generic)

data Sign = MuSign | NuSign
    deriving (Show, Eq)

type Label = String
type State = Int
type Adjacencies = Vector (HashMap Label [State])
data LTS = LTS
    { states :: IntSet
    , adjacencies :: Adjacencies
    , nStates :: Int
    , nTransitions :: Int
    , startState :: State }
    deriving (Show, Eq, Generic)

instance NFData Formula
instance NFData LTS

(*!*) :: LTS -> (State, Label) -> [State]
lts *!* (s, a) = HashMap.findWithDefault [] a ((adjacencies lts) Vector.! s)
