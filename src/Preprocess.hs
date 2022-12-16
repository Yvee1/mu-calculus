module Preprocess (preprocess) where

import Types
import AldebaranParser
import Control.Monad
import qualified Data.IntSet as IntSet
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Mutable as MVector

preprocess :: ParsedLTS -> LTS
preprocess pLTS = LTS ss adj ns nt s0
    where ns = nrOfStates pLTS
          nt = nrOfTransitions pLTS
          s0 = firstState pLTS
          ss = IntSet.fromDistinctAscList [0..ns-1]
          adj = Vector.create $ do
            v <- MVector.replicate ns HashMap.empty
            forM_ (transitions pLTS) $ \(Transition s l t) -> MVector.modify v (HashMap.insertWith (++) l [t]) s
            return v
