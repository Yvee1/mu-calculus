module FormulaHelpers where

import Types
import qualified Data.Set as Set

isOpen :: Formula -> Bool
isOpen = go Set.empty
    where go _ F = False
          go _ T = False
          go sVars (Var c)   = if c `Set.member` sVars then False else True
          go sVars (And f g) = go sVars f || go sVars g
          go sVars (Or f g)  = go sVars f || go sVars g
          go sVars (Diamond _ f) = go sVars f
          go sVars (Box _ f) = go sVars f
          go sVars (Mu c f) = go (Set.insert c sVars) f 
          go sVars (Nu c f) = go (Set.insert c sVars) f

varsOfOpenSubformulas :: Sign -> Formula -> [Char]
varsOfOpenSubformulas sign = go
    where go F = []
          go T = []
          go (Var _) = []
          go (And f g) = go f ++ go g
          go (Or f g) = go f ++ go g
          go (Diamond _ f) = go f
          go (Box _ f) = go f
          go f@(Mu c g) = (if sign == MuSign && isOpen f then (c:) else id) (go g)
          go f@(Nu c g) = (if sign == NuSign && isOpen f then (c:) else id) (go g)

nestingDepth :: Formula -> Int
nestingDepth = nd
    where nd F = 0
          nd T = 0
          nd (Var _) = 0
          nd (And f g) = max (nd f) (nd g)
          nd (Or  f g) = max (nd f) (nd g)
          nd (Diamond _ f) = nd f
          nd (Box _  f) = nd f
          nd (Mu _ f) = 1 + nd f
          nd (Nu _ f) = 1 + nd f

alternationDepth :: Formula -> Int
alternationDepth = ad
    where ad F = 0
          ad T = 0
          ad (Var _) = 0
          ad (And f g) = max (ad f) (ad g)
          ad (Or  f g) = max (ad f) (ad g)
          ad (Diamond _ f) = ad f
          ad (Box _ f) = ad f
          ad (Mu _ f) = muNu NuSign f 
          ad (Nu _ f) = muNu MuSign f
          
          muNu otherSign f = maximum ([ad f, 1 + maximum (0 : map ad (fixpointChildFormulas otherSign f))]) 

dependentAlternationDepth :: Formula -> Int
dependentAlternationDepth = dad
    where dad F = 0
          dad T = 0
          dad (Var _) = 0
          dad (And f g) = max (dad f) (dad g)
          dad (Or  f g) = max (dad f) (dad g)
          dad (Diamond _ f) = dad f
          dad (Box _ f) = dad f
          dad (Mu c f) = muNu NuSign c f
          dad (Nu c f) = muNu MuSign c f

          muNu otherSign c f = maximum ([dad f, 1 + maximum (0 : map dad (filter (c `occursIn`) (fixpointChildFormulas otherSign f)))]) 

fixpointChildFormulas :: Sign -> Formula -> [Formula]
fixpointChildFormulas sign = cf
    where cf F = []
          cf T = []
          cf (Var _) = []
          cf (And f g) = cf f <> cf g
          cf (Or  f g) = cf f <> cf g
          cf (Diamond _ f) = cf f
          cf (Box _ f) = cf f
          cf f@(Mu _ _) = if sign == MuSign then [f] else []
          cf f@(Nu _ _) = if sign == NuSign then [f] else []

occursIn :: Char -> Formula -> Bool
occursIn c = (c `elem`) . varsOfFormula

varsOfFormula :: Formula -> [Char]
varsOfFormula = go
    where go F             = []
          go T             = []
          go (Var c)       = [c]
          go (And f g)     = go f <> go g
          go (Or f g)      = go f <> go g
          go (Diamond _ f) = go f
          go (Box _ f)     = go f
          go (Mu _ f)      = go f
          go (Nu _ f)      = go f
