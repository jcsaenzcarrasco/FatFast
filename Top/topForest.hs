{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import FTtop
import qualified Data.Set as S
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment
import Control.DeepSeq
import Data.Foldable

type Pair      = (Int,Int)
type SetPair   = S.Set Pair
type FT        = FingerTree SetPair Pair
data Top       = Top {getsetFT::SetPair,getFT::FT} deriving Show 
type Forest    = FingerTree SetPair Top
data TopForest = TopForest {getsetForest ::SetPair,getForest::Forest} deriving Show 

instance Edges SetPair Pair where
    edges p = S.insert p S.empty
instance Edges SetPair Top where
    edges (Top set _) = set     -- when == set, forest is FTsemi-alike;
                                -- when == mempty (or S.empty), forest is FTseq-alike
instance NFData Top where
   rnf (Top s t) = rnf s `seq` rnf t
instance NFData TopForest where
   rnf (TopForest s f) = rnf s `seq` rnf f

emptyTree :: FT
emptyTree =  empty

emptyForest :: Forest
emptyForest =  empty

listPairs :: Int -> Int -> [Pair]
listPairs 0    _  = []
listPairs _    0  = []
listPairs from to
    | from <= to = zip[from,(from+1)..to][from..] 
    | from >= to = zip[from,(from-1)..to][from,(from-1)..]
    
treeGen :: Int -> Int -> FT
treeGen from to = fromList (listPairs from to)

treeList :: [Pair] -> FT
treeList list = fromList list

topGen :: Int -> Int -> Top
topGen from to = 
    let set  = S.fromList list 
        ft   = treeList   list
        list = (listPairs from to)
    in  Top set ft

listTops :: Int -> Int -> [Top]
listTops 0 _    = []
listTops nTops sizeTops = top : listTops (nTops-1) sizeTops
 where
    to    = nTops * sizeTops
    from  = ( (nTops - 1) * sizeTops ) + 1
    top   = topGen from to

forestGen :: Int -> Int -> Forest
forestGen nTops sizeTops = foldr (<|) emptyForest (listTops nTops sizeTops)

forestGenList :: [Top] -> Forest
forestGenList list = fromList list

data ResultSearch 
   = NoResult
   | ResultSearch Top Pair Top 
   deriving Show


mer :: Top -> Top -> Top
mer (Top sleft tleft) (Top sright tright) = 
  let set = sleft `S.union` sright
      ft  = tleft   ><      tright
  in  set `seq` ft `seq` Top set ft

sea ::  Pair -> Top -> ResultSearch 
sea pair top@(Top set ft) = if p set then sea' p top else NoResult 
 where
    p b = (S.member pair) b

sea'   :: (SetPair -> Bool) -> Top -> ResultSearch 
sea' p top@(Top set ft) = case (search p ft ) of
    NoBuilt                            -> NoResult
    Built spot ltree lset x rtree rset ->
        case spot of
            LeftEnd   -> ResultSearch llset x lrset
            RightEnd  -> ResultSearch rlset x rrset
            CentreEnd -> ResultSearch lsetx x rsetx
        where
            lsetx = Top {getsetFT=lset         ,getFT=ltree}
            rsetx = Top {getsetFT=rset         ,getFT=rtree}
            
            llset = Top {getsetFT=lset          ,getFT=ltree}
            lrset = Top {getsetFT=set S.\\ xlset,getFT=rtree}

            rlset = Top {getsetFT=set S.\\ xrset,getFT=ltree}
            rrset = Top {getsetFT=rset          ,getFT=rtree}

            xlset = S.insert x lset -- (getPair x) lset
            xrset = S.insert x rset -- (getPair x) rset
    
lookForPair :: Pair -> TopForest -> Bool
lookForPair pair (TopForest set forest)
    = if p set then
         case (searchTree p forest) of
         BuiltTree _ top  _ -> case (sea pair top) of 
                                  ResultSearch _ _ _ -> True 
                                  NoResult           -> False
         NoBuiltTree        -> False
         
      else False
      where
          p b = (S.member pair) b


main = do
    [nx,sx]  <- getArgs
    let nt =  read nx :: Int  -- number of tops, therefore nnodes = n * size
    let s  =  read sx :: Int  -- size for tops
    let n  =  nt * s

    putStrLn $ " [][][][][][][][][][][][][][][][][][][][][] [ ][ ][ ][ ][ ][ ] [][][][][][][]\n"

    startList <- getCurrentTime
    let lstTops  = listTops nt s
    let lstTops' = deepseq lstTops "(LIST tops done);\n "
    putStr lstTops'
    endList   <- getCurrentTime 

    startFT <- getCurrentTime
    let forest    = forestGenList lstTops
--    let setforest = S.fromList (listPairs 1 (nt*s))
    let setforest = foldr ((<>) . getsetFT) S.empty lstTops
    let tforest   = TopForest setforest forest
    let tforest'  = deepseq tforest "(FOREST done);\n "
    putStr tforest'
    endFT   <- getCurrentTime


--    putStrLn $ show tforest

    startSea1 <- getCurrentTime
    let pair50  = (div n 2, div n 2)
    let spot1   = lookForPair pair50 tforest
    putStr $ "\n(SEARCHING: N/2 , N(3/4), N+1, N/4 : N = " ++ show n ++
     "\n" ++ show spot1 ++ show pair50 ++ "; "
    endSea1   <- getCurrentTime

    startSea2 <- getCurrentTime
    let p75     = div n 4
    let pair75 = (n - p75, n - p75)
    let spot2   = lookForPair pair75 tforest
    putStr $ show spot2 ++ show pair75 ++ "; "
    endSea2   <- getCurrentTime

    startSea3 <- getCurrentTime
    let pairFalse = (n + 1, n + 1)
    let spot3   = lookForPair pairFalse tforest
    putStr $ show spot3 ++ show pairFalse ++ "; "
    endSea3   <- getCurrentTime

    startSea4 <- getCurrentTime
    let pair25  = (div n 4, div n 4)
    let spot4   = lookForPair pair25 tforest
    putStr $ show spot4 ++ show pair25 ++ ";\n "
    endSea4   <- getCurrentTime

    let (BuiltTree _ tleft  _) = searchTree (S.member pair50) (getForest tforest)
    let (BuiltTree _ tright _) = searchTree (S.member pair25) (getForest tforest)
    let tleft' = deepseq tleft " "
    putStr tleft'
    let tright'= deepseq tright " "
    putStr tright'
    startMer <- getCurrentTime
    let mer'    = mer tleft tright
    let mer''   = deepseq mer' "\n(MERGING Ts done);\n " 
    putStr mer''
    endMer   <- getCurrentTime

    startMer2 <- getCurrentTime
    let pair30 = (div n 3, div n 3)
    let (BuiltTree _ t75 _)  = searchTree (S.member pair75) (getForest tforest)
    let (BuiltTree _ t30 _)  = searchTree (S.member pair30) (getForest tforest)
    let mer2     = mer t75 t30
    let mer2'    = deepseq mer2 "\n(MERGING Ts inc. pairs, done);\n " 
    putStr mer2'
    endMer2   <- getCurrentTime

    startIns <- getCurrentTime
    let extraList = listPairs (n+1) (n + s)
    let extraSet  = S.fromList extraList
    let extraFT   = treeList extraList
    let extraTop  = Top extraSet extraFT
    let newt = TopForest (extraSet <> setforest) (extraTop <| forest)
    let newt' = newt `seq` " "
    putStr newt' 
    endIns   <- getCurrentTime
    
    let pairExtraFalse = ((n + (div s 2) - 1),(n + s - 1))
    let pairExtraTrue  = ((n + (div s 2)),(n + (div s 2)))    
    let spot5   = lookForPair pairExtraFalse newt
    putStr $ "\nAdding Top from " ++ show (n+1) ++ " up to " ++ show (n+s) ++ "\n" 
     ++ show spot5 ++ "..." ++ show pairExtraFalse ++ "..."
    let spot6   = lookForPair pairExtraTrue newt
    putStr $ "\n" ++ show spot6 ++ "..." ++ show pairExtraTrue ++ "...  (INSERTION T done);\n "

    startVR <- getCurrentTime
    let (_ :> rmostt) = viewr (getForest newt)
    let (lmostp :< _) = viewl (getFT rmostt)
    putStr $ "\nrigth most pair: " ++ show lmostp ++ "\n"
    endVR   <- getCurrentTime

    let tlist = endList `diffUTCTime` startList 
    let tft   = endFT   `diffUTCTime` startFT 
    let tsea1 = endSea1 `diffUTCTime` startSea1 
    let tsea2 = endSea2 `diffUTCTime` startSea2
    let tsea3 = endSea3 `diffUTCTime` startSea3
    let tsea4 = endSea4 `diffUTCTime` startSea4 
    let tmer  = endMer  `diffUTCTime` startMer  
    let tmer2 = endMer2 `diffUTCTime` startMer2  
    let tins  = endIns  `diffUTCTime` startIns
    let trm   = endVR   `diffUTCTime` startVR

    putStrLn $ "\nLISTts______ " ++ show tlist ++
               "\nFOREST______ " ++ show tft   ++
               "\nVIEW_LFT_RGT " ++ show trm   ++ 
               "\nSEARCH(N1/2) " ++ show tsea1 ++
               "\nSEARCH(N3/4) " ++ show tsea2 ++
               "\nSEARCH(N+01) " ++ show tsea3 ++
               "\nSEARCH(N1/4) " ++ show tsea4 ++
               "\nMERGE_pr_giv " ++ show tmer  ++
               "\nMERGE_inc_pr " ++ show tmer2 ++
               "\nINSERT______ " ++ show tins  

    putStrLn $ " [][][][][][][][][] [-F-][-I-][-N-][-I-][-S-][-H-] [][][][][][][]\n"

-- | E X A M P L E S 
-- | use Data.Foldable for "toList" function on trees 

t01_09,t10,t11,t12_20,t01_20 :: FT 
t01_09  = treeGen 01 09   -- [(1,1)   ..   (9,9)]  where toList upon
t12_20  = treeGen 12 20   -- [(12,12) .. (20,20)]  where toList upon

t10 = t01_09  |> (10,10)  -- [(1,1)   .. (10,10)]  where toList upon
t11 = (11,11) <| t12_20   -- [(11,11) .. (20,20)]  where toList upon

t01_20 = t10 >< t11       -- [(1,1)  ..  (20,20)]  where toList upon


u20 :: FT
u20 =  foldr (<|) emptyTree (listPairs 1 20) -- == fromList as in treeGen 1 20

t21 = treeGen 1 21   -- a "medium-size" middle Single tree
t6  = treeGen 1 6    -- the "smallest-size" middle Single tree

forest2_5 = forestGen 2 5
set2_5    = S.fromList (listPairs 1 10)

topForest10 = TopForest set2_5 forest2_5 


