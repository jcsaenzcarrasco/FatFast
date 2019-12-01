{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import FTsemi
import Data.Monoid
import qualified Data.Set as S
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Foldable
import Control.DeepSeq

type Pair = (Int,Int)
type FT = FingerTree (S.Set Pair) Pair 

type Forest = FingerTree (S.Set Pair) FT

emptyForest :: Forest 
emptyForest =  Empty

t1,t2,t3,t4 :: FT 
t1 = fromList [(3,3),(1,1),(5,5)]
t2 = fromList [(6,6),(2,2),(4,4)]
t3 = fromList [(7,7),(9,9),(11,11)]
t4 = fromList [(12,12),(8,8),(10,10)]

forest = t2 <| t3 <| t4 <| t1 <| emptyForest

instance Edges (S.Set Pair) Pair where
    edges (x,y) = S.insert (x,y) S.empty 


listZip :: Int -> [Pair]
listZip n = zip[1..n][1..]

ntxs :: Int -> Int -> [FT]
ntxs 0 _ = []
ntxs n s = ft : ntxs (n-1) s
 where
    to    = n * s 
    from  = ( (n-1) * s ) + 1
    lista = zip[from,from+1..to][from,from+1..]
    ft    = fromList lista

forestN :: Int -> Int -> Forest
forestN n s = foldr (<|) empty (ntxs n s)

forestNList :: [FT] -> Forest
forestNList list =  fromList list

{-
instance NFData a => NFData (FT a) where
    rnf (FT Empty)             = ()
    rnf (FT (Single x))        = rnf x
    rnf (FT (Deep _ pr mi sf)) = rnf pr `seq` rnf mi `seq` rnf sf

instance NFData a => NFData (Forest a) where
    rnf (Forest Empty)             = ()
    rnf (Forest (Single x))        = rnf x
    rnf (Forest (Deep _ pr mi sf)) = rnf pr `seq` rnf mi `seq` rnf sf
-}

listPairs :: Int -> Int -> [Pair]
listPairs 0    _  = []
listPairs _    0  = []
listPairs from to
    | from <= to = zip[from,(from+1)..to][from..] 
    | from >= to = zip[from,(from-1)..to][from,(from-1)..]


main = do 
    [nx,sx]  <- getArgs
    let nt =  read nx :: Int
    let s  =  read sx :: Int
    let n  =  nt * s 

    putStrLn $ " [][][][][][][][][][][][][][][][][][][][][] [ ][ ][ ][ ][ ][ ] [][][][][][][]\n"

    startList <- getCurrentTime
    let listFTs = ntxs nt s 
    let list'   = deepseq listFTs "(LIST FTs done);\n "
    putStr list'
    endList   <- getCurrentTime 

    startFT <- getCurrentTime
    let ft     = forestNList listFTs
    let ft'    = deepseq ft "(FOREST done);\n "
    putStr ft'
    endFT   <- getCurrentTime

    startSea1 <- getCurrentTime
    let pair50 = (div n 2, div n 2)
    let spot1  = lookForPair pair50 ft 
    putStr $ "\n(SEARCHING: N/2 , N(3/4), N+1, N/4 : N = " ++ show n
     ++" \n" ++ show spot1 ++ show pair50 ++ "; "
    endSea1   <- getCurrentTime

    startSea2 <- getCurrentTime
    let p75     = div n 4
    let pair75 = (n - p75, n - p75)
    let spot2   = lookForPair pair75 ft 
    putStr $ show spot2 ++ show pair75 ++ "; "
    endSea2   <- getCurrentTime

    startSea3 <- getCurrentTime
    let pairFalse = (n + 1, n + 1)
    let spot3     = lookForPair pairFalse ft 
    putStr $ show spot3 ++ show pairFalse ++ "; "
    endSea3   <- getCurrentTime

    startSea4 <- getCurrentTime
    let pair25 = (div n 4, div n 4)
    let spot4     = lookForPair pair25 ft 
    putStr $ show spot4 ++ show pair25 ++ ";\n "
    endSea4   <- getCurrentTime

--    let (ResultForest tl1 _ tr1)  = searchForest pair50 ft
--    let (ResultForest tl2 _ tr2)  = searchForest pair25 ft
    let (Position tl1 _ tr1)  = lookForPairFT pair50 ft
    let (Position tl2 _ tr2)  = lookForPairFT pair25 ft
    let tleft = tl1 >< tr1
    let tleft' = deepseq tleft " "
    putStr tleft'
    let tright = tl2 >< tr2
    let tright'= deepseq tright " "
    putStr tright'
    startMer <- getCurrentTime
    let mer     = tleft >< tright
    let mer'    = deepseq mer "\n(MERGING Ts done);\n " 
    putStr mer'
    endMer   <- getCurrentTime

    startMer2 <- getCurrentTime
    let pair30 = (div n 3, div n 3)
--    let (ResultForest t75l _ t75r)  = searchForest pair75 ft
--    let (ResultForest t30l _ t30r)  = searchForest pair30 ft
    let (Position t75l _ t75r)  = lookForPairFT pair75 ft
    let (Position t30l _ t30r)  = lookForPairFT pair30 ft
    let mer30    = t30l >< t30r
    let mer75    = t75l >< t75r
    let mer2     = mer30 >< mer75
    let mer2'    = deepseq mer2 "\n(MERGING Ts inc. pairs, done);\n " 
    putStr mer2'
    endMer2   <- getCurrentTime

    startIns <- getCurrentTime
    let extraList =  listPairs (n+1) (n+s) 
    let extraTree =  fromList extraList 
    let newft      = extraTree <| ft
    let newft'    = seq newft " " 
    putStr newft' 
    endIns   <- getCurrentTime
    
    let pairExtraFalse = ((n + (div s 2) - 1),(n + s - 1))
    let pairExtraTrue  = ((n + (div s 2)),(n + (div s 2)))    
    let spot5   = lookForPair pairExtraFalse newft    
    putStr $ "\nAdding FT from " ++ show (n+1) ++ " up to " ++ show (n+1+s) ++ "\n" 
     ++ show spot5 ++ "..." ++ show pairExtraFalse ++ "..."
    let spot6   = lookForPair pairExtraTrue newft
    putStr $ "\n" ++ show spot6 ++ "..." ++ show pairExtraTrue ++ "... (INSERTION T done)\n" 
    
    startVR <- getCurrentTime
    let (_ :> rmostt) = viewr newft
    let (lmostp :< _) = viewl rmostt
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

{-
lookForPair pair forest =  
      case (searchForest pair forest) of
        ResultForest _ tree _ -> True
        NoResultForest        -> False
-}
data ResultForest = NoResultForest | ResultForest FT Pair FT deriving Show

searchForest :: Pair -> Forest -> ResultForest 
searchForest = undefined
{-
searchForest pair forest = sweep lista
  where
     lista = toList $ fmap (search p) forest
     p b   = (S.member pair) b


sweep []     = NoResultForest
sweep (x:xs) = case x of
    NoBuilt     -> sweep xs  
    Built l x r -> ResultForest l x r 
-}
lookForPair :: Pair -> Forest -> Bool -- SearchResult (S.Set Pair) FT
lookForPair p f
   = case (search pred f) of
   Position _ _ _ -> True
   _ -> False
   
 where
    pred b _ = (S.member p) b

lookForPairFT :: Pair -> Forest -> SearchResult (S.Set Pair) FT
lookForPairFT p f =  search pred f
 where
    pred b _ = (S.member p) b
