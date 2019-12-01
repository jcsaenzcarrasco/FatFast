{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Main where

import FTfull
import Data.Monoid
import qualified Data.Set as S
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Control.DeepSeq

type Pair = (Int,Int)
type FT = FingerTree (S.Set Pair) Pair 
type Forest = FingerTree (S.Set Pair) FT

instance Edges (S.Set Pair) Pair where
    edges (x,y) = S.insert (x,y) S.empty 

emptyForest :: Forest
emptyForest =  Empty

ntxs :: Int -> Int -> [FT]
ntxs 0 _ = []
ntxs n s = ft : ntxs (n-1) s
 where
    to    = n * s
    from  = ( (n-1) * s ) + 1
    lista = zip[from,from+1..to][from,from+1..]
    ft    = fromList lista

listPairs :: Int -> Int -> [Pair]
listPairs 0    _  = []
listPairs _    0  = []
listPairs from to
    | from <= to = zip[from,(from+1)..to][from..] 
    | from >= to = zip[from,(from-1)..to][from,(from-1)..]

forestN :: Int -> Int -> Forest
forestN n s = foldr (<|) emptyForest (ntxs n s)

forestNList :: [FT] -> Forest
forestNList list = fromList list

main = do
    [nx,sx]  <- getArgs
    let nt = read nx :: Int
    let s  = read sx :: Int
    let n  = nt * s 

    putStrLn $ "   [][][][][][][][][][][][][][][][][][][][] [ ][ ][ ][ ][ ][ ] [][][][][][][]\n"

    startList <- getCurrentTime
    let listFTs = ntxs nt s
    let list' = deepseq listFTs "(LIST done);\n "
    putStr list'
    endList   <- getCurrentTime 

    startFT <- getCurrentTime
    let ft     = forestNList listFTs
    let ft'    = deepseq ft "(FT done);\n "
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

    let (Position _ tleft  _)  = searchPair pair50 ft
    let (Position _ tright _)  = searchPair pair25 ft
    let tleft' = deepseq tleft " "
    putStr tleft'
    let tright'= deepseq tright " "
    putStr tright'
    
    startMer <- getCurrentTime
    let mer     = tleft >< tright
    let mer'    = deepseq mer "\n(MERGING Ts pairs given, done);\n " 
    putStr mer'
    endMer   <- getCurrentTime

    startMer2 <- getCurrentTime
    let pair30 = (div n 3, div n 3)
    let (Position _ t75 _)  = searchPair pair75 ft
    let (Position _ t30 _)  = searchPair pair30 ft
    let mer2     = t75 >< t30
    let mer2'    = deepseq mer2 "\n(MERGING Ts inc. pairs, done);\n " 
    putStr mer2'
    endMer2   <- getCurrentTime


    startIns <- getCurrentTime
    let extraList = listPairs (n+1) (n+s)
    let extraTree = fromList extraList
    let newft     = extraTree <| ft
    let newft'    = seq newft " "
    putStr newft'
    endIns   <- getCurrentTime

    let pairExtraFalse = ((n + (div s 2) - 1),(n + s - 1))
    let pairExtraTrue  = ((n + (div s 2)),(n + (div s 2)))    
    let spot5   = lookForPair pairExtraFalse newft    
    putStr $ "\nAdding FT from " ++ show (n+1) ++ " up to " ++ show (n+1+s) ++ "\n" 
     ++ show spot5 ++ "..." ++ show pairExtraFalse ++ "..."
    let spot6   = lookForPair pairExtraTrue newft
    putStr $ "\n" ++ show spot6 ++ "..." ++ show pairExtraTrue ++ "...  (INSERTION T done);\n" 

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

    putStrLn $ "\n [][][][][][][][][] [-F-][-I-][-N-][-I-][-S-][-H-] [][][][][][][]\n"


lookForPair pair ft
   = case (search p ft) of
     Position _ tree _ -> case (search p tree) of
                           Position _ _ _ -> True
                           _              -> False
     _                 -> False
     where
       p b a = (S.member pair) b


searchPair pair tree = search p tree
 where
     p b a = (S.member pair) b


ins :: Pair -> FT -> FT
ins x Empty = Single x
ins x (Single y) = deep (One x) Empty (One y)
ins x (Deep v pr mi sf) =
       Deep (S.insert x v) (consDigit x pr) mi sf
ins x xs@(Deep v (Four _ _ _ _) _ _) = x <| xs