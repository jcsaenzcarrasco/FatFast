
module Main where

import Prelude hiding (splitAt)
import FTseq
import System.Environment
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Foldable
import Control.DeepSeq 
import Data.Maybe

type Pair = (Int,Int)
type FT = Seq Pair 

type SeqForest = Seq FT 

tree1,tree2,tree3,tree4 :: FT
tree1 = fromList $ zip [1,3..10][1,3..]
tree3 = fromList $ zip [11,13..20][11,13..]
tree2 = fromList $ zip [2,4..10][2,4..]
tree4 = fromList $ zip [12,14..20][12,14..]

forest :: SeqForest
forest = tree3 <| tree4 <| (empty |> tree2 |> tree1) 

-- > FT.filter (/=Nothing) $ fmap (FT.elemIndexL (9,9)) forest
-- Sequence [Just 4]
-- > FT.Seq x = FT.filter (/=Nothing) $ fmap (FT.elemIndexL (9,9)) forest
-- > null x == False

ntxs :: Int -> Int -> [FT]
ntxs 0 _ = []
ntxs n s = ft : ntxs (n-1) s
 where
    to    = n * s 
    from  = ( (n-1) * s ) + 1
    lista = zip[from,from+1..to][from,from+1..]
    ft    = fromList lista

emptySeqForest :: SeqForest
emptySeqForest =  empty

seqForestN :: Int -> Int -> SeqForest
seqForestN n s = foldr (<|) emptySeqForest (ntxs n s)

seqForestNList :: [FT] -> SeqForest
seqForestNList list = fromList list

listPairs :: Int -> Int -> [Pair]
listPairs 0    _  = []
listPairs _    0  = []
listPairs from to
    | from <= to = zip[from,(from+1)..to][from..] 
    | from >= to = zip[from,(from-1)..to][from,(from-1)..]

ft xs = fromList xs 

main = do
    [nx,sx]  <- getArgs
    let nt =  read nx :: Int
    let s  =  read sx :: Int
    let n  = nt * s

    putStrLn $ "\n\n [][][][][][][][][][][][][][][][][][][][][] [ ][ ][ ][ ][ ][ ] [][][][][][][]\n"

    startList <- getCurrentTime
    let listFTs = ntxs nt s
    let list'   = deepseq listFTs "(LIST FTs done);\n "
    putStr list'
    endList   <- getCurrentTime 

    startFT <- getCurrentTime
    let ft     = seqForestNList listFTs
    let ft'    = deepseq ft "\n(FOREST done);\n "
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

    let (ResultForest _ tleft  _)  = searchForest pair50 ft
    let (ResultForest _ tright _)  = searchForest pair25 ft
    let tleft' = deepseq tleft " "
    putStr tleft'
    let tright'= deepseq tright " "
    putStr tright'
    startMer <- getCurrentTime
    let mer     = tleft >< tright
    let mer'    = deepseq mer "\n(MERGING Ts done);\n " 
    putStr mer'
    endMer   <- getCurrentTime

    startMer2 <- getCurrentTime
    let pair30 = (div n 3, div n 3)
    let (ResultForest _ t75 _)  = searchForest pair75 ft
    let (ResultForest _ t30 _)  = searchForest pair30 ft
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


lookForPair pair forest =  
      case (searchForest pair forest) of
        ResultForest _ tree _ -> True
        NoResultForest        -> False

data ResultForest = NoResultForest | ResultForest SeqForest FT SeqForest deriving Show

searchForest :: Pair -> SeqForest -> ResultForest 
searchForest pair forest 
  | idxF < 0  = NoResultForest
  | otherwise = ResultForest left tree right
  where
     left  = fst splito
     (tree :< right) = viewl $ snd splito
     splito = splitAt idxF forest
     (idxF, _) = whichTree $ fmap (elemIndexL pair) forest


-- whichTree :: (Num a, Num b) => FT.Seq (Maybe b) -> (a, b)
whichTree tree = whichTree' tree 0

-- whichTree' :: (Num a, Num b) => FT.Seq (Maybe b) -> a -> (a, b)
whichTree' tree n
    = case viewl tree of
          EmptyL    -> ((-1),(-1))
          (x :< xs) ->
              case x of
                  Nothing -> whichTree' xs (n+1)
                  Just x  -> (n,x)