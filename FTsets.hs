{-#  LANGUAGE FlexibleInstances     #-}
{-#  LANGUAGE FlexibleContexts      #-}
{-#  LANGUAGE MultiParamTypeClasses #-}

module Main where 

import FTs 
import qualified Data.Set as S
import Data.Maybe (fromJust) 
import GHC.Exts (IsList(..))
import Data.Monoid ((<>),Sum,getSum)
import System.Environment 


-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
--                                                         DATA FTs 
--                                                      type FTsets
--                                                      type FTidxs
--                                                      type FTpair
-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]

data DatumSet a = DS a     deriving (Eq, Ord, Show)
data DatumIdx a = DI a     deriving (Eq, Ord, Show)
data DatumTup a = DT (a,a) deriving (Eq, Ord, Show)
type FTsets   a = FingerTree (S.Set a)     (DatumSet a)
type FTidxs   a = FingerTree (  Sum a)     (DatumIdx a)
type FTpair   a = FingerTree (S.Set (a,a)) (DatumTup a)

emptyFTsets :: (Ord a) => FTsets a 
emptyFTsets  = empty 

emptyFTidxs :: (Num a,Ord a) => FTidxs a 
emptyFTidxs  = empty 

emptyFTpair :: (Num a,Ord a) => FTpair a 
emptyFTpair  = empty 

instance (Ord a) => Measured (S.Set a) (DatumSet a) where
   measure (DS x) = S.insert x S.empty 

instance (Num a,Ord a) => Measured (Sum a) (DatumIdx a) where
   measure (DI x) = getSum 1 

instance (Ord a) => Measured (S.Set (a,a)) (DatumTup a) where
   measure (DT p) = S.insert p S.empty 


-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
--                                                             ROOT 
-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]

rootFTsets :: Ord a => FTsets a -> Maybe (DatumSet a)  
rootFTsets tree = case viewl tree of
  EmptyL   -> Nothing
  x :< _   -> Just x

rootFTidxs :: (Num a, Ord a) => FTidxs a -> Maybe (DatumIdx a)  
rootFTidxs tree = case viewl tree of
  EmptyL   -> Nothing
  x :< _   -> Just x

rootFTpair :: (Num a, Ord a) => FTpair a -> Maybe (DatumTup a)  
rootFTpair tree = case viewl tree of
  EmptyL   -> Nothing
  x :< _   -> Just x

-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
--                                                           TOLIST 
-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]

toListFTsets :: (Ord a) => FTsets a -> [DatumSet a] 
toListFTsets ft = case (viewl ft) of
  EmptyL  -> []
  x :< xs -> x : toListFTsets xs

toListFTidxs :: (Num a,Ord a) => FTidxs a -> [DatumIdx a] 
toListFTidxs ft = case (viewl ft) of
  EmptyL  -> []
  x :< xs -> x : toListFTidxs xs

toListFTpair :: (Num a,Ord a) => FTpair a -> [DatumTup a] 
toListFTpair ft = case (viewl ft) of
  EmptyL  -> []
  x :< xs -> x : toListFTpair xs

-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
--                                                         EXAMPLES 
-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]

fts1,fts2,fts8,fts9 :: FTsets Int
fti1,fti2,fti8,fti9 :: FTidxs Int
ftp1,ftp2,ftp8,ftp9 :: FTpair Int

fts1 = foldr   (<|)  emptyFTsets  [DS 2,DS 3,DS 4,DS 1]
fts2 = foldr   (<|)  emptyFTsets  (map DS [5,6,7]) 
fts8 = (DS 8)   <|   emptyFTsets 
fts9 = (DS 99)  <|   emptyFTsets 

fti1 = foldr   (<|)  emptyFTidxs  [DI 2,DI 3,DI 4,DI 1]
fti2 = foldr   (<|)  emptyFTidxs  (map DI [5,6,7]) 
fti8 = (DI 8)   <|   emptyFTidxs 
fti9 = (DI 99)  <|   emptyFTidxs 

ftp1 = foldr   (<|)  emptyFTpair  [DT (2,2),DT (3,3),DT (4,4),DT (1,1)]
ftp2 = foldr   (<|)  emptyFTpair  (map DT [(5,5),(6,6),(7,7)]) 
ftp8 = (DT (8,8))   <| emptyFTpair
ftp9 = (DT (99,99)) <| emptyFTpair  

ft :: FTsets Int 
ft =  Deep total prefix middle suffix 

total = S.fromList [2,4,3,1,5]
prefix = Two (DS 2) (DS 4) 
suffix = One (DS 5)
middle = Single $ Node2 (S.fromList [3,1]) (DS 3) (DS 1) 


-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
--                                                            MAIN 
-- [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]

main = do
  [oprx, optx, totalx] <- getArgs
-- oprx == operation : [ "INS" for insertion or (<|),"MER" for merging or (><) ]
  let opt   = read optx   :: Int -- option : [sets,idxs,pair]
  let total = read totalx :: Int -- number of elements or data
  putStrLn $ (solution oprx opt total)

--solution :: String -> Int -> Int -> String
solution opr opc total = case opr of
   "INS" -> solINS opc total
   "MER" -> solMER opc total
   _     -> error "\nPlease select either INS or MER for insertion or merging \n\n"

solINS :: Int -> Int -> String 
solINS opc total
 | opc == 0 =                                              -- Sets
    let tree     = foldr (<|) emptyFTsets (map DS [1,3..total])
        (_ :> x) = viewr tree
    in  show x 

 | opc == 1 =                                              -- Idxs
    let tree     = foldr (<|) emptyFTidxs (map DI [2,4..total])
        (_ :> x) = viewr tree
    in  show x 

 | opc == 2 =                                              -- Pair
    let tree     = foldr (<|) emptyFTpair (map DT (zip [1,3..total][2,4..total]))  
        (_ :> x) = viewr tree
    in  show x 

solMER :: Int -> Int -> String
solMER opc total
 | opc == 0 =                                              -- Sets
    let limR  = div total 2
        treeL = foldr (<|) emptyFTsets (map DS [1,3..limR])
        treeR = foldr (<|) emptyFTsets (map DS [limR+1,limR+2..total])
        tree  = treeL >< treeR
        (_ :> x) = viewr tree
    in  show x 
 | opc == 1 =                                              -- Idxs
    let limR  = div total 2
        treeL = foldr (<|) emptyFTidxs (map DI [2,4..limR])
        treeR = foldr (<|) emptyFTidxs (map DI [limR+1,limR+2..total])
        tree  = treeL >< treeR
        (_ :> x) = viewr tree
    in  show x 
 | opc == 2 =                                              -- Pair
    let limR  = div total 2
        treeL = foldr (<|) emptyFTpair (map DT (zip[2,4..limR][2,4..limR]) )
        treeR = foldr (<|) emptyFTpair (map DT (zip[limR+1,limR+2..total][limR+1,limR+2..total]) )
        tree  = treeL >< treeR
        (_ :> x) = viewr tree
    in  show x 




search' :: (Eq a, Measured v a) => a -> (v-> v -> Bool) -> FingerTree v a -> SearchResult v a
search' w p tree
  = case tree of
    Empty              -> Nowhere
    Single x           ->
       if w == x then Position mempty w mempty
                 else Nowhere
    Deep  _ pr mid sf  ->
         case (searchT p tree) of
          Built l x r -> Position l x r
          NoBuilt     -> Nowhere

data Built t a = NoBuilt | Built t a t

searchT :: (Measured v a) => (v -> v -> Bool) -> FingerTree v a
        -> Built (FingerTree v a) a
searchT _ Empty      = NoBuilt 
searchT p (Single x)
   | p (measure x) mempty = Built Empty x Empty
   | otherwise            = NoBuilt
searchT p (Deep _ pr m sf) 
  | p vl mempty =  let  Split l x r     =  searchD p pr 
                   in   Built (maybe Empty digitToTree l) x (deepL r m sf)
  | p vr mempty =  let  Split l x r     =  searchD p sf 
                   in   Built (deepR pr  m  l) x (maybe Empty digitToTree r)
  | otherwise   =  case (searchT p m) of
                   NoBuilt        -> NoBuilt
                   Built ml xs mr -> 
                     let  Split l x r     =  searchN p xs
                     in   Built (deepR pr ml l) x (deepL r mr sf) 
  where
    vl  = measure pr
    vr  = measure sf

searchD :: (Measured v a) => (v -> v -> Bool) -> Digit a
        -> Split (Maybe (Digit a)) a
searchD _ (One a)        = Split Nothing         a  Nothing
searchD p (Two a b) 
  | p (measure a) mempty = Split Nothing         a  (Just (One b))
  | otherwise            = Split (Just (One a))  b  Nothing
searchD p (Three a b c) 
  | p (measure a) mempty = Split Nothing           a  (Just (Two b c))
  | p (measure b) mempty = Split (Just (One a))    b  (Just (One c))
  | otherwise            = Split (Just (Two a b))  c  Nothing
searchD p (Four a b c d) 
  | p (measure a) mempty = Split Nothing               a  (Just (Three b c d))
  | p (measure b) mempty = Split (Just (One a))        b  (Just (Two c d))
  | p (measure c) mempty = Split (Just (Two a b))      c  (Just (One d))
  | otherwise            = Split (Just (Three a b c))  d  Nothing

searchN :: (Measured v a) => (v -> v -> Bool) -> Node v a
        -> Split (Maybe (Digit a)) a
searchN p (Node2 _ a b) 
  | p (measure a) mempty = Split Nothing         a  (Just (One b))
  | otherwise            = Split (Just (One a))  b  Nothing
searchN p (Node3 _ a b c) 
  | p (measure a) mempty = Split Nothing           a  (Just (Two b c))
  | p (measure b) mempty = Split (Just (One a))    b  (Just (One c))
  | otherwise            = Split (Just (Two a b))  c  Nothing
