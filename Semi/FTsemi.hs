{-# LANGUAGE CPP, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}

module FTsemi where

import Prelude hiding (null)
import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Control.DeepSeq
import Data.Foldable (Foldable(foldMap), toList)
import Data.Monoid
import Data.Semigroup
import qualified Data.Set as S

data FingerTree set elem
    = Empty
    | Single elem
    | Deep   !set !(Digit elem) (FingerTree set (Node set elem)) !(Digit elem) deriving Show
{-
instance (Show a) => Show (FingerTree v a) where
    show Empty             = "[X]"
    show (Single x)        = "["++show x++"]"
    show (Deep _ pr mi sf) = "{"++show pr++";"++show mi++";"++show sf++"}"
-}
instance (Eq elem) => Eq (FingerTree set elem) where
-- toList is called from Data.Foldable
    xs == ys = toList xs == toList ys

data Digit  elem
    = One   elem
    | Two   elem elem
    | Three elem elem elem
    | Four  elem elem elem elem deriving Show

data Node set elem
    = Node2 !set elem elem
    | Node3 !set elem elem elem deriving Show
{-
instance (Show a) => Show (Node v a) where
   show (Node2 v x y)   = "N2(" ++ show x ++ ", " ++ show y ++ ")"
   show (Node3 v x y z) = "N3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
-}
class (Monoid set) => Edges set elem | elem -> set where
    edges :: elem -> set
{-
EXCLUSIVE OF THE APPLICATION SUCH IN semi.hs
instance (Ord elem) => Edges (S.Set (elem,elem)) (elem,elem) where 
    edges (x,y) = S.insert (x,y) S.empty 
-}
instance (Edges set elem) => Edges set (FingerTree set elem) where
    edges Empty            = mempty
    edges (Single x)       = edges x
    edges (Deep s pr mi sf) = edges pr <> edges mi <> edges sf 
{-
seto :: (FingerTree set elem) -> set
--seto (Empty) = mempty
seto (Deep _ pr mi sf) = pr <> mi <> sf
-}
instance (Monoid set) => Edges set (Node set elem) where
    edges (Node2 set _ _)   = set
    edges (Node3 set _ _ _) = set

instance (Edges set elem) => Edges set (Digit elem) where
    edges = foldMap edges

instance Foldable Digit where
    foldMap f (One   w)       = f w
    foldMap f (Two   w x)     = f w <> f x
    foldMap f (Three w x y)   = f w <> f x <> f y
    foldMap f (Four  w x y z) = f w <> f x <> f y <> f z

ft1,fte,fts :: FingerTree (S.Set (Int,Int)) (Int,Int)
ft1 =  Deep (S.fromList (zip[1..][1..10])) (Two (1,1)(2,2)) Empty (Three (3,3)(4,4)(5,5))
fte = Empty
fts =  Single (1,1)

data ViewL tree elem
    = EmptyL            
    | elem :< tree elem 
    deriving (Eq, Ord, Show, Read)

data ViewR tree elem
    = EmptyR            
    | tree elem :> elem 
    deriving (Eq, Ord, Show, Read)

instance (Functor tree) => Functor (ViewL tree) where
    fmap _ EmptyL    = EmptyL
    fmap f (x :< xs) = f x :< fmap f xs

instance (Functor tree) => Functor (ViewR tree) where
    fmap _ EmptyR    = EmptyR
    fmap f (xs :> x) = fmap f xs :> f x

instance (Edges set elem) => Semigroup (FingerTree set elem) where
    (<>) = (><)

instance (Edges set elem) => Monoid (FingerTree set elem) where
    mempty  = empty
    mappend = (><)

instance NFData elem => NFData (FingerTree set elem) where
    rnf (Empty)           = ()
    rnf (Single x)        = rnf x
    rnf (Deep _ pr mi sf) = rnf pr `seq` rnf mi `seq` rnf sf

instance NFData elem => NFData (Digit elem) where
    rnf (One   w)       = rnf w
    rnf (Two   w x)     = rnf w `seq` rnf x
    rnf (Three w x y)   = rnf w `seq` rnf x `seq` rnf y
    rnf (Four  w x y z) = rnf w `seq` rnf x `seq` rnf y `seq` rnf z

instance NFData elem => NFData (Node set elem) where
    rnf (Node2 _ x y)   = rnf x `seq` rnf y
    rnf (Node3 _ x y z) = rnf x `seq` rnf y `seq` rnf z

instance Foldable (Node set) where
    foldMap f (Node2 _ x y)   = f x <> f y
    foldMap f (Node3 _ x y z) = f x <> f y <> f z

node2 :: (Edges set elem) => elem -> elem -> Node set elem
node2 x y = Node2 (edges x <> edges y) x y

node3 :: (Edges set elem) => elem -> elem -> elem -> Node set elem
node3 x y z = Node3 (edges x <> edges y <> edges z) x y z


nodeToDigit :: Node set elem -> Digit elem
nodeToDigit (Node2 _ x y)   = Two   x y
nodeToDigit (Node3 _ x y z) = Three x y z

deep :: (Edges set elem) => Digit elem -> FingerTree set (Node set elem) -> Digit elem -> FingerTree set elem
deep pr mi sf = Deep mempty pr mi sf

-- | Elements from left to right.
instance Foldable (FingerTree set) where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Deep _ pr mi sf) = foldMap f pr <> foldMap (foldMap f) mi <> foldMap f sf

-- | Lexicographical order from left to right.
instance (Ord elem) => Ord (FingerTree set elem) where
-- toList is called from Data.Foldable
    compare xs ys = compare (toList xs) (toList ys)

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

-- | /O(1)/. Is this the empty sequence?
null :: FingerTree set elem -> Bool
null Empty = True
null _     = False

-- | /O(1)/. The empty sequence.
empty :: Edges set elem => FingerTree set elem
empty =  Empty

-- | /O(1)/. A singleton sequence.
singleton :: Edges set elem => elem -> FingerTree set elem
singleton =  Single

-- | /O(n)/. Create a sequence from a finite list of elements.
-- The opposite operation 'toList' is supplied by the 'Foldable' instance.
fromList :: (Edges set elem) => [elem] -> FingerTree set elem
fromList = foldr (<|) Empty

-- | /O(1)/. Add an element to the left end of a tree
(<|) :: (Edges set elem) => elem -> FingerTree set elem -> FingerTree set elem
v <| Empty                       =  Single v
v <| Single w                    =  deep (One v) Empty (One w)
-- we ignore the set construction through _ (wildcard)
v <| Deep _ (Four w x y z) mi sf = mi `seq` Deep mempty (Two v w)        (node3 x y z <| mi) sf
v <| Deep _ pr             mi sf = mi `seq` Deep mempty (consDigit v pr) mi                  sf

consDigit :: elem -> Digit elem -> Digit elem
consDigit w (One   x)       = Two   w x
consDigit w (Two   x y)     = Three w x y
consDigit w (Three x y z)   = Four  w x y z 
consDigit _ (Four  _ _ _ _) = illegal_argument "consDigit"

-- | /O(1)/. Add an element to the right end of a tree
(|>) :: (Edges set elem) => FingerTree set elem -> elem -> FingerTree set elem
Empty    |> z  =  Single z
Single y |> z  =  deep (One y) Empty (One z)
Deep   _ pr mi (Four v w x y) |> z = mi `seq` Deep mempty pr (mi |> node3 v w x) (Two y z)
Deep   _ pr mi sf             |> z = mi `seq` Deep mempty pr mi                  (snocDigit sf z)

snocDigit :: Digit elem -> elem -> Digit elem
snocDigit (One   w)       x = Two   w x
snocDigit (Two   w x)     y = Three w x y
snocDigit (Three w x y)   z = Four  w x y z
snocDigit (Four  _ _ _ _) _ = illegal_argument "snocDigit"

-- | /O(1)/. Analyse the left end of a sequence.
viewl :: (Edges set elem) => FingerTree set elem -> ViewL (FingerTree set) elem
viewl Empty                  = EmptyL
viewl (Single x)             = x :< Empty
viewl (Deep _ (One x) mi sf) = x :< rotL mi sf
viewl (Deep _ pr      mi sf) = lheadDigit pr :< deep (ltailDigit pr) mi sf

rotL :: (Edges set elem) => FingerTree set (Node set elem) -> Digit elem -> FingerTree set elem
rotL mi sf = case viewl mi of
    EmptyL   -> digitToTree sf
    x :< mi' -> Deep mempty (nodeToDigit x) mi' sf

lheadDigit :: Digit elem -> elem
lheadDigit (One   x)       = x
lheadDigit (Two   x _)     = x
lheadDigit (Three x _ _)   = x
lheadDigit (Four  x _ _ _) = x

ltailDigit :: Digit elem -> Digit elem
ltailDigit (One   _)       = illegal_argument "ltailDigit"
ltailDigit (Two   _ x)     = One   x
ltailDigit (Three _ x y)   = Two   x y
ltailDigit (Four  _ x y z) = Three x y z

-- | /O(1)/. Analyse the right end of a sequence.
viewr :: (Edges set elem) => FingerTree set elem -> ViewR (FingerTree set) elem
viewr Empty                  =  EmptyR
viewr (Single x)             =  Empty :> x
viewr (Deep _ pr mi (One x)) =  rotR pr mi :> x
viewr (Deep _ pr mi sf)      =  deep pr mi (rtailDigit sf) :> rheadDigit sf

rotR :: (Edges set elem) => Digit elem -> FingerTree set (Node set elem) -> FingerTree set elem
rotR pr mi = case viewr mi of
    EmptyR   -> digitToTree pr
    mi' :> x -> Deep mempty pr mi' (nodeToDigit x)

rheadDigit :: Digit elem -> elem
rheadDigit (One   x)       = x
rheadDigit (Two   _ x)     = x
rheadDigit (Three _ _ x)   = x
rheadDigit (Four  _ _ _ x) = x

rtailDigit :: Digit elem -> Digit elem
rtailDigit (One   _)       = illegal_argument "rtailDigit"
rtailDigit (Two   x _)     = One   x
rtailDigit (Three x y _)   = Two   x y
rtailDigit (Four  x y z _) = Three x y z

digitToTree :: (Edges set elem) => Digit elem -> FingerTree set elem
digitToTree (One   w)       = Single w
digitToTree (Two   w x)     = deep (One w)   Empty (One x)
digitToTree (Three w x y)   = deep (Two w x) Empty (One y)
digitToTree (Four  w x y z) = deep (Two w x) Empty (Two y z)

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><) :: (Edges set elem) => FingerTree set elem -> FingerTree set elem -> FingerTree set elem
(><) = appendTree0

appendTree0 :: (Edges set elem) => FingerTree set elem -> FingerTree set elem -> FingerTree set elem
appendTree0 Empty               xs                   =  xs
appendTree0 xs                  Empty                =  xs
appendTree0 (Single x)          xs                   =  x <| xs
appendTree0 xs                  (Single x)           =  xs |> x
appendTree0 (Deep _ pr1 m1 sf1) (Deep _ pr2 m2 sf2)  =  deep pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: (Edges set elem) => FingerTree set (Node set elem) -> Digit elem -> Digit elem -> FingerTree set (Node set elem) -> FingerTree set (Node set elem)
addDigits0 m1 (One a)        (One b)        m2 = appendTree1 m1 (node2 a b)   m2
addDigits0 m1 (One a)        (Two b c)      m2 = appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a)        (Three b c d)  m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits0 m1 (One a)        (Four b c d e) m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits0 m1 (Two a b)      (One c)        m2 = appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b)      (Two c d)      m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits0 m1 (Two a b)      (Three c d e)  m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits0 m1 (Two a b)      (Four c d e f) m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c)  (One d)        m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits0 m1 (Three a b c)  (Two d e)      m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits0 m1 (Three a b c)  (Three d e f)  m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c)  (Four d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits0 m1 (Four a b c d) (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: (Edges set elem) => FingerTree set elem -> elem -> FingerTree set elem -> FingerTree set elem
appendTree1 Empty               a xs                  = a <| xs
appendTree1 xs                  a Empty               = xs |> a
appendTree1 (Single x)          a xs                  = x <| a <| xs
appendTree1 xs                  a (Single x)          = xs |> a |> x
appendTree1 (Deep _ pr1 m1 sf1) a (Deep _ pr2 m2 sf2) = deep pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: (Edges set elem) => FingerTree set (Node set elem) -> Digit elem -> elem -> Digit elem -> FingerTree set (Node set elem) -> FingerTree set (Node set elem)
addDigits1 m1 (One a)        b (One c)        m2 = appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a)        b (Two c d)      m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits1 m1 (One a)        b (Three c d e)  m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits1 m1 (One a)        b (Four c d e f) m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b)      c (One d)        m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits1 m1 (Two a b)      c (Two d e)      m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits1 m1 (Two a b)      c (Three d e f)  m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b)      c (Four d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits1 m1 (Three a b c)  d (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits1 m1 (Three a b c)  d (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c)  d (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits1 m1 (Three a b c)  d (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits1 m1 (Four a b c d) e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits1 m1 (Four a b c d) e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: (Edges set elem) => FingerTree set elem -> elem -> elem -> FingerTree set elem -> FingerTree set elem
appendTree2 Empty               a b xs                  = a <| b <| xs
appendTree2 xs                  a b Empty               = xs |> a |> b
appendTree2 (Single x)          a b xs                  = x <| a <| b <| xs
appendTree2 xs                  a b (Single x)          = xs |> a |> b |> x
appendTree2 (Deep _ pr1 m1 sf1) a b (Deep _ pr2 m2 sf2) = deep pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: (Edges set elem) => FingerTree set (Node set elem) -> Digit elem -> elem -> elem -> Digit elem -> FingerTree set (Node set elem) -> FingerTree set (Node set elem)
addDigits2 m1 (One a)        b c (One d)        m2 = appendTree2 m1 (node2 a b)   (node2 c d)   m2
addDigits2 m1 (One a)        b c (Two d e)      m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits2 m1 (One a)        b c (Three d e f)  m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a)        b c (Four d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits2 m1 (Two a b)      c d (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits2 m1 (Two a b)      c d (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b)      c d (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits2 m1 (Two a b)      c d (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits2 m1 (Three a b c)  d e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c)  d e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits2 m1 (Three a b c)  d e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits2 m1 (Three a b c)  d e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g)        m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits2 m1 (Four a b c d) e f (Two g h)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits2 m1 (Four a b c d) e f (Three g h i)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j) m2

appendTree3 :: (Edges set elem) => FingerTree set elem -> elem -> elem -> elem -> FingerTree set elem -> FingerTree set elem
appendTree3 Empty               a b c xs                  = a <| b <| c <| xs
appendTree3 xs                  a b c Empty               = xs |> a |> b |> c
appendTree3 (Single x)          a b c xs                  = x <| a <| b <| c <| xs
appendTree3 xs                  a b c (Single x)          = xs |> a |> b |> c |> x
appendTree3 (Deep _ pr1 m1 sf1) a b c (Deep _ pr2 m2 sf2) = deep pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: (Edges set elem) => FingerTree set (Node set elem) -> Digit elem -> elem -> elem -> elem -> Digit elem -> FingerTree set (Node set elem) -> FingerTree set (Node set elem)
addDigits3 m1 (One a)        b c d (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e)   m2
addDigits3 m1 (One a)        b c d (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a)        b c d (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits3 m1 (One a)        b c d (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits3 m1 (Two a b)      c d e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b)      c d e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits3 m1 (Two a b)      c d e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits3 m1 (Two a b)      c d e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c)  d e f (One g)        m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits3 m1 (Three a b c)  d e f (Two g h)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits3 m1 (Three a b c)  d e f (Three g h i)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c)  d e f (Four g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h)        m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits3 m1 (Four a b c d) e f g (Two h i)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j)  m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: (Edges set elem) => FingerTree set elem -> elem -> elem -> elem -> elem -> FingerTree set elem -> FingerTree set elem
appendTree4 Empty               a b c d xs                  = a <| b <| c <| d <| xs
appendTree4 xs                  a b c d Empty               = xs |> a |> b |> c |> d
appendTree4 (Single x)          a b c d xs                  = x <| a <| b <| c <| d <| xs
appendTree4 xs                  a b c d (Single x)          = xs |> a |> b |> c |> d |> x
appendTree4 (Deep _ pr1 m1 sf1) a b c d (Deep _ pr2 m2 sf2) = deep pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: (Edges set elem) => FingerTree set (Node set elem) -> Digit elem -> elem -> elem -> elem -> elem -> Digit elem -> FingerTree set (Node set elem) -> FingerTree set (Node set elem)
addDigits4 m1 (One a)        b c d e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a)        b c d e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits4 m1 (One a)        b c d e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits4 m1 (One a)        b c d e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b)      c d e f (One g)        m2 = appendTree3 m1 (node3 a b c) (node2 d e)   (node2 f g)   m2
addDigits4 m1 (Two a b)      c d e f (Two g h)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits4 m1 (Two a b)      c d e f (Three g h i)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b)      c d e f (Four g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j)   m2
addDigits4 m1 (Three a b c)  d e f g (One h)        m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h)   m2
addDigits4 m1 (Three a b c)  d e f g (Two h i)      m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c)  d e f g (Three h i j)  m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j)   m2
addDigits4 m1 (Three a b c)  d e f g (Four h i j k) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k)   m2
addDigits4 m1 (Four a b c d) e f g h (One i)        m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j)      m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h)   (node2 i j)   m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k)  m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k)   m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

deepL :: (Edges set elem) => Maybe (Digit elem) -> FingerTree set (Node set elem) -> Digit elem -> FingerTree set elem
deepL Nothing   mi sf = rotL mi sf
deepL (Just pr) mi sf = deep pr mi sf

deepR :: (Edges set elem) => Digit elem -> FingerTree set (Node set elem) -> Maybe (Digit elem) -> FingerTree set elem
deepR pr mi Nothing   = rotR pr mi
deepR pr mi (Just sf) = deep pr mi sf

data SearchResult set elem
    = Position (FingerTree set elem) elem (FingerTree set elem)
    | OnLeft
    | OnRight
    | Nowhere
    deriving (Eq, Ord, Show)

data Split tree elem = Split tree elem tree deriving Show

data Built tree elem
    = NoBuilt
    | Built tree elem tree deriving Show

{-
search :: (Edges set elem) => (set -> Bool) -> FingerTree set elem -> Built (FingerTree set elem) elem
search _ Empty      = NoBuilt 
search p (Single x)
   | p (edges x) = Built Empty x Empty
   | otherwise     = NoBuilt
search p (Deep _ pr m sf) 
  | p vl =  let  Split l x r     =  searchD p pr 
            in   Built (maybe Empty digitToTree l) x (deepL r m sf)
  | p vr =  let  Split l x r     =  searchD p sf 
            in   Built (deepR pr  m  l) x (maybe Empty digitToTree r)
  | otherwise =  case (search p m) of
                   NoBuilt        -> NoBuilt
                   Built ml xs mr -> 
                     let  Split l x r     =  searchN p xs
                     in   Built (deepR pr ml l) x (deepL r mr sf) 
  where
    vl  = edges pr
    vr  = edges sf

searchD :: (Edges set elem) => (set -> Bool) -> Digit elem -> Split (Maybe (Digit elem)) elem
searchD _ (One w)   = Split Nothing w Nothing
searchD p (Two w x) 
  | p (edges w) = Split Nothing        w (Just (One x))
  | otherwise   = Split (Just (One w)) x Nothing
searchD p (Three w x y) 
  | p (edges w) = Split Nothing          w (Just (Two x y))
  | p (edges x) = Split (Just (One w))   x (Just (One y))
  | otherwise   = Split (Just (Two w x)) y Nothing
searchD p (Four w x y z) 
  | p (edges w) = Split Nothing               w (Just (Three x y z))
  | p (edges x) = Split (Just (One   w))      x (Just (Two   y z))
  | p (edges y) = Split (Just (Two   w x))    y (Just (One   z))
  | otherwise   = Split (Just (Three w x y))  z Nothing

searchN :: (Edges set elem) => (set -> Bool) -> Node set elem -> Split (Maybe (Digit elem)) elem
searchN p (Node2 _ x y) 
  | p (edges x) = Split Nothing         x  (Just (One y))
  | otherwise   = Split (Just (One x))  y  Nothing
searchN p (Node3 _ x y z) 
  | p (edges x) = Split Nothing          x (Just (Two y z))
  | p (edges y) = Split (Just (One x))   y (Just (One z))
  | otherwise   = Split (Just (Two x y)) z Nothing

illegal_argument :: String -> elem
illegal_argument name = error $ "Logic error: " ++ name ++ " called with illegal argument"

instance Functor (FingerTree a) where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Deep v pr m sf) =
        Deep v (fmap f pr) (fmap (fmap f) m) (fmap f sf)

instance Functor Digit where
    fmap f (One a) = One (f a)
    fmap f (Two a b) = Two (f a) (f b)
    fmap f (Three a b c) = Three (f a) (f b) (f c)
    fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)

instance Functor (Node a) where
    fmap f (Node2 v a b) = Node2 v (f a) (f b)
    fmap f (Node3 v a b c) = Node3 v (f a) (f b) (f c)
-}

search :: (Edges v a) => (        v ->         v -> Bool) -> FingerTree v a -> SearchResult v a
search p t
  | p_left     && p_right = OnLeft
  | not p_left && p_right = case searchTree p mempty t mempty of
        Split l x r -> Position l x r
  | not p_left && not p_right = OnRight
  | otherwise = Nowhere
  where
    p_left  = p mempty vt
    p_right = p vt mempty
    vt      = edges t

searchTree :: (Edges v a) => (         v ->          v -> Bool) ->         v -> FingerTree v a ->          v -> Split (FingerTree v a) a
searchTree _ _ Empty _ = illegal_argument "searchTree"
searchTree _ _ (Single x) _ = Split Empty x Empty
searchTree p vl (Deep _ pr m sf) vr
  | p vlp vmsr  =  let  Split l x r     =  searchDigit p vl pr vmsr
                   in   Split (maybe Empty digitToTree l) x (deepL r m sf)
  | p vlpm vsr  =  let  Split ml xs mr  =  searchTree p vlp m vsr
                        Split l x r     =  searchNode p (vlp `mappend` edges ml) xs (edges mr `mappend` vsr)
                   in   Split (deepR pr  ml l) x (deepL r mr sf)
  | otherwise   =  let  Split l x r     =  searchDigit p vlpm sf vr
                   in   Split (deepR pr  m  l) x (maybe Empty digitToTree r)
  where
    vlp     =  vl `mappend` edges pr
    vlpm    =  vlp `mappend` vm
    vmsr    =  vm `mappend` vsr
    vsr     =  edges sf `mappend` vr
    vm      =  edges m

searchNode :: (Edges v a) => (        v ->         v -> Bool) ->         v -> Node v a ->         v -> Split (Maybe (Digit a)) a
searchNode p vl (Node2 _ a b) vr
  | p va vb     = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va      = vl `mappend` edges a
    vb      = edges b `mappend` vr
searchNode p vl (Node3 _ a b c) vr
  | p va vbc    = Split Nothing a (Just (Two b c))
  | p vab vc    = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    va      = vl `mappend` edges a
    vab     = va `mappend` edges b
    vc      = edges c `mappend` vr
    vbc     = edges b `mappend` vc

searchDigit :: (Edges v a) => (        v ->         v -> Bool) ->         v -> Digit a ->         v -> Split (Maybe (Digit a)) a
searchDigit _ vl (One a) vr = vl `seq` vr `seq` Split Nothing a Nothing
searchDigit p vl (Two a b) vr
  | p va vb     = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    va      = vl `mappend` edges a
    vb      = edges b `mappend` vr
searchDigit p vl (Three a b c) vr
  | p va vbc    = Split Nothing a (Just (Two b c))
  | p vab vc    = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    va      = vl `mappend` edges a
    vab     = va `mappend` edges b
    vbc     = edges b `mappend` vc
    vc      = edges c `mappend` vr
searchDigit p vl (Four a b c d) vr
  | p va vbcd   = Split Nothing a (Just (Three b c d))
  | p vab vcd   = Split (Just (One a)) b (Just (Two c d))
  | p vabc vd   = Split (Just (Two a b)) c (Just (One d))
  | otherwise   = Split (Just (Three a b c)) d Nothing
  where
    va      = vl `mappend` edges a
    vab     = va `mappend` edges b
    vabc    = vab `mappend` edges c
    vbcd    = edges b `mappend` vcd
    vcd     = edges c `mappend` vd
    vd      = edges d `mappend` vr

illegal_argument :: String -> a
illegal_argument name =
    error $ "Logic error: " ++ name ++ " called with illegal argument"
