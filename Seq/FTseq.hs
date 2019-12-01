{-# LANGUAGE CPP, DeriveDataTypeable #-} 

module FTseq where

import Prelude hiding (Functor(..), null, length )
import qualified Data.List
import Control.Applicative (Applicative(..), (<$>), Alternative, WrappedMonad(..), liftA, liftA2, liftA3)
import qualified Control.Applicative as Applicative (Alternative(..))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (MonadPlus(..), ap)
--import Data.Monoid (Monoid(..))
import Data.Functor (Functor(..))
import Data.Foldable hiding(length,null)
import Data.Traversable
import Data.Typeable

newtype Elem a = Elem { getElem :: a } deriving Show
newtype Seq a = Seq (FingerTree (Elem a))  deriving Show

data FingerTree a
    = Empty
    | Single a
    | Deep   !Int       !(Digit a) (FingerTree (Node a)) !(Digit a) deriving Show

data Digit  a
    = One   a
    | Two   a a
    | Three a a a
    | Four  a a a a    deriving Show

data Node a
    = Node2 !Int       a a
    | Node3 !Int       a a a  deriving Show

class Sized a where
     size :: a -> Int

instance Sized a => Sized (FingerTree a) where
    size Empty              = 0
    size (Single x)         = size x
    size (Deep v _ _ _)     = v

instance Sized a => Sized (Digit a) where
    size = foldl1 (+) . fmap size

instance Sized (Node a) where
    size (Node2 v _ _)      = v
    size (Node3 v _ _ _)    = v

instance Sized (Elem a) where
    size _ = 1

instance Functor Elem where
    fmap f (Elem x) = Elem (f x)

instance Foldable Elem where
    foldr f z (Elem x) = f x z
    foldl f z (Elem x) = f z x

instance Traversable Elem where
    traverse f (Elem x) = Elem <$> f x

instance NFData a => NFData (Elem a) where
    rnf (Elem x) = rnf x

instance Eq a => Eq (Seq a) where
    xs == ys = length xs == length ys && toList xs == toList ys

instance Ord a => Ord (Seq a) where
    compare xs ys = compare (toList xs) (toList ys)

instance Functor Seq where
    fmap f (Seq xs) = Seq (fmap (fmap f) xs)

instance Foldable Seq where
    foldr f z (Seq xs) = foldr (flip (foldr f)) z xs
    foldl f z (Seq xs) = foldl (foldl f) z xs

    foldr1 f (Seq xs) = getElem (foldr1 f' xs)
      where f' (Elem x) (Elem y) = Elem (f x y)

    foldl1 f (Seq xs) = getElem (foldl1 f' xs)
      where f' (Elem x) (Elem y) = Elem (f x y)

instance Traversable Seq where
    traverse f (Seq xs) = Seq <$> traverse (traverse f) xs

instance NFData a => NFData (Seq a) where
    rnf (Seq xs) = rnf xs

instance Monad Seq where
    return = singleton
    xs >>= f = foldl' add empty xs
      where add ys x = ys >< f x

instance Applicative Seq where
    pure = singleton
    fs <*> xs = foldl' add empty fs
      where add ys f = ys >< fmap f xs

instance MonadPlus Seq where
    mzero = empty
    mplus = (><)

instance Alternative Seq where
    empty = empty
    (<|>) = (><)
{-
#if TESTING
instance Show a => Show (Seq a) where
    showsPrec p (Seq x) = showsPrec p x
#else
instance Show a => Show (Seq a) where
    showsPrec p xs = showParen (p > 10) $
        showString "Sequence " . shows (toList xs)
#endif
-}
instance Semigroup (Seq a) where
   (<>) = (><)

instance Monoid (Seq a) where
    mempty = empty
    mappend = (><)

instance Foldable FingerTree where
    foldr _ z Empty = z
    foldr f z (Single x) = x `f` z
    foldr f z (Deep _ pr m sf) =
        foldr f (foldr (flip (foldr f)) (foldr f z sf) m) pr

    foldl _ z Empty = z
    foldl f z (Single x) = z `f` x
    foldl f z (Deep _ pr m sf) =
        foldl f (foldl (foldl f) (foldl f z pr) m) sf

    foldr1 _ Empty = error "foldr1: empty sequence"
    foldr1 _ (Single x) = x
    foldr1 f (Deep _ pr m sf) =
        foldr f (foldr (flip (foldr f)) (foldr1 f sf) m) pr

    foldl1 _ Empty = error "foldl1: empty sequence"
    foldl1 _ (Single x) = x
    foldl1 f (Deep _ pr m sf) =
        foldl f (foldl (foldl f) (foldl1 f pr) m) sf

instance Functor FingerTree where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Deep v pr m sf) =
        Deep v (fmap f pr) (fmap (fmap f) m) (fmap f sf)

instance Traversable FingerTree where
    traverse _ Empty = pure Empty
    traverse f (Single x) = Single <$> f x
    traverse f (Deep v pr m sf) =
        Deep v <$> traverse f pr <*> traverse (traverse f) m <*>
            traverse f sf

instance NFData a => NFData (FingerTree a) where
    rnf (Empty) = ()
    rnf (Single x) = rnf x
    rnf (Deep _ pr m sf) = rnf pr `seq` rnf m `seq` rnf sf

deep            :: Sized a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deep pr m sf    =  Deep (size pr + size m + size sf) pr m sf

pullL :: Sized a => Int -> FingerTree (Node a) -> Digit a -> FingerTree a
pullL s m sf = case viewLTree m of
    Nothing2        -> digitToTree' s sf
    Just2 pr m'     -> Deep s (nodeToDigit pr) m' sf

pullR :: Sized a => Int -> Digit a -> FingerTree (Node a) -> FingerTree a
pullR s pr m = case viewRTree m of
    Nothing2        -> digitToTree' s pr
    Just2 m' sf     -> Deep s pr m' (nodeToDigit sf)

deepL :: Sized a => Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing m sf      = pullL (size m + size sf) m sf
deepL (Just pr) m sf    = deep pr m sf

deepR :: Sized a => Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR pr m Nothing      = pullR (size m + size pr) pr m
deepR pr m (Just sf)    = deep pr m sf


instance Foldable Digit where
    foldr f z (One a) = a `f` z
    foldr f z (Two a b) = a `f` (b `f` z)
    foldr f z (Three a b c) = a `f` (b `f` (c `f` z))
    foldr f z (Four a b c d) = a `f` (b `f` (c `f` (d `f` z)))

    foldl f z (One a) = z `f` a
    foldl f z (Two a b) = (z `f` a) `f` b
    foldl f z (Three a b c) = ((z `f` a) `f` b) `f` c
    foldl f z (Four a b c d) = (((z `f` a) `f` b) `f` c) `f` d

    foldr1 _ (One a) = a
    foldr1 f (Two a b) = a `f` b
    foldr1 f (Three a b c) = a `f` (b `f` c)
    foldr1 f (Four a b c d) = a `f` (b `f` (c `f` d))

    foldl1 _ (One a) = a
    foldl1 f (Two a b) = a `f` b
    foldl1 f (Three a b c) = (a `f` b) `f` c
    foldl1 f (Four a b c d) = ((a `f` b) `f` c) `f` d

instance Functor Digit where
    fmap f (One a) = One (f a)
    fmap f (Two a b) = Two (f a) (f b)
    fmap f (Three a b c) = Three (f a) (f b) (f c)
    fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)

instance Traversable Digit where
    traverse f (One a) = One <$> f a
    traverse f (Two a b) = Two <$> f a <*> f b
    traverse f (Three a b c) = Three <$> f a <*> f b <*> f c
    traverse f (Four a b c d) = Four <$> f a <*> f b <*> f c <*> f d

instance NFData a => NFData (Digit a) where
    rnf (One a) = rnf a
    rnf (Two a b) = rnf a `seq` rnf b
    rnf (Three a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (Four a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

digitToTree     :: Sized a => Digit a -> FingerTree a
digitToTree (One a) = Single a
digitToTree (Two a b) = deep (One a) Empty (One b)
digitToTree (Three a b c) = deep (Two a b) Empty (One c)
digitToTree (Four a b c d) = deep (Two a b) Empty (Two c d)

digitToTree' :: Int -> Digit a -> FingerTree a
digitToTree' n (Four a b c d) = Deep n (Two a b) Empty (Two c d)
digitToTree' n (Three a b c) = Deep n (Two a b) Empty (One c)
digitToTree' n (Two a b) = Deep n (One a) Empty (One b)
digitToTree' n (One a) = n `seq` Single a

instance Foldable Node where
    foldr f z (Node2 _ a b) = a `f` (b `f` z)
    foldr f z (Node3 _ a b c) = a `f` (b `f` (c `f` z))

    foldl f z (Node2 _ a b) = (z `f` a) `f` b
    foldl f z (Node3 _ a b c) = ((z `f` a) `f` b) `f` c

instance Functor Node where
    fmap f (Node2 v a b) = Node2 v (f a) (f b)
    fmap f (Node3 v a b c) = Node3 v (f a) (f b) (f c)

instance Traversable Node where
    traverse f (Node2 v a b) = Node2 v <$> f a <*> f b
    traverse f (Node3 v a b c) = Node3 v <$> f a <*> f b <*> f c

instance NFData a => NFData (Node a) where
    rnf (Node2 _ a b) = rnf a `seq` rnf b
    rnf (Node3 _ a b c) = rnf a `seq` rnf b `seq` rnf c

node2           :: Sized a => a -> a -> Node a
node2 a b       =  Node2 (size a + size b) a b

node3           :: Sized a => a -> a -> a -> Node a
node3 a b c     =  Node3 (size a + size b + size c) a b c

nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c

-------------------------------------------------------
-- Applicative construction
-------------------------------------------------------

newtype Id a = Id {runId :: a}

instance Functor Id where
    fmap f (Id x) = Id (f x)

instance Monad Id where
    return = Id
    m >>= k = k (runId m)

instance Applicative Id where
    pure = return
    (<*>) = ap

-- | This is essentially a clone of Control.Monad.State.Strict.
newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
    fmap = liftA

instance Monad (State s) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return x = State $ \ s -> (s, x)
    m >>= k = State $ \ s -> case runState m s of
        (s', x) -> runState (k x) s'

instance Applicative (State s) where
    pure = return
    (<*>) = ap

execState :: State s a -> s -> a
execState m x = snd (runState m x)

-- | A helper method: a strict version of mapAccumL.
mapAccumL' :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL' f s t = runState (traverse (State . flip f) t) s

-- | 'applicativeTree' takes an Applicative-wrapped construction of a
-- piece of a FingerTree, assumed to always have the same size (which
-- is put in the second argument), and replicates it as many times as
-- specified.  This is a generalization of 'replicateA', which itself
-- is a generalization of many Data.Sequence methods.
{-# SPECIALIZE applicativeTree :: Int -> Int -> State s a -> State s (FingerTree a) #-}
{-# SPECIALIZE applicativeTree :: Int -> Int -> Id a -> Id (FingerTree a) #-}
-- Special note: the Id specialization automatically does node sharing,
-- reducing memory usage of the resulting tree to /O(log n)/.
applicativeTree :: Applicative f => Int -> Int -> f a -> f (FingerTree a)
applicativeTree n mSize m = mSize `seq` case n of
    0 -> pure Empty
    1 -> liftA Single m
    2 -> deepA one emptyTree one
    3 -> deepA two emptyTree one
    4 -> deepA two emptyTree two
    5 -> deepA three emptyTree two
    6 -> deepA three emptyTree three
    7 -> deepA four emptyTree three
    8 -> deepA four emptyTree four
    _ -> let (q, r) = n `quotRem` 3 in q `seq` case r of
        0 -> deepA three (applicativeTree (q - 2) mSize' n3) three
        1 -> deepA four (applicativeTree (q - 2) mSize' n3) three
        _ -> deepA four (applicativeTree (q - 2) mSize' n3) four
  where
    one = liftA One m
    two = liftA2 Two m m
    three = liftA3 Three m m m
    four = liftA3 Four m m m <*> m
    deepA = liftA3 (Deep (n * mSize))
    mSize' = 3 * mSize
    n3 = liftA3 (Node3 mSize') m m m
    emptyTree = pure Empty

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------
infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>
infixr 5 `consTree`
infixl 5 `snocTree`

-- | /O(1)/. The empty sequence.
empty           :: Seq a
empty           =  Seq Empty

-- | /O(1)/. A singleton sequence.
singleton       :: a -> Seq a
singleton x     =  Seq (Single (Elem x))

(<|)            :: a -> Seq a -> Seq a
x <| Seq xs     =  Seq (Elem x `consTree` xs)

consTree        :: Sized a => a -> FingerTree a -> FingerTree a
consTree a Empty                        = Single a
consTree a (Single b)                   = deep (One a) Empty (One b)
consTree a (Deep s (Four b c d e) m sf) = m `seq` Deep (size a + s) (Two a b) (node3 c d e `consTree` m) sf
consTree a (Deep s (Three b c d) m sf)  = Deep (size a + s) (Four a b c d) m sf
consTree a (Deep s (Two b c) m sf)      = Deep (size a + s) (Three a b c) m sf
consTree a (Deep s (One b) m sf)        = Deep (size a + s) (Two a b) m sf

(|>)            :: Seq a -> a -> Seq a
Seq xs |> x     =  Seq (xs `snocTree` Elem x)

snocTree        :: Sized a => FingerTree a -> a -> FingerTree a
snocTree Empty a                        = Single a
snocTree (Single a) b                   = deep (One a) Empty (One b)
snocTree (Deep s pr m (Four a b c d)) e = m `seq` Deep (s + size e) pr (m `snocTree` node3 a b c) (Two d e)
snocTree (Deep s pr m (Three a b c)) d  = Deep (s + size d) pr m (Four a b c d)
snocTree (Deep s pr m (Two a b)) c      = Deep (s + size c) pr m (Three a b c)
snocTree (Deep s pr m (One a)) b        = Deep (s + size b) pr m (Two a b)

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><)            :: Seq a -> Seq a -> Seq a
Seq xs >< Seq ys = Seq (appendTree0 xs ys)

-- The appendTree/addDigits gunk below is machine generated

appendTree0 :: FingerTree (Elem a) -> FingerTree (Elem a) -> FingerTree (Elem a)
appendTree0 Empty xs = xs
appendTree0 xs Empty = xs
appendTree0 (Single x) xs = x `consTree` xs
appendTree0 xs (Single x) = xs `snocTree` x
appendTree0 (Deep s1 pr1 m1 sf1) (Deep s2 pr2 m2 sf2) = Deep (s1 + s2) pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: FingerTree (Node (Elem a)) -> Digit (Elem a) -> Digit (Elem a) -> FingerTree (Node (Elem a)) -> FingerTree (Node (Elem a))
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

appendTree1 :: FingerTree (Node a) -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree1 Empty a xs =    a `consTree` xs
appendTree1 xs a Empty =    xs `snocTree` a
appendTree1 (Single x) a xs =    x `consTree` a `consTree` xs
appendTree1 xs a (Single x) =    xs `snocTree` a `snocTree` x
appendTree1 (Deep s1 pr1 m1 sf1) a (Deep s2 pr2 m2 sf2) =    Deep (s1 + size a + s2) pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits1 m1 (One a) b (One c) m2 =    appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: FingerTree (Node a) -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree2 Empty a b xs =    a `consTree` b `consTree` xs
appendTree2 xs a b Empty =    xs `snocTree` a `snocTree` b
appendTree2 (Single x) a b xs =    x `consTree` a `consTree` b `consTree` xs
appendTree2 xs a b (Single x) =    xs `snocTree` a `snocTree` b `snocTree` x
appendTree2 (Deep s1 pr1 m1 sf1) a b (Deep s2 pr2 m2 sf2) =    Deep (s1 + size a + size b + s2) pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits2 m1 (One a) b c (One d) m2 =    appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

appendTree3 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree3 Empty a b c xs =    a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c Empty =    xs `snocTree` a `snocTree` b `snocTree` c
appendTree3 (Single x) a b c xs =    x `consTree` a `consTree` b `consTree` c `consTree` xs
appendTree3 xs a b c (Single x) =    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` x
appendTree3 (Deep s1 pr1 m1 sf1) a b c (Deep s2 pr2 m2 sf2) =    Deep (s1 + size a + size b + size c + s2) pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits3 m1 (One a) b c d (One e) m2 =    appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: FingerTree (Node a) -> Node a -> Node a -> Node a -> Node a -> FingerTree (Node a) -> FingerTree (Node a)
appendTree4 Empty a b c d xs =    a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d Empty =    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d
appendTree4 (Single x) a b c d xs =    x `consTree` a `consTree` b `consTree` c `consTree` d `consTree` xs
appendTree4 xs a b c d (Single x) =    xs `snocTree` a `snocTree` b `snocTree` c `snocTree` d `snocTree` x
appendTree4 (Deep s1 pr1 m1 sf1) a b c d (Deep s2 pr2 m2 sf2) =    Deep (s1 + size a + size b + size c + size d + s2) pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: FingerTree (Node (Node a)) -> Digit (Node a) -> Node a -> Node a -> Node a -> Node a -> Digit (Node a) -> FingerTree (Node (Node a)) -> FingerTree (Node (Node a))
addDigits4 m1 (One a) b c d e (One f) m2 =    appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 =    appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 =    appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =    appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2

null            :: Seq a -> Bool
null (Seq Empty) = True
null _          =  False

length          :: Seq a -> Int
length (Seq xs) =  size xs

data Maybe2 a b = Nothing2 | Just2 a b

-- | View of the left end of a sequence.
data ViewL a
    = EmptyL        -- ^ empty sequence
    | a :< Seq a    -- ^ leftmost element and the rest of the sequence
    deriving (Eq, Ord, Show) 

instance Functor ViewL where
    fmap _ EmptyL       = EmptyL
    fmap f (x :< xs)    = f x :< fmap f xs

instance Foldable ViewL where
    foldr _ z EmptyL = z
    foldr f z (x :< xs) = f x (foldr f z xs)

    foldl _ z EmptyL = z
    foldl f z (x :< xs) = foldl f (f z x) xs

    foldl1 _ EmptyL = error "foldl1: empty view"
    foldl1 f (x :< xs) = foldl f x xs

instance Traversable ViewL where
    traverse _ EmptyL       = pure EmptyL
    traverse f (x :< xs)    = (:<) <$> f x <*> traverse f xs

-- | /O(1)/. Analyse the left end of a sequence.
viewl           ::  Seq a -> ViewL a
viewl (Seq xs)  =  case viewLTree xs of
    Nothing2 -> EmptyL
    Just2 (Elem x) xs' -> x :< Seq xs'

viewLTree       :: Sized a => FingerTree a -> Maybe2 a (FingerTree a)
viewLTree Empty                        = Nothing2
viewLTree (Single a)                   = Just2 a Empty
viewLTree (Deep s (One a) m sf)        = Just2 a (pullL (s - size a) m sf)
viewLTree (Deep s (Two a b) m sf)      = Just2 a (Deep (s - size a) (One b) m sf)
viewLTree (Deep s (Three a b c) m sf)  = Just2 a (Deep (s - size a) (Two b c) m sf)
viewLTree (Deep s (Four a b c d) m sf) = Just2 a (Deep (s - size a) (Three b c d) m sf)

-- | View of the right end of a sequence.
data ViewR a
    = EmptyR        -- ^ empty sequence
    | Seq a :> a    -- ^ the sequence minus the rightmost element,
            -- and the rightmost element
    deriving (Eq, Ord, Show) --, Read)

instance Functor ViewR where
    fmap _ EmptyR       = EmptyR
    fmap f (xs :> x)    = fmap f xs :> f x

instance Foldable ViewR where
    foldr _ z EmptyR = z
    foldr f z (xs :> x) = foldr f (f x z) xs

    foldl _ z EmptyR = z
    foldl f z (xs :> x) = foldl f z xs `f` x

    foldr1 _ EmptyR = error "foldr1: empty view"
    foldr1 f (xs :> x) = foldr f x xs

instance Traversable ViewR where
    traverse _ EmptyR       = pure EmptyR
    traverse f (xs :> x)    = (:>) <$> traverse f xs <*> f x

viewr           ::  Seq a -> ViewR a
viewr (Seq xs)  =  case viewRTree xs of
    Nothing2 -> EmptyR
    Just2 xs' (Elem x) -> Seq xs' :> x

viewRTree       :: Sized a => FingerTree a -> Maybe2 (FingerTree a) a
viewRTree Empty                        = Nothing2
viewRTree (Single z)                   = Just2 Empty z
viewRTree (Deep s pr m (One z))        = Just2 (pullR (s - size z) pr m) z
viewRTree (Deep s pr m (Two y z))      = Just2 (Deep (s - size z) pr m (One y)) z
viewRTree (Deep s pr m (Three x y z))  = Just2 (Deep (s - size z) pr m (Two x y)) z
viewRTree (Deep s pr m (Four w x y z)) = Just2 (Deep (s - size z) pr m (Three w x y)) z

splitAt                 :: Int -> Seq a -> (Seq a, Seq a)
splitAt i (Seq xs)      =  (Seq l, Seq r)
  where (l, r)          =  split i xs

split :: Int -> FingerTree (Elem a) ->
    (FingerTree (Elem a), FingerTree (Elem a))
split i Empty   = i `seq` (Empty, Empty)
split i xs
  | size xs > i = (l, consTree x r)
  | otherwise   = (xs, Empty)
  where Split l x r = splitTree i xs

data Split t a = Split t a t deriving Show

splitTree :: Sized a => Int -> FingerTree a -> Split (FingerTree a) a
splitTree _ Empty = error "splitTree of empty tree"
splitTree i (Single x) = i `seq` Split Empty x Empty
splitTree i (Deep _ pr m sf)
  | i < spr     = case splitDigit i pr of
            Split l x r -> Split (maybe Empty digitToTree l) x (deepL r m sf)
  | i < spm     = case splitTree im m of
            Split ml xs mr -> case splitNode (im - size ml) xs of
                Split l x r -> Split (deepR pr ml l) x (deepL r mr sf)
  | otherwise   = case splitDigit (i - spm) sf of
            Split l x r -> Split (deepR pr m l) x (maybe Empty digitToTree r)
  where
    spr     = size pr
    spm     = spr + size m
    im      = i - spr

splitNode :: Sized a => Int -> Node a -> Split (Maybe (Digit a)) a
splitNode i (Node2 _ a b)
  | i < sa      = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    sa      = size a
splitNode i (Node3 _ a b c)
  | i < sa      = Split Nothing a (Just (Two b c))
  | i < sab     = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    sa      = size a
    sab     = sa + size b

splitDigit :: Sized a => Int -> Digit a -> Split (Maybe (Digit a)) a
splitDigit i (One a) = i `seq` Split Nothing a Nothing
splitDigit i (Two a b)
  | i < sa      = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where
    sa      = size a
splitDigit i (Three a b c)
  | i < sa      = Split Nothing a (Just (Two b c))
  | i < sab     = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where
    sa      = size a
    sab     = sa + size b
splitDigit i (Four a b c d)
  | i < sa      = Split Nothing a (Just (Three b c d))
  | i < sab     = Split (Just (One a)) b (Just (Two c d))
  | i < sabc    = Split (Just (Two a b)) c (Just (One d))
  | otherwise   = Split (Just (Three a b c)) d Nothing
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c

tails                   :: Seq a -> Seq (Seq a)
tails (Seq xs)          = Seq (tailsTree (Elem . Seq) xs) |> empty

inits                   :: Seq a -> Seq (Seq a)
inits (Seq xs)          = empty <| Seq (initsTree (Elem . Seq) xs)

tailsDigit :: Digit a -> Digit (Digit a)
tailsDigit (One a) = One (One a)
tailsDigit (Two a b) = Two (Two a b) (One b)
tailsDigit (Three a b c) = Three (Three a b c) (Two b c) (One c)
tailsDigit (Four a b c d) = Four (Four a b c d) (Three b c d) (Two c d) (One d)

initsDigit :: Digit a -> Digit (Digit a)
initsDigit (One a) = One (One a)
initsDigit (Two a b) = Two (One a) (Two a b)
initsDigit (Three a b c) = Three (One a) (Two a b) (Three a b c)
initsDigit (Four a b c d) = Four (One a) (Two a b) (Three a b c) (Four a b c d)

tailsNode :: Node a -> Node (Digit a)
tailsNode (Node2 s a b) = Node2 s (Two a b) (One b)
tailsNode (Node3 s a b c) = Node3 s (Three a b c) (Two b c) (One c)

initsNode :: Node a -> Node (Digit a)
initsNode (Node2 s a b) = Node2 s (One a) (Two a b)
initsNode (Node3 s a b c) = Node3 s (One a) (Two a b) (Three a b c)

tailsTree :: (Sized a, Sized b) => (FingerTree a -> b) -> FingerTree a -> FingerTree b
tailsTree _ Empty = Empty
tailsTree f (Single x) = Single (f (Single x))
tailsTree f (Deep n pr m sf) =
    Deep n (fmap (\ pr' -> f (deep pr' m sf)) (tailsDigit pr))
        (tailsTree f' m)
        (fmap (f . digitToTree) (tailsDigit sf))
  where
    f' ms = let Just2 node m' = viewLTree ms in
        fmap (\ pr' -> f (deep pr' m' sf)) (tailsNode node)


initsTree :: (Sized a, Sized b) => (FingerTree a -> b) -> FingerTree a -> FingerTree b
initsTree _ Empty = Empty
initsTree f (Single x) = Single (f (Single x))
initsTree f (Deep n pr m sf) =
    Deep n (fmap (f . digitToTree) (initsDigit pr))
        (initsTree f' m)
        (fmap (f . deep pr m) (initsDigit sf))
  where
    f' ms =  let Just2 m' node = viewRTree ms in
             fmap (\ sf' -> f (deep pr m' sf')) (initsNode node)

foldlWithIndex :: (b -> Int -> a -> b) -> b -> Seq a -> b
foldlWithIndex f z xs = foldl (\ g x i -> i `seq` f (g (i - 1)) i x) (const z) xs (length xs - 1)

foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldrWithIndex f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0

listToMaybe' :: [a] -> Maybe a
listToMaybe' = foldr (\ x _ -> Just x) Nothing


-- | /O(n)/.  The 'filter' function takes a predicate @p@ and a sequence
-- @xs@ and returns a sequence of those elements which satisfy the
-- predicate.
filter :: (a -> Bool) -> Seq a -> Seq a
filter p = foldl (\ xs x -> if p x then xs |> x else xs) empty

elemIndexL :: Eq a => a -> Seq a -> Maybe Int
elemIndexL x = findIndexL (x ==)

elemIndicesL :: Eq a => a -> Seq a -> [Int]
elemIndicesL x = findIndicesL (x ==)

findIndexL :: (a -> Bool) -> Seq a -> Maybe Int
findIndexL p = listToMaybe' . findIndicesL p

findIndicesL p xs = foldrWithIndex g [] xs
  where g i x is = if p x then i:is else is

fromList        :: [a] -> Seq a
fromList        =  Data.List.foldl' (|>) empty

-- Indexing

-- | /O(log(min(i,n-i)))/. The element at the specified position,
-- counting from 0.  The argument should thus be a non-negative
-- integer less than the size of the sequence.
-- If the position is out of range, 'index' fails with an error.
index           :: Seq a -> Int -> a
index (Seq xs) i
  | 0 <= i && i < size xs = case lookupTree i xs of
                Place _ (Elem x) -> x
  | otherwise   = error "index out of bounds"

data Place a = Place {-# UNPACK #-} !Int a
#if TESTING
    deriving Show
#endif

{-# SPECIALIZE lookupTree :: Int -> FingerTree (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupTree :: Int -> FingerTree (Node a) -> Place (Node a) #-}
lookupTree :: Sized a => Int -> FingerTree a -> Place a
lookupTree _ Empty = error "lookupTree of empty tree"
lookupTree i (Single x) = Place i x
lookupTree i (Deep _ pr m sf)
  | i < spr     =  lookupDigit i pr
  | i < spm     =  case lookupTree (i - spr) m of
                   Place i' xs -> lookupNode i' xs
  | otherwise   =  lookupDigit (i - spm) sf
  where
    spr     = size pr
    spm     = spr + size m

{-# SPECIALIZE lookupNode :: Int -> Node (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupNode :: Int -> Node (Node a) -> Place (Node a) #-}
lookupNode :: Sized a => Int -> Node a -> Place a
lookupNode i (Node2 _ a b)
  | i < sa      = Place i a
  | otherwise   = Place (i - sa) b
  where
    sa      = size a
lookupNode i (Node3 _ a b c)
  | i < sa      = Place i a
  | i < sab     = Place (i - sa) b
  | otherwise   = Place (i - sab) c
  where
    sa      = size a
    sab     = sa + size b

{-# SPECIALIZE lookupDigit :: Int -> Digit (Elem a) -> Place (Elem a) #-}
{-# SPECIALIZE lookupDigit :: Int -> Digit (Node a) -> Place (Node a) #-}
lookupDigit :: Sized a => Int -> Digit a -> Place a
lookupDigit i (One a) = Place i a
lookupDigit i (Two a b)
  | i < sa      = Place i a
  | otherwise   = Place (i - sa) b
  where
    sa      = size a
lookupDigit i (Three a b c)
  | i < sa      = Place i a
  | i < sab     = Place (i - sa) b
  | otherwise   = Place (i - sab) c
  where
    sa      = size a
    sab     = sa + size b
lookupDigit i (Four a b c d)
  | i < sa      = Place i a
  | i < sab     = Place (i - sa) b
  | i < sabc    = Place (i - sab) c
  | otherwise   = Place (i - sabc) d
  where
    sa      = size a
    sab     = sa + size b
    sabc    = sab + size c
