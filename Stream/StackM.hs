{-# Language
KindSignatures
,RankNTypes
,ConstraintKinds
,DefaultSignatures
,TypeFamilies
,ScopedTypeVariables
,TypeApplications
#-}

module Stream.StackM where

import Control.Applicative (liftA2)
import Stream.StateM
import Data.Bifunctor

----
-- Pair

traversePair :: Applicative f => (a -> f x) -> (b -> f y) -> (a,b) -> f (x,y)
traversePair f1 f2 (a1,a2) = liftA2 (,) (f1 a1) (f2 a2) 

----
-- StackM

class GetM (f :: (* -> *) -> * -> *) where 
 getM :: Monad m => StateM m (f m a) a

class SetM (f :: (* -> *) -> * -> *) where 
 setM  :: Monad m => m (Maybe (a, f m a)) -> f m a -- delete?
 setM' :: Monad m =>    Maybe (a, f m a)  -> f m a

consM :: (SetM f, Monad m) => a -> f m a -> f m a
consM x = setM' . ((Just . ((,) x))) 

emptyM :: (SetM f,Monad m) => f m a
emptyM = setM (pure Nothing)

class (GetM f,SetM f) => StackM f

----
-- convertM

convertM :: forall a b t' t m f. (SetM t', GetM t, Monad m, Applicative f) => (a -> f b) -> t m a -> m (f (t' m b))
convertM f s = getM s >>= maybe (return (pure emptyM)) g
 where
  g (x,xs) = (\y->((<*>)( fmap (curry (setM' . Just)) y)) <$> convertM f xs) (f x)

-- traverse' f = foldr cons_f (pure []) where cons_f x ys = liftA2 (:) (f x) ys

----
-- zip

zipWithM :: forall f1 f2 t a b c m. (GetM f1,GetM f2,SetM t, Monad m) => (a -> b -> c) -> f1 m a -> f2 m b -> t m c
zipWithM f xs ys = setM $ liftA2 (liftA2 g) (getM xs) (getM ys) 
 where
  g :: (a, f1 m a) -> (b, f2 m b) -> (c, t m c)
  g (a,xs') (b,ys') = (f a b,zipWithM f xs' ys')

zipM :: (GetM f1,GetM f2,SetM t, Monad m) => f1 m a -> f2 m b -> t m (a,b)
zipM = zipWithM (,)

----
-- unzip

unzipWithM :: forall f t1 t2 a b c m. (GetM f,SetM t1,SetM t2,Monad m) => (a -> (b,c)) -> f m a -> (t1 m b,t2 m c)
unzipWithM f xs = (setM $ (fmap.fmap) (g fst fst) m,setM $ (fmap.fmap) (g snd snd) m)
 where
  m :: m (Maybe (a, (t1 m b, t2 m c)))
  m = (fmap.fmap.fmap) (unzipWithM f) $ getM xs
  g :: SetM f0 => ((b, c) -> a1) -> (t0 -> f0 m a0) -> (a, t0) -> (a1, f0 m a0)
  g f1 f2 (x,xs') = ((f1 . f) x,f2 $ xs')

unzipM :: forall f t1 t2 a b c m. (GetM f,SetM t1,SetM t2,Monad m) => f m (a,b) -> (t1 m a,t2 m b)
unzipM = unzipWithM id

forkM :: forall f t1 t2 a b c m. (GetM f,SetM t1,SetM t2,Monad m) => f m a -> (t1 m a,t2 m a)
forkM = unzipWithM (\a->(a,a))

{-
unzipWith :: forall f t1 t2 a b c m. (GetM f,SetM t1,SetM t2,Monad m) => (a -> (b,c)) -> f m a -> (t1 m b,t2 m c)
unzipWith f xs = (h.g) <$> getM xs
 where
  g f = fmap (\(x,xs') -> bimap f (f x, unzipWith f xs'))
-}
{-
 let b = fmap (\(x,xs') -> (fst x,fst xs'))
 let c = fmap (\(x,xs') -> (snd x,snd xs'))
 (setM b,setM c) 
-}
----
-- FunctorM default

fmapMDefault :: (StackM t, Monad m) => (a -> m b) -> t m a -> t m b
fmapMDefault f s = setM (getM s >>= maybe (return Nothing) g)
  where
   g (x,xs) = do
    x' <- f x
    return (Just (x' ,fmapMDefault f xs))

----
-- FoldableM default

foldrMDefault :: (GetM t,Monad m) => (a -> m (b -> m b)) -> b -> t m a -> m b 
foldrMDefault f b xs = getM xs >>= maybe (return b) (\(x,xs)->f x >>= \g -> foldrMDefault f b xs >>= g) 

foldlMDefault :: (GetM t,Monad m) => (b -> a -> m b) -> m b -> t m a -> m b 
foldlMDefault f b xs = getM xs >>= maybe b (\(x,xs)-> foldlMDefault f (b >>= \y -> (f y x)) xs) 

----
-- ScannerM default

scannerMDefault
  :: forall s (t :: (* -> *) -> * -> *) (m :: * -> *) a b. (Monad m, StackM t) =>
     (s -> a -> m ( b, s)) -> s -> t m a -> (t m b,m s)
scannerMDefault f s xs = (setM  (maybe (return Nothing) g  =<< getM xs),return s)
 where
  g :: (a, t m a) -> m (Maybe (b,t m b))
  g (x,xs) = (Just . fmap (\s' -> fst $ scannerMDefault f s' xs)) <$> f s x 

----
-- TraverseM default

traverseMDefault :: forall a b t m f. (StackM t, Monad m, Applicative f) => (a -> f b) -> t m a -> m (f (t m b))
traverseMDefault = convertM

