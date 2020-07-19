module Stream.StateM where

----
-- StateM

type StateM   m s a = s -> m (Maybe (a, s))
type CoStateM m s a = m (Maybe (a, s)) -> s

hyloStateM :: Functor m => StateM m s a -> (CoStateM m b a) -> s -> b
hyloStateM f c = go
 where
  go = c . fmap (fmap (fmap go)) . f

{-
stackMDifference :: forall t t' m a b f. (Monad m,GetM t, SetM t', Applicative f) => (a -> f b) -> StateM m (t m a) (f (t' m b -> t' m b))
stackMDifference f xs = fmap (\(x,xs') -> (fmap (curry (setM.pure.Just)) (f x),xs')) <$> getM xs

convertM' :: forall f m t t' a b. (f~m,Monad m,GetM t, SetM t', Applicative f) => (a -> f b) -> t m a -> f (t' m b)
convertM' f = hyloStateM (stackMDifference f) g
 where
  g :: (SetM t',Applicative f,Applicative m) => CoStateM m (f (t' m b)) (f (t' m b -> t' m b))
  g m = m >>= maybe (return emptyM) (uncurry (liftA2 ($)))
-}