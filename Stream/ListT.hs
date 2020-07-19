{-# Language 
TypeSynonymInstances
,RankNTypes
,TypeApplications,ScopedTypeVariables
,InstanceSigs
#-}

module Stream.ListT (ListT(..),IOList,liftList,unfoldrM,display,discard,ML(..),MList) where

import GHC.Base
import Control.Monad.Identity
import Stream.StackM
import Stream.StateM
import Stream.LinearM

----
-- ListT

data ML m a = MEmpty | a `MCons` MList m a
type MList m a  = m (ML m a)

newtype ListT m a = ListT { runListT :: MList m a }

type IOList = ListT IO

liftList :: Monad m => [a] -> ListT m a
liftList [] = ListT $ return MEmpty
liftList (x:xs) = ListT . return $ x `MCons` (runListT $ liftList xs)

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> m b -> ListT m a
unfoldrM f b = setM ((((unfoldrM f . return) <$>) <$>) <$> (b >>= f))

----
-- LinearM instance

instance GetM ListT where
 getM (ListT m) = fmap g m where
  g MEmpty = Nothing
  g (x `MCons` xs) = Just (x, ListT xs)

instance SetM ListT where
 setM  z = ListT $ maybe (return MEmpty) (\(x,ListT xs) -> return (x `MCons` xs)) =<< z
 setM' z = ListT $ maybe (return MEmpty) (\(x,ListT xs) -> return (x `MCons` xs))   $ z

instance StackM ListT
instance FunctorM ListT 
instance FoldableM ListT 
instance ScannerM ListT 
instance TraversableM ListT 
instance LinearM ListT 

----
-- testing

--consume

display :: Show a => IOList a -> IO ()
display = foldrM (\a -> print a >> (return (\ () -> return ()))) () 

discard :: (Show a,Monad m) => ListT m a -> m ()
discard = foldrM (\a -> (return (\ () -> return ()))) ()

--

egListT :: IOList Int
egListT = liftList [1..]

egListT1 :: IOList Int
egListT1 = unfoldrM (\b -> return (Just (b,b))) (return (0 :: Int))


egListT2 :: IOList Int
egListT2 = fmapM (\x -> return (x*2)) egListT 

egListT3 :: (IOList Int)
egListT3 = fst $ scannerM (\s a -> return (a*s,s+1)) (0::Int) egListT 

egListT4 :: IO (IOList Int)
egListT4 = runIdentity <$> traverseM return egListT 

test  =              display egListT
test1 =              display egListT1 
test2 =              display egListT2 
test3 =              display egListT3 
test4 = egListT4 >>= display



