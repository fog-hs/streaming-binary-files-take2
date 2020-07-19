module Stream.Clock (Clock,clock,timer,reStream) where
import Stream.StackM (zipWithM)
import Stream.ListT
import Stream.LinearM
import Control.Concurrent
import Control.Applicative (liftA2)

scannerIO
  :: (s -> IO (b, s)) -> s -> IOList a -> (IOList b, IO s)
scannerIO f s = scannerM (\ s' _ -> f s') s 

----
-- Clock

type Clock m = ListT m ()

clock :: Monad m => Clock m
clock = liftList $ repeat ()

test1 :: IO ()
test1 = display clock


----
-- timer

timer :: Int -> Clock IO
timer n = fmapM (\ _ -> threadDelay n >> return ()) clock 

test2 :: IO ()
test2 = display $ timer 100000 

----
-- untimed

untimed :: (s -> IO (b, s)) -> s -> (IOList b, IO s)
untimed f s = scannerIO f s clock

test3 :: IO ()
test3 = display . fst $ untimed (\s -> return (s*2,s+1)) 0

----
-- timed

timed :: Int -> (s -> IO (b, s)) -> s -> (IOList b, IO s)
timed n f s = scannerIO f s $ timer n

test4 :: IO ()
test4 = display . fst $ timed 100000 (\s -> return (s*2,s+1)) 0

----
-- reStream

reStream :: Monad m => Clock m -> ListT m a -> ListT m a
reStream = zipWithM (flip const)

test5 :: IO ()
test5 = discard $ timedShoutingClock 
 where
  timedShoutingClock :: (ListT IO ())
  timedShoutingClock = reStream (timer 100000) shoutingClock
  shoutingClock = fmapM (\ _ -> print "shout!" >> return ()) clock
