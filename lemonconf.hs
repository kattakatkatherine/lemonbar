-- lemonbar config
-- compile with $ stack ghc lemonconf.hs
-- run with $ ./lemonconf | lemonbar

{-# OPTIONS_GHC -O2            #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (init, putStrLn, readFile, take)
import Data.Time
import Data.Text
import Data.Text.IO (putStrLn, readFile)
import System.Process
import Control.Concurrent

-- set up MVars and threads
main = do 
    work  <- newEmptyMVar
    clock <- newEmptyMVar
    bat   <- newEmptyMVar
    forkIO $ pushWork work
    forkIO $ pushClock clock
    forkIO $ pushBat bat
    printLemon work clock bat

-- print and repeat
printLemon :: MVar Text -> MVar Text -> MVar Text -> IO ()
printLemon work clock bat = do
    w <- readMVar work
    c <- readMVar clock
    b <- readMVar bat
    -- lemonbar only processes every ~100th output for technical reasons
    mapM_ (\x -> putStrLn $ w <> c <> b) [0..127]
    threadDelay 100000
    printLemon work clock bat

-- docs to get info from
time = getZonedTime >>= return . formatTime defaultTimeLocale "%b %e, %k:%M"
charging = readFile "/sys/class/power_supply/BAT0/status"
battery = readFile "/sys/class/power_supply/BAT0/capacity"
focused = readProcess "bspc" ["query","-D","-d","focused","--names"] ""
workspaces = readProcess "bspc" ["query","-D","--names"] ""

-- workspace monitors
formatWork f w = "%{l}" <> (replace "\n" "  " (replace ("\n" <> g)
    (" %{B#00b8c8} " <> init g <> " %{B-} ") ("\n" <> v)))
    where g = pack f
          v = pack w
pushWork :: MVar Text -> IO ()
pushWork work = do 
    f <- focused
    w <- workspaces
    x <- tryTakeMVar work
    putMVar work $! formatWork f w
    threadDelay 90000 -- update often
    pushWork work

-- time and date
formatClock t = " %{c}" <> if index c 0 == ' ' then take 3 c <> takeEnd 7 c
    else c
    where c = pack t
pushClock :: MVar Text -> IO ()
pushClock clock = do 
    t <- time
    x <- tryTakeMVar clock
    putMVar clock $! formatClock t
    threadDelay 30000000 -- update every 30 sec
    pushClock clock

-- battery and charge status
formatBat b ch = " %{r}" <> init b <> (if ch == "Charging\n" then "%+" else "%")
pushBat :: MVar Text -> IO ()
pushBat bat = do 
    b  <- battery
    ch <- charging
    x  <- tryTakeMVar bat
    putMVar bat $! formatBat b ch
    threadDelay 10000000 -- update every 10 sec
    pushBat bat
