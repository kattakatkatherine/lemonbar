-- lemonbar config
-- compile with $ stack ghc lemonbar.hs -- -threaded
-- run with ./lemonbar +RTS -N

import Data.Time
import Data.Text (pack, replace, unpack)
import System.Process
import Control.Concurrent

-- set up MVars and threads
main = do work <- newEmptyMVar
          clock <- newEmptyMVar
          bat <- newEmptyMVar
          forkIO $ pushWork work
          forkIO $ pushClock clock
          forkIO $ pushBat bat
          printLemon work clock bat

-- print and repeat
printLemon :: MVar String -> MVar String -> MVar String -> IO ()
printLemon work clock bat = do w <- readMVar work
                               c <- readMVar clock
                               b <- readMVar bat
                               putStrLn $ w ++ c ++ b
                               threadDelay 1500
                               printLemon work clock bat

-- docs to get info from
time = getZonedTime >>= return . formatTime defaultTimeLocale "%b%e, %k:%M"
charging = readFile "/sys/class/power_supply/BAT0/status"
battery = readFile "/sys/class/power_supply/BAT0/capacity"
focused = readProcess "bspc" ["query","-D","-d","focused","--names"] ""
workspaces = readProcess "bspc" ["query","-D","--names"] ""

-- workspace monitors
formatWork f w = "%{l}" ++ unpack (replace (pack "\n") (pack "  ") (replace (pack $ "\n" ++ f) (pack $ " %{B#00b8c8} " ++ init f ++ " %{B-} ") (pack $ "\n" ++ w)))
pushWork :: MVar String -> IO ()
pushWork work = do f <- focused
                   w <- workspaces
                   x <- tryTakeMVar work
                   putMVar work $ formatWork f w
                   threadDelay 1400 -- update often
                   pushWork work

-- time and date
formatClock t = " %{c}" ++ (if t!!4 == ' ' then (take 3 t ++ take 9 (drop 4 t)) else t)
pushClock :: MVar String -> IO ()
pushClock clock = do t <- time
                     x <- tryTakeMVar clock
                     putMVar clock $ formatClock t
                     threadDelay 30000000 -- update every 30 sec
                     pushClock clock

-- battery and charge status
formatBat b ch = " %{r}" ++ init b ++ (if ch == "Charging\n" then "%+" else "%")
pushBat :: MVar String -> IO ()
pushBat bat = do b <- battery
                 ch <- charging
                 x <- tryTakeMVar bat
                 putMVar bat $ formatBat b ch
                 threadDelay 10000000 -- update every 10 sec
                 pushBat bat
