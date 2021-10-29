import System.Process (system)
import System.Exit (exitWith)

-- | For now, we use the old regression runtests.sh script.
main :: IO ()
main = system "cd regression-tests && ./runtests.sh" >>= exitWith
