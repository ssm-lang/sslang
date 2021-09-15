import System.Process (system)
import System.Exit (exitWith)

main :: IO ()
main = (system "cd regression-tests && ./runtests.sh") >>= exitWith


