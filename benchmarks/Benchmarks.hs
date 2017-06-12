import System.Directory (getDirectoryContents, setCurrentDirectory)
import Criterion.Main
import qualified Data.Text.IO as TIO
import Hasmin

main :: IO ()
main = do
    setCurrentDirectory "../hasmin-benchmarks/bnch"
    xs <- getDirectoryContents "."
    ys <- traverse TIO.readFile (filter (flip notElem [".", ".."]) xs)
    defaultMain [bgroup "minification" [bench "a" $ nf (map minifyCSS) ys]]
