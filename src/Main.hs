module Main where
import Reader
import Parser
import Assembler
import Util
import Hexdump
import System.Environment
import qualified Data.ByteString as BS

main :: IO ()
main = do
  -- Determine the file to compile.
  args <- getArgs
  let file = head args
  let output = head . tail $ args
  -- Parse the file into an AST
  source <- Reader.readProgram file
  case Parser.parse source of
    (Left err)  -> putStrLn err
    (Right ast) -> do
                     mapM_ (putStrLn . show) ast
                     let (bs, mem) = assemble ast
                     putStrLn (prettyHex bs)
                     putStrLn (show mem)
                     -- Write the binary to a file.
                     BS.writeFile  output bs
