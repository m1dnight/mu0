module Main where
import Reader
import Parser
import Assembler
import Util
import Hexdump

main :: IO ()
main = do
  source <- Reader.readProgram "asm/fac.asm"
  case Parser.parse source of
    (Left err)  -> putStrLn err
    (Right ast) -> do
                     mapM_ (putStrLn . show) ast
                     let (bs, mem) = assemble ast
                     putStrLn (prettyHex bs)
                     putStrLn (show mem)
