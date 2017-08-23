module Main where
import Reader
import Parser
import Assembler

main :: IO ()
main = do
  source <- Reader.readProgram "asm/add.asm"
  let (Right ast) = Parser.parse source
  mapM_ print ast
