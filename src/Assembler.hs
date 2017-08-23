module Assembler where
import AST
import Data.ByteString as BS
import Data.Word
import Data.Map.Strict
import Control.Monad.Trans.State.Lazy
import Data.Map as M
import Hexdump
import Parser

----------------------------------
-- The state during compilation --
----------------------------------


data CompileState =
  M {
      backpatches :: M.Map String [Integer],
      linecount   :: Integer,
      labels      :: M.Map String Integer
    }

emptyState = M M.empty 0 M.empty

type Memory = CompileState

type Compiler a = State Memory a

-- Remembers a constant
set_label :: String -> Integer -> Compiler ()
set_label label value =
  do
    state <- get
    let labels' = M.insert label value $ labels state
    let state'   = state { labels = labels' }
    put state'


-- Remembers a label and a current address in the
set_backpatch :: String -> Integer -> Compiler ()
set_backpatch label position =
  do
    state <- get
    let patches' = M.insertWith (++) label [position] $ backpatches state
    let state'   = state { backpatches = patches' }
    put state'

-- Manage the current line count.
line_count :: Compiler Integer
line_count = fmap linecount get

line_count_inc :: Integer -> Compiler ()
line_count_inc delta =
  do
    state <- get
    let lc = linecount state
    let state' = state {linecount = lc + delta}
    put state'

-------------
-- Program --
-------------


test :: String
test =
  let src         = "ACC <= [L2]"
      (Right ast) = Parser.parse src
  in
    debug ast

debug :: [ProgramLine] -> String
debug ls =
  let compiled = evalState (assemble ls) emptyState
  in
    prettyHex compiled

assemble :: [ProgramLine] -> Compiler ByteString
assemble [] = return BS.empty
assemble (l:ls) =
  do
    compiled <- assemble_line l
    compiled' <- assemble ls
    return (append compiled compiled')


-------------------
-- Program Lines --
-------------------


assemble_line :: ProgramLine -> Compiler ByteString
assemble_line (Instr instruction) =
  assemble_ins instruction

assemble_line (Const label value) =
  do
    set_label label value
    return $ BS.empty

assemble_line (LabeledInstr label instruction) =
  do
    -- Remember the current label position
    posn <- line_count
    set_label label posn
    -- Compile the rest of the instructions
    assemble_instruction instruction


------------------
-- Instructions --
------------------


assemble_instruction :: Instruction -> Compiler ByteString
-- STOP == 0x7
assemble_instruction STOP =
  return $ BS.singleton . fromIntegral $ 7

-- ACC <= [S]
-- M2A == 0x0 S
assemble_instruction (M2A storeline) =
  do
    let opr = BS.singleton . fromIntegral $ 0
    -- Increase the program counter once so that the position of the next address
    -- is correct.
    line_count_inc 1
    adr <- assemble_storeline storeline
    return $ append opr adr

-- catch all
assemble_ins _    = return $ BS.empty


----------------
-- StoreLines --
----------------


assemble_storeline :: StoreLine -> Compiler ByteString
-- An address is a literal so we just output that.
assemble_storeline (Address a) =
  return $ BS.singleton . fromIntegral $ a

-- In case of a label we output a dummy value, 0x0, and backpatch it later.
-- Additionally, we remember the current position to be able to backpatch.
assemble_storeline (Label label) =
  do
    linecount <- line_count
    set_backpatch label linecount
    return $ BS.singleton . fromIntegral $ 0
