module Assembler where
import AST
import Data.ByteString as BS
import Data.Word
import Data.Map.Strict
import Control.Monad.Trans.State.Lazy
import Data.Map as M
import Hexdump
import Parser
import Util

import Debug.Trace

----------------------------------
-- The state during compilation --
----------------------------------

data CompileState =
  M {
      backpatches :: M.Map Int String, -- Maps line numbers to labels
      linecount   :: Int,              -- Current number of bytes written
      labels      :: M.Map String Int  -- Maps labels to line numbers
    } deriving (Show)

emptyState = M M.empty 0 M.empty

type Memory = CompileState

type Compiler a = State Memory a

-------------
-- Helpers --
-------------


-- Remembers a constant
set_label :: String -> Int -> Compiler ()
set_label label value =
  do
    state <- get
    let labels' = M.insert label value $ labels state
    let state'   = state { labels = labels' }
    put state'

-- Remembers a label and a current address in the
set_backpatch :: String -> Int -> Compiler ()
set_backpatch label position =
  do
    state <- get
    let patches' = M.insert position label $ backpatches state
    let state'   = state { backpatches = patches' }
    put state'

-- Manage the current line count.
line_count :: Compiler Int
line_count = fmap linecount get

-- Incremenets the current linecount with delta.
line_count_inc :: Int -> Compiler ()
line_count_inc delta =
  do
    state <- get
    let lc = linecount state
    let state' = state {linecount = lc + delta}
    put state'

-- Backpatches the entire binary with the state accumulated during compilation.
backpatch :: ByteString -> Compiler ByteString
backpatch bs =
  do
    state <- get
    let src = BS.unpack bs
    let ls  = labels state
    let bps = backpatches state
    -- Iterate over all the labels and replace them if they have been referenced.
    let patched = M.foldWithKey (\line label bs ->
                                   let (Just lblline) = M.lookup label ls
                                   in
                                     replaceNth line (fromIntegral lblline) bs)
                                src
                                bps
    return . pack $ patched



assemble :: [ProgramLine] -> (ByteString, Memory)
assemble ls =
  let (compiled, mem) = runState (assemble_program ls) emptyState
  in
    (compiled, mem)

-------------
-- Program --
-------------

assemble_program :: [ProgramLine] -> Compiler ByteString
assemble_program ls =
  do
    bytes   <- assemble_program' ls
    patched <- backpatch bytes
    return patched

assemble_program' :: [ProgramLine] -> Compiler ByteString
assemble_program' [] = return BS.empty
assemble_program' (l:ls) =
  do
    -- Compile all the lines into bytestrings.
    compiled  <- assemble_line l
    compiled' <- assemble_program' ls
    -- Backpatch all the found labels.
    let compiled'' = append compiled compiled'
    return compiled''


-------------------
-- Program Lines --
-------------------


assemble_line :: ProgramLine -> Compiler ByteString
assemble_line (Instr instruction) =
  assemble_instruction instruction

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
-- ACC <= [S]
-- M2A == 0x0 S
assemble_instruction (M2A storeline) =
  do
    let opr = BS.singleton . fromIntegral $ 0
    -- Increase the program counter once so that the position of the next address
    -- is correct.
    line_count_inc 1
    adr <- assemble_storeline storeline
    line_count_inc 1
    return $ append opr adr

-- ACC => [S]
-- A2M == 0x1 S
assemble_instruction (A2M storeline) =
  do
    let opr = BS.singleton . fromIntegral $ 1
    -- Increase the program counter once so that the position of the next address
    -- is correct, in case it is a label that needs backpatching.
    line_count_inc 1
    adr <- assemble_storeline storeline
    line_count_inc 1
    return $ append opr adr

-- ACC + [S]
-- APM == 0x2 S
assemble_instruction (APM storeline) =
  do
    let opr = BS.singleton . fromIntegral $ 2
    -- Increase the program counter once so that the position of the next address
    -- is correct, in case it is a label that needs backpatching.
    line_count_inc 1
    adr <- assemble_storeline storeline
    line_count_inc 1
    return $ append opr adr

-- ACC - [S]
-- APM == 0x3 S
assemble_instruction (AMM storeline) =
  do
    let opr = BS.singleton . fromIntegral $ 3
    -- Increase the program counter once so that the position of the next address
    -- is correct, in case it is a label that needs backpatching.
    line_count_inc 1
    adr <- assemble_storeline storeline
    line_count_inc 1
    return $ append opr adr

-- PC <= S
-- PCSet == 0x4 S
assemble_instruction(PCSet storeline) =
  do
    let opr = BS.singleton . fromIntegral $ 4
    -- Increase the program counter once so that the position of the next address
    -- is correct, in case it is a label that needs backpatching.
    line_count_inc 1
    adr <- assemble_storeline storeline
    line_count_inc 1
    return $ append opr adr

-- IF +VE PC <= S
-- IfVe == 0x5
assemble_instruction(IfVe storeline) =
  do
    let opr = BS.singleton . fromIntegral $ 5
    -- Increase the program counter once so that the position of the next address
    -- is correct, in case it is a label that needs backpatching.
    line_count_inc 1
    adr <- assemble_storeline storeline
    line_count_inc 1
    return $ append opr adr

-- IF != 0 PC <= S
-- IfNe == 0x5
assemble_instruction(IfNe storeline) =
  do
    let opr = BS.singleton . fromIntegral $ 6
    -- Increase the program counter once so that the position of the next address
    -- is correct, in case it is a label that needs backpatching.
    line_count_inc 1
    adr <- assemble_storeline storeline
    line_count_inc 1
    return $ append opr adr

-- STOP == 0x7
assemble_instruction STOP =
  do
    line_count_inc 1
    return $ BS.singleton . fromIntegral $ 7


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
