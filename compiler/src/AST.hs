module AST where

data ProgramLine
 = LabeledInstr String Instruction
 | Const String Int
 | Instr Instruction
 deriving (Show, Eq)


data Register
 = Acc
 | Pc
 deriving (Show, Eq)


data StoreLine
 = Address Int
 | Label String
 | ProgramCounter
 deriving (Show, Eq)


data Instruction
 = M2A StoreLine
 | A2M StoreLine
 | APM StoreLine
 | AMM StoreLine
 | PCSet StoreLine
 | IfVe StoreLine
 | IfNe StoreLine
 | STOP
 deriving (Show, Eq)
