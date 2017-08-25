{
module Parser where
import Lexer
import Text.Printf
import AST

-- | Number | Human Readable  | Action                                                         |
-- |--------|-----------------|----------------------------------------------------------------|
-- | 0      | ACC <= [S]      | Copy address S from memory to ACC                              |
-- | 1      | ACC => [S]      | Copy the value of the ACC to the memory address S.             |
-- | 2      | ACC + [S]       | Add the value at memory address S to the ACC value.            |
-- | 3      | ACC - [S]       | Subtract the value at address S from ACC.                      |
-- | 4      | PC <= S         | Set the program counter to line address S (not the value!).    |
-- | 5      | IF +VE PC <= S  | Set the program counter to line address S if the ACC > 0.      |
-- | 6      | IF != 0 PC <= S | Set the program counter to line address S if the ACC is not 0. |
-- | 7      | STOP            | Computer stops.                                                |
------------------------------------------------------------------------------------------------

-- Register    ::= ACC | PC
-- StoreLine   ::= Integer | Label
-- Label       ::= String
--
-- Instruction ::= Register <= [StoreLine]
--               | Register => [StoreLine]
--               | Register + [Storeline]
--               | Register - [StoreLine]
--               | PC <= StoreLine
--               | IF +VE PC <= StoreLine
--               | IF != 0 PC <= StoreLine
--               | STOP
--
-- ProgramLine ::= Label : Instruction | Instruction

}

%name parseIniFile
%error     {parseError}
%lexer     {alexWrapper} {Eof}
%monad     {Alex}
%tokentype {Token}
%token
    ACC         {ACC          _ }
    PC          {PC           _ }
    '<='        {LArrow       _ }
    '=>'        {RArrow       _ }
    '+'         {Plus         _ }
    '-'         {Min          _ }
    '+VE'       {VE           _ }
    '!= 0'      {Ne           _ }
    '['         {LBrack       _ }
    ']'         {RBrack       _ }
    ':'         {Colon        _ }
    ','         {Comma        _ }
    'STOP'      {Stop         _ }
    'IF'        {If           _ }
    Number      {IntegerNum v _ }
    Identifier  {Identifier v _ }



%%

Program
 : ProgramLines                    { reverse $1                 }


ProgramLines
 : {- empty -}                     { []                         }
 | ProgramLines ProgramLine        { $2 : $1                    }

ProgramLine
 : Identifier ':' Instruction      { LabeledInstr (name $1) $3  }
 | Identifier ':' Number           { Const (name $1) (value $3) }
 | Instruction                     { Instr $1                   }


Instruction
 : ACC '<=' '[' StoreLine ']'        { M2A $4                     }
 | ACC '=>' '[' StoreLine ']'        { A2M $4                     }
 | ACC '+' '[' StoreLine ']'         { APM $4                     }
 | ACC '-' '[' StoreLine ']'         { AMM $4                     }
 | PC  '<=' StoreLine                { PCSet $3                   }
 | 'IF' '+VE' PC '<=' StoreLine      { IfVe $5                    }
 | 'IF' '!= 0' ',' PC '<=' StoreLine { IfNe $6                    }
 | 'STOP'                            { STOP                       }


Register
 : ACC { Acc }
 | PC  { Pc  }

StoreLine
 : Number     { Address (value $1) }
 | Identifier { Label (name $1)   }


{



parseError :: Token -> Alex a
parseError t = case t of
  Eof -> alexError "Encountered EOF while parsing"
  _   -> alexError $ let (AlexPn _ row col) = position t
                     in
                       printf "Parser error at line: %s, col: %s" (show row) (show col)

alexWrapper :: (Token -> Alex a) -> Alex a
alexWrapper cont = alexMonadScan >>= cont

parse :: String -> Either String [ProgramLine]
parse s = runAlex s parseIniFile


}
