{
module Lexer where

import Text.Printf
import Debug.Trace
}

%wrapper "monadUserState"

$spaces = [\ \t]
$alpha  = [a-zA-Z]
$digits = [0-9]
$alnum  = [$alpha$digits]

@identifier = $alnum+
@comment    = \#.*
@integer    = $digits+

:-

ACC             { mkL LACC        }
PC              { mkL LPC         }
"<="            { mkL LLArrow     }
"=>"            { mkL LRArrow     }
"="             { mkL LEqual      }
"+"             { mkL LPlus       }
"-"             { mkL LMin        }
"+VE"           { mkL LVE         }
"!= 0"          { mkL LNe         }
"["             { mkL LLBrack     }
"]"             { mkL LRBrack     }
":"             { mkL LColon      }
","             { mkL LComma      }
"STOP"          { mkL LStop       }
"IF"            { mkL LIf         }
@integer        { mkL LInteger    }
@identifier     { mkL LIdentifier }
@comment        ;
[\ \t \n]+      ;

{

-- The actual type of the state of the monad.
type AlexUserState = [Token]

-- Initial state of the monad.
alexInitUserState :: AlexUserState
alexInitUserState = []

data LexemeClass
  = LACC
  | LPC
  | LLArrow
  | LRArrow
  | LEqual
  | LPlus
  | LMin
  | LVE
  | LNe
  | LLBrack
  | LRBrack
  | LColon
  | LComma
  | LStop
  | LIf
  | LInteger
  | LIdentifier
  | LEOF
    deriving (Eq, Show)


mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = let t = take len str
                           in case c of
                                LACC        -> return $ ACC p
                                LPC         -> return $ PC p
                                LEqual      -> return $ Equal p
                                LLArrow     -> return $ LArrow p
                                LRArrow     -> return $ RArrow p
                                LPlus       -> return $ Plus p
                                LMin        -> return $ Min p
                                LVE         -> return $ VE p
                                LNe         -> return $ Ne p
                                LLBrack     -> return $ LBrack p
                                LRBrack     -> return $ RBrack p
                                LColon      -> return $ Colon p
                                LComma      -> return $ Comma p
                                LStop       -> return $ Stop p
                                LIf         -> return $ If p
                                LInteger    -> return (IntegerNum p ((read t) :: Int))
                                LIdentifier -> return (Identifier p t)


alexEOF :: Alex Token
alexEOF = return Eof


data Token
  = ACC           {position :: AlexPosn                   }
  | PC            {position :: AlexPosn                   }
  | Equal         {position :: AlexPosn                   }
  | LArrow        {position :: AlexPosn                   }
  | RArrow        {position :: AlexPosn                   }
  | Plus          {position :: AlexPosn                   }
  | Min           {position :: AlexPosn                   }
  | VE            {position :: AlexPosn                   }
  | Ne            {position :: AlexPosn                   }
  | RBrack        {position :: AlexPosn                   }
  | LBrack        {position :: AlexPosn                   }
  | Colon         {position :: AlexPosn                   }
  | Comma         {position :: AlexPosn                   }
  | Stop          {position :: AlexPosn                   }
  | If            {position :: AlexPosn                   }
  | IntegerNum    {position :: AlexPosn, value :: Int     }
  | Identifier    {position :: AlexPosn, name :: String   }
  | Eof
  deriving (Eq, Show)


scanner str = runAlex str $ do
    let loop i = do token <- alexMonadScan
                    if token == Eof
                      then return i
                      else do
                             let i' = i++[token] in i' `seq` loop i'
    loop []



}
