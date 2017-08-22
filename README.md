# mu0

## Grammar

An example program (adding two numbers) of the MU0 emulator is shown below.

```
ACC <= [L2]  # Set Z to 0 in the beginning.
ACC => [112] # Write the initial value of Z.
# Read current Z, add X, and write back.   
L1: ACC <= [112]  
ACC + [110 ]
ACC => [112]
# Read in Y, decrement, and write back.
ACC <= [111]
ACC - [L3]
ACC => [111]
IF != 0, PC <= L1
STOP
# Constants
L2: 0
L3: 1
```

From this program we derived the following grammar.

### Canonicals

```
-- Register    = ACC | PC
-- Operator    = <= | => | + | -
-- Variable    = String
-- IfOperators = +VE | !=
-- MemAddress  = Integer
```

### Addresses

```
-- Reference   = Variable | MemAddress
-- ValueAt     = [Reference]
-- Address     = Reference
```

### Instructions

```
-- Conditional = If IfOperator Register Operator MemAddress
-- Instruction = Register Operator Address  | Stop | Conditional
```
