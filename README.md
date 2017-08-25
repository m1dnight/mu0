# mu0

## Grammar

An example program to multiply two numbers.

```
    ACC <= [X]
    ACC => [40]

    ACC => [Y]
    ACC => [41]

# Set the accumulator to 0.
    ACC <= [L2]

# Write our temporary result to memory address 42.
    ACC => [42]

# Start of sequence to keep adding.
L1: ACC <= [42]
    ACC + [40]
    ACC => [42]
    ACC <= [41]
    ACC - [L3]
    ACC => [41]
    IF != 0, PC <= L1
    STOP
L2: 0
L3: 20

X:  100
Y:    2
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

# Executing

 1. Compile the compiler with `cd compiler && stack install && cd ..`.
 2. Compile a sample program `mu0 examples/fac.asm fac`
 3. Compile the VM `cd vm & make & cd ..`
 4. Execute the program `./vm/main fac`

You will see the memory dump of the VM.
