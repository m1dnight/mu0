########################################################
# This program will compute the factorial of a number. #
#                                                      #
# We assume the following.                             #
#  - The number is stored at address 50.               #
#  - The result will be stored at address 51.          #
########################################################


# Set the result to 0.
       ACC <= [ONE]
       ACC => [51]


# Set the working memory.

       ACC <= [51]
       ACC => [60]
       
## We need to multiply ACC with ACC - 1, until ACC == 0.

# Put ACC and ACC-1 in the locations for multiplication.
ITER:  ACC <= [60]
       ACC => [40]
       
       ACC - [ONE]
       ACC => [41]
       
       ACC <= [60]
       ACC - [TWO]
       ACC => [60]

       PC <= MUL
       
MULTD: ACC <= [42]

       # Multiply result with temp result
       ACC => [40]
       
       ACC <= [51]
       ACC => [41]
       PC <= MUL2
       
MULTD2:ACC <= [42]
       ACC => [51]
       IF != 0, PC <= ITER
       STOP

# Multiply routine: x * y = z
# x in 40, y in 41, z in 42
MUL:  ACC <= [ZERO]
      ACC => [42]
LOOP: ACC <= [42]
      ACC + [40]
      ACC => [42]
      ACC <= [41]
      ACC - [ONE]
      ACC => [41]
      IF != 0, PC <= LOOP
      PC <= MULTD


MUL2: ACC <= [ZERO]
      ACC => [42]
LOOP2:ACC <= [42]
      ACC + [40]
      ACC => [42]
      ACC <= [41]
      ACC - [ONE]
      ACC => [41]
      IF != 0, PC <= LOOP2
      PC <= MULTD2
      
ZERO: 0
ONE : 1
TWO : 2

 
