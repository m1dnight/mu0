######################################################################
# This program multiplies two numbers.                               #
# X * Y = Z                                                          #
#                                                                    #
# We assume the following:                                           #
#                                                                    #
# - X is located at storeline 40                                     #
# - Y is located at storeline 41                                     #
# - Z will be located at storeline 42                                #
#                                                                    #
######################################################################
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

######################################################################
# This program should compile to the following binary .              #
# 0x00 0x00 | 0x01 0x2A | 0x00 0x2A | 0x02 0x28 | 0x01 0x2A          #
# 0x00 0x29 | 0x03 0x14 | 0x01 0x29 | 0x06 0x04 | 0x07               #
######################################################################
