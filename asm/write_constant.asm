################################################################################
# After execution, this program should contain "100" at memory locatoin 10,    #
# and 200 at memory location 11.                                               #
################################################################################

    ACC <= [L1]
    ACC => [30]
    ACC <= [L2]
    ACC => [31]
    STOP
L1: 100
L2: 200
