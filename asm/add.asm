    ACC <= [L2]
    ACC => [112] 
L1: ACC <= [112]  
    ACC + [110]  
    ACC => [112]
    ACC <= [111]  
    ACC - [L3]
    ACC => [111]
    IF != 0, PC <= L1
    STOP
L2: 0
L3: 1
