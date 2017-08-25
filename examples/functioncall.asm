        ACC <= [X]
        PC <= WRITE
        STOP
        
        
        # Function to write ACC to memory position 30.
WRITE:  ACC => [30]
        PC <= RETURN
 
RETURN: STOP       

X:      10
Y:      20
Z:      30
