
#!/bin/sh
#
#  This Bourne shell is used to display 
#  the temporary cell map created by the 
#  grass/agnps interface   
#  
#            Zhian Li, July, 1995
    
   
    echo "The Current Resolution is: $1"

    d.erase 
    g.region raster=temp_cell_num res=$1
    g.region -p >new.win
    d.rast map=temp_cell_num
