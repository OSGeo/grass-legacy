#!/bin/sh

        awk 'BEGIN {FS=" "} NR==1 {print "res="$1}' temp_file >temp_file2
        . temp_file2 
        g.copy rast=temp_cell_num,cell_num.map.$res 
