
XDRIVER24 - src/display/devices/XDRIVER 
           author unknown 
24 bit color for graphics display - see compilation note for modification 
NOTE: the source must be placed under src/display/devices/XDRIVER directory




src/display/devices/XDRIVER24

problem -

    need to edit the Gmakefile for XINCPATH and XLIBPATH variables 

why -

    the above two variables are system dependent 

fixes -

    modify the src/display/devices/XDRIVER24/Gmakefile - line 8 and 9 
        XINCPATH=-I/usr/X11/include 
        XLIBPATH=-L/usr/X11/lib 

 