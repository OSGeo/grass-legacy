# This is the g.gnuplot test file for s.sample -C
#
# edit cubic.c and #define DEBUG 1
# this will print out the 4x4 neighbor hood along with a diagnostic
# message for each point sampled, e.g,
# ----------------
# 2 0 4 3 
# 2 2 4 3 
# 1 1 1 1 
# 1 1 1 1 
# DIAG: (4,2) 1=2.6 2=3.4 3=  1 4=  1     e=0.649299 n=0.663327
# 29.19438878|3.36673347|1.8884
# ----------------
# 
# a,b,c,d are values of cells along the top row of the neighborhood.
a=2.0
b=0.0
c=4.0
d=3.0
#
# 1=2.6 indicates that interpolating along this first row gave
# a value of 2.6. e & n are the location of the points normalized
# to a grid spacing of unity (see the tutorial). So, at x=0.649299
# y should be equal to 2.6. I put this in a data file called 'point'
# ----------------
# 0.649299 2.6
# ----------------
#
# This is the polynomial evalulated by Horner's method.
plot [0:1] x*(x*(x*(d-c+b-a)+(c-d-2.0*b+2.0*a))+(c-a))+b, 2.6, 'point' w p
pause -1
