# default is 5 by 3
set term latex
#set size 0.45,0.75
set size 0.45,0.585
# \begin{picture}(610,503)(88,68)
set nozeroaxis
set noxtics                         
set noytics 
set nokey
set out 'csr.latex'
plot 'csr.dat' w p 7 7
set out 'reg.latex'
plot 'reg.dat' w p 7 7
set out 'cls.latex'
plot 'cls.dat' w p 7 7 

