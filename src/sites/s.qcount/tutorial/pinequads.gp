#set size 0.45,0.585
set size 0.90,1.17
set nozeroaxis
set noytics
set noxtics
set nokey
set term latex
set out 'pinequads.latex'
plot 'trees' w d, 'circles' w p
