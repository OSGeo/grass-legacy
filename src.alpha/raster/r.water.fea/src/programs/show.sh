d.frame -e
d.frame -c frame=r.water.fea1 at=0,100,0,50
d.frame -c frame=r.water.fea2 at=0,100,50,100
d.frame -s r.water.fea1
d.frame -c frame=basin at=25,100,8,50
d.rast fea.basin.$1
d.frame -c frame=lege at=25,100,0,8
d.legend map=fea.basin.$1 lines=30
d.frame -c frame=lelower at=0,25,0,25
d.graph << eof
move 4. 92.5
text TRAPEZOIDAL SECTION
move 4. 87.5
text -------------------
move 4. 80.
draw 28. 20.
draw 72. 20.
draw 96. 80.
move  6. 75.
draw 94. 75.
move 20. 71.
draw 80. 71.
move 35. 65.
draw 65. 65.
color yellow
move 8.72 67.
draw 8.72 39
draw 20. 39
move 28. 15.
draw 28. 10.
move 28. 12.5
draw 72. 12.5
move 72. 15.
draw 72. 10.
color white
move 10. 34.
text n
move 5. 50.
text 1
move 50. 6.
text W
eof
d.frame -c frame=rilower at=0,25,25,50
grass.logo.sh
d.frame -c frame=ligend at=0,100,50,58
d.legend map=fea.basin.$1 lines=30
d.frame -c frame=uppper at=55,100,58,100
d.rast fea.basin.$1
d.frame -c frame=midddle at=45,55,58,100
d.graph << eof
move 12. 40.
size 5. 40.
text PROJECT: $1
eof
d.frame -c frame=loweer at=0,45,58,100
d.rast fea.stream.$1
