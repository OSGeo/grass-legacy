#!/bin/csh -f
# visPath.csh

set fromE = 244178.898
set fromN = 3826353.818

set toE = 235860
set toN = 3819840

g.region region=StudyArea -p

r.slope.aspect -z elev=dtedSA slope=dtedSASlope
r.mapcalc CostdtedSA = "if(dtedSASlope > 0,30+300*tan(dtedSASlope - 1)+max(0,dtedSA - 400),1500)"
r.cost input=CostdtedSA output=ConeTo coord=$toE,$toN
r.cost input=CostdtedSA output=ConeFrom coord=$fromE,$fromN
r.mapcalc PathCostDted = "ConeTo + ConeFrom"
r.describe PathCostDted
# 56445 is minimum value
r.mapcalc PathCostDted.re = 'PathCostDted * 1000 / 56445'
d.erase
cat rules | r.colors map=PathCostDted.re color=rules
d.rast PathCostDted.re
