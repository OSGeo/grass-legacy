#!/bin/csh -f
# visDEM.csh

set fromE = 244178.898
set fromN = 3826353.818

set toE = 235860
set toN = 3819840

g.region region=StudyArea -p

r.slope.aspect -z elev=dem slope=demSlope
r.mapcalc CostDEM = "if(demSlope > 0,30+300*tan(demSlope - 1)+max(0,dem - 400),1500)"
r.cost input=CostDEM output=DEMConeTo coord=$toE,$toN
r.cost input=CostDEM output=DEMConeFrom coord=$fromE,$fromN
r.mapcalc PathCostDEM = "DEMConeTo + DEMConeFrom"
r.describe PathCostDEM
# 61263 is minimum value
r.mapcalc PathCostDEM.re = 'PathCostDEM * 1000 / 61263'
d.erase
cat rules | r.colors map=PathCostDEM.re color=rules
d.rast PathCostDEM.re
