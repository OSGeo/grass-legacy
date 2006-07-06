#!/bin/sh

# sample script for Spearfish
# written by
#   andreas.philipp geo.uni-augsburg.de

dem=elevation.10m
g.region rast=${dem} -p

echo "Preparing input maps..."
r.slope.aspect --o elevation=$dem dx=${dem}_dx dy=${dem}_dy
r.mapcalc "${dem}_rain =if(${dem},10.0,null())"
r.mapcalc "${dem}_manin=if(${dem},0.04,null())"
r.mapcalc "${dem}_infil=if(${dem},0.9 ,null())"

echo "r.sim.water --o elevin=${dem} dxin=${dem}_dx dyin=${dem}_dy \
      rain=${dem}_rain manin=${dem}_manin infil=${dem}_infil \
      depth=${dem}_depth disch=${dem}_disch err=${dem}_err"
  
r.sim.water --o elevin=${dem} dxin=${dem}_dx dyin=${dem}_dy \
  rain=${dem}_rain manin=${dem}_manin infil=${dem}_infil \
  depth=${dem}_depth disch=${dem}_disch err=${dem}_err

r.info -r ${dem}_depth
r.info -r ${dem}_disch
r.info -r ${dem}_err

echo "Written:
 Output water depth raster file:     ${dem}_depth
 Output water discharge raster file: ${dem}_disch
 Output simulation error raster file: ${dem}_err
"
