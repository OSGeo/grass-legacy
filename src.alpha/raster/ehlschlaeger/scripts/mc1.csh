#!/bin/csh -f
# mC1.csh
# This script was run using /bin/csh on a rs6000

# This shell script generates correlograms to determine dted -> dem 
# spatial statistics.

g.region region=StudyArea -p
g.remove rast=MASK
d.erase

# calculate error of dted from interpMeanDted.csh
# This used to determine maximum extent of spatial correlation to 
# calculate least squares fit too.  dem is a map of higher quality,
# a 7.5 minute DEM.  While MeanDtedInterp is a DTED, a 1 degree
# DEM.  It is interpolated to 30 meter resolution and the mean error
# has been adjusted based on the tangent of slope of the various
# cells.

r.mapcalc ErrorMDI = 'dem - MeanDtedInterp'
g.region n=n-1000 s=s+1000 e=e-1000 w=w+1000 -p
r.random.cells output=RandSample dist=0 seed=17
r.mapcalc RandSample = "if( RandSample >= 1 && RandSample <= 800 )"
r.neighbors input=RandSample output=WideRandSample method=maximum size=7
r.mapcalc MASK = 'if(WideRandSample)'
r.random input=MASK rast=newMask nsites=10%
g.region region=StudyArea -p
r.mapcalc MASK = 'if( newMask )'
r.1Dcorrelogram input=ErrorMDI out=EMDIcor max=3600 lags=20
c.correl2v in=EMDIcor out=EMDIcor

r.mapcalc MASK = 'if( newMask )'

echo Results of mC1.csh > mC1results

@ d = 500
while ( $d <= 3000 )
    @ f = 100
    while ( $f <= 300 )
        @ e = 3
        while ( $e <= 7 )
            echo d $d   e .0$e  f $f d2d.0{$e}d{$d}f$f
            r.random.surface -q out=RF di=$d ex=.0$e flat=$f hi=1000 se=7
            r.mapcalc RF = "@RF * 144.5753"
            r.1Dcorrelogram -q in=RF out=d2d.0{$e}d{$d}f$f lags=20 max=2840
            least.squares max=2840 input=EMDIcor,d2d.0{$e}d{$d}f$f > mC1value
            cat mC1value
            cat mC1results mC1value > mC1result
            mv mC1result mC1results
            c.correl2v in=d2d.0{$e}d{$d}f$f out=d2d.0{$e}d{$d}f$f
            d.vect d2d.0{$e}d{$d}f$f col=red
            d.vect EMDIcor col=green
            @ e = $e + 4
        end
        d.vect EMDIcor col=green
        @ f = $f + 100
    end
    @ d = $d + 500
end

END:
