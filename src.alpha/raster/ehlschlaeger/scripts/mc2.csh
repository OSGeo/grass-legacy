#!/bin/csh -f
# mC2.csh
# This script was run using /bin/csh on a rs6000

# This shell script generates correlograms to determine dted -> dem 
# spatial statistics.

g.region region=StudyArea -p
g.remove rast=MASK
d.erase

# calculate error of dted from interpMeanDted.csh
# This used to determine maximum extent of spatial correlation to 
# calculate least squares fit too.

# r.mapcalc ErrorMDI = 'dem - MeanDtedInterp'
# g.region n=n-1000 s=s+1000 e=e-1000 w=w+1000 -p
# r.random.cells output=RandSample dist=0 seed=17
# r.mapcalc RandSample = "if( RandSample >= 1 && RandSample <= 800 )"
# r.neighbors input=RandSample output=WideRandSample method=maximum size=7
# r.mapcalc MASK = 'if(WideRandSample)'
# r.random input=MASK rast=newMask nsites=10%
# g.region region=StudyArea -p
# r.mapcalc MASK = 'if( newMask )'
# r.1Dcorrelogram input=ErrorMDI out=EMDIcor max=3600 lags=20
# c.correl2v in=EMDIcor out=EMDIcor

r.mapcalc MASK = 'if( newMask )'

# best results from mC1.csh
echo 1.146915 lsf from EMDIcor and d2d.07d3000f100 best previous > mC2results
# 1.284569 lsf from EMDIcor and d2d.07d3000f200
# 1.321645 lsf from EMDIcor and d2d.07d3000f300
# 1.492547 lsf from EMDIcor and d2d.07d2500f100
# 1.508158 lsf from EMDIcor and d2d.07d2500f300
# 1.572464 lsf from EMDIcor and d2d.07d2500f200
# 1.700540 lsf from EMDIcor and d2d.07d2000f300

@ d = 2800
while ( $d <= 3800 )
    @ f = 50
    while ( $f <= 250 )
        @ e = 5
        while ( $e <= 9 )
            echo d $d   e .0$e  f $f d2d.0{$e}d{$d}f$f
            r.random.surface -q out=RF di=$d ex=.0$e flat=$f hi=1000 se=7
            r.mapcalc RF = "@RF * 144.5753"
            r.1Dcorrelogram -q in=RF out=d2d.0{$e}d{$d}f$f lags=20 max=2840
            least.squares max=2840 input=EMDIcor,d2d.0{$e}d{$d}f$f > mC2value
            cat mC2value
            cat mC2results mC2value > mC2result
            mv mC2result mC2results
            c.correl2v in=d2d.0{$e}d{$d}f$f out=d2d.0{$e}d{$d}f$f
            d.vect d2d.0{$e}d{$d}f$f col=red
            d.vect EMDIcor col=green
            @ e = $e + 2
        end
        @ f = $f + 100
    end
    @ d = $d + 500
end

END:
