#!/bin/csh -f
# mC3.csh
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

# best results from mC2.csh
echo 0.862594 lsf from EMDIcor and d2d.07d3800f150 best mC2.csh > mC3results
# 0.957559 lsf from EMDIcor and d2d.09d3300f150
# 0.961318 lsf from EMDIcor and d2d.09d3800f250
# 0.998780 lsf from EMDIcor and d2d.07d3300f150
# 1.029675 lsf from EMDIcor and d2d.07d3800f250
# 1.049966 lsf from EMDIcor and d2d.09d2800f150
# 1.062857 lsf from EMDIcor and d2d.09d3300f250
# 1.092857 lsf from EMDIcor and d2d.09d3800f150
# 1.121960 lsf from EMDIcor and d2d.05d3800f150
# 1.146915 lsf from EMDIcor and d2d.07d3000f100 best previous from mC1.csh

@ d = 3600
while ( $d <= 4600 )
    @ f = 125
    while ( $f <= 275 )
        @ e = 7
        while ( $e <= 9 )
            echo d $d   e .0$e  f $f d2d.0{$e}d{$d}f$f
            r.random.surface -q out=RF di=$d ex=.0$e flat=$f hi=1000 se=7
            r.mapcalc RF = "@RF * 144.5753"
            r.1Dcorrelogram -q in=RF out=d2d.0{$e}d{$d}f$f lags=20 max=2840
            least.squares max=2840 input=EMDIcor,d2d.0{$e}d{$d}f$f > mC3value
            cat mC3value
            cat mC3results mC3value > mC3result
            mv mC3result mC3results
            c.correl2v in=d2d.0{$e}d{$d}f$f out=d2d.0{$e}d{$d}f$f
            d.vect d2d.0{$e}d{$d}f$f col=red
            c.correl2v in=d2d.07d3800f150 out=d2d.07d3800f150
            d.vect d2d.07d3800f150 col=green
            c.correl2v in=EMDIcor out=EMDIcor
            d.vect EMDIcor col=white
            @ e = $e + 1
        end
        @ f = $f + 75
    end
    @ d = $d + 500
end

END:
