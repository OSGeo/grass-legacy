#!/bin/csh -f
# dC2.csh
# This script was run using /bin/csh on a rs6000

g.region region=StudyArea -p
g.region n=s+3000 e=w+3000 -p
d.erase

@ d = 2800
while ( $d <= 3800 )
    @ f = 50
    while ( $f <= 250 )
        @ e = 5
        while ( $e <= 9 )
            c.correl2v in=d2d.0{$e}d{$d}f$f out=d2d.0{$e}d{$d}f$f
            d.vect d2d.0{$e}d{$d}f$f col=red
            @ e = $e + 2
        end
        @ f = $f + 100
    end
    @ d = $d + 500
end

# best results from mC1.csh
c.correl2v in=d2d.07d3000f100 out=d2d.07d3000f100
d.vect d2d.07d3000f100 col=green

c.correl2v in=EMDIcor out=EMDIcor
d.vect EMDIcor col=white

END:
