#!/bin/csh -f
# dC1.csh
# This script was run using /bin/csh on a rs6000

g.region region=StudyArea -p
g.region n=s+3000 e=w+3000 -p
d.erase

@ d = 500
while ( $d <= 3000 )
    @ f = 100
    while ( $f <= 300 )
        @ e = 3
        while ( $e <= 7 )
            c.correl2v in=d2d.0{$e}d{$d}f$f out=d2d.0{$e}d{$d}f$f
            d.vect d2d.0{$e}d{$d}f$f col=red
            @ e = $e + 4
        end
        c.correl2v in=EMDIcor out=EMDIcor
        d.vect EMDIcor col=white
        @ f = $f + 100
    end
    @ d = $d + 500
end

END:
