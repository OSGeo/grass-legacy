#!/bin/csh -f
# dC3.csh
# This script was run using /bin/csh on a rs6000

# This shell script generates correlograms to determine dted -> dem 
# spatial statistics.

g.region region=StudyArea -p
g.region n=s+3000 e=w+3000 -p
d.erase

@ d = 3600
while ( $d <= 4600 )
    @ f = 125
    while ( $f <= 275 )
        @ e = 7
        while ( $e <= 9 )
            c.correl2v in=d2d.0{$e}d{$d}f$f out=d2d.0{$e}d{$d}f$f
            d.vect d2d.0{$e}d{$d}f$f col=red
            @ e = $e + 1
        end
        @ f = $f + 75
    end
    @ d = $d + 500
end

c.correl2v in=d2d.07d3800f150 out=d2d.07d3800f150
d.vect d2d.07d3800f150 col=green
c.correl2v in=EMDIcor out=EMDIcor
d.vect EMDIcor col=white

END:
