#!/bin/csh -f
# mC4b.csh
# This script was run using /bin/csh on a rs6000

@ d = 4400
while ( $d <= 5400 )
    @ f = 150
    while ( $f <= 250 )
        @ e = 65
        while ( $e <= 70 )
            echo d $d   e .0$e  f $f d2d.0{$e}d{$d}f$f
            r.random.surface -q out=RF di=$d ex=.0$e flat=$f hi=1000 se=7
            r.mapcalc RF = "@RF * 144.5753"
            r.1Dcorrelogram -q in=RF out=d2d.0{$e}d{$d}f$f lags=20 max=2840
            least.squares max=2840 input=EMDIcor,d2d.0{$e}d{$d}f$f > mC4value
            cat mC4value
            cat mC4results mC4value > mC4result
            mv mC4result mC4results
            c.correl2v in=d2d.0{$e}d{$d}f$f out=d2d.0{$e}d{$d}f$f
            d.vect d2d.0{$e}d{$d}f$f col=red
            c.correl2v in=d2d.07d3800f150 out=d2d.07d3800f150
            d.vect d2d.07d3800f150 col=green
            c.correl2v in=EMDIcor out=EMDIcor
            d.vect EMDIcor col=white
            @ e = $e + 5
        end
        @ f = $f + 50
    end
    @ d = $d + 500
end

END:
