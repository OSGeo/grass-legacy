#!/bin/sh

# set gis variables
eval `sed '1,$s/^\([A-Za-z_]*\): *\(.*\)/\1="\2"; /' $GISRC`

# set defaults
TMP=/tmp/.3-d.$$
MAP=elevation
EXAG=3
EF=elevation
FH=30000
SV=0
LF=20
BACK=black

# evaluate arguments
for i do
	case $i in
		file=*)
			MAP=`echo $i | sed s/file=//` ;;
		name=*)
			MAP=`echo $i | sed s/name=//` ;;
		ef=*)
			EF=`echo $i | sed s/ef=//` ;;
		fh=*)
			FH=`echo $i | sed s/fh=//` ;;
		vh=*)
			FH=`echo $i | sed s/vh=//` ;;
		sv=*)
			SV=`echo $i | sed s/sv=//` ;;
		lf=*)
			LF=`echo $i | sed s/lf=//` ;;
		exag=*)
			EXAG=`echo $i | sed s/exag=//` ;;
		back=*)
			BACK=`echo $i | sed s/back=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: file=mapname ef=mapname vh=viewing_height sv=sink_value exag=exag lf=line_freq back=background-color
			echo Defaults:
			echo "   mapname =        $MAP"
			echo "   exag    =        $EXAG"
			echo "   ef      =        $EF"
			echo "   viewing_height = $FH"
			echo "   sink_value=      $SV"
			echo "   line_freq =      $LF"
			echo "   back(ground) =   $BACK"
			exit
	esac
done

echo MAP: $MAP
echo EXAG: $EXAG
echo EF: $EF
echo FH: $FH

Dclear.screen

# Center map display
Dnew C  38 62 34 66
Dchoose C
Dcell $MAP
Dnew Ch  62 66 34 66
Dchoose Ch
(echo ".S 80" ; cat $LOCATION/../PERMANENT/MYNAME) | Dtext
Dnew Cf  34 38 34 66
Dchoose Cf
(echo ".S 80" ; echo $MAP) | Dtext

# Get the window edges
Gwindow print | sed "s/\..*//" > $TMP
N=`grep north $TMP | sed "s/[^0-9]*//"`
S=`grep south $TMP | sed "s/[^0-9]*//"`
E=`grep east $TMP | sed "s/[^0-9]*//"`
W=`grep west $TMP | sed "s/[^0-9]*//"`
rm $TMP

# center view
CVN=`expr $N + $S`
CVN=`expr $CVN / 2`
CVE=`expr $E + $W`
CVE=`expr $CVE / 2`

# delta dist from center view
NS=`expr $N - $S`
EW=`expr $E - $W`
# distance for N,S,E, and W view
D=`expr $NS + $EW`
D=`expr $D`
# distance for diagonal views
DD=`expr $D \* 1000`
DD=`expr $DD / 1414`
sleep 2

# view from the SOUTHWEST
Dnew SW  1 33  1 33
VN=`expr $S - $DD`
VE=`expr $W - $DD`
Dchoose SW
Derase $BACK
echo D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
sleep 2
Dnew SWl   1 6 6 28
Dchoose SWl
Derase black
Dtext << END
.S 80
 SW to NE
END

# view from the WEST
Dnew W  34 66  1 33
VN=$CVN
VE=`expr $W - $D`
Dchoose W
Derase $BACK
echo D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
sleep 2
Dnew Wl   34 39 6 28
Dchoose Wl
Derase black
Dtext << END
.S 80
  W to E
END

# view from the NORTHWEST
Dnew NW 67 99  1 33
VN=`expr $N + $DD`
VE=`expr $W - $DD`
Dchoose NW
Derase $BACK
echo D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
sleep 2
Dnew NWl   67 72 6 28
Dchoose NWl
Derase black
Dtext << END
.S 80
 NW to SE
END

# view from the NORTH
Dnew N  67 99 34 66
VN=`expr $N + $D`
VE=$CVE
Dchoose N
Derase $BACK
echo D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
sleep 2
Dnew Nl   67 72 39 61
Dchoose Nl
Derase black
Dtext << END
.S 80
  N to S
END

# view from the NORTHEAST
Dnew NE 67 99 67 99
VN=`expr $N + $DD`
VE=`expr $E + $DD`
Dchoose NE
Derase $BACK
echo D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
sleep 2
Dnew NEl   67 72 72 94
Dchoose NEl
Derase black
Dtext << END
.S 80
 NE to SW
END

# view from the EAST
Dnew E  34 66 67 99
VN=$CVN
VE=`expr $E + $D`
Dchoose E
Derase $BACK
echo D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
sleep 2
Dnew El   34 39 72 94
Dchoose El
Derase black
Dtext << END
.S 80
  E to W
END

# view from the SOUTHEAST
Dnew SE  1 33 67 99
VN=`expr $S - $DD`
VE=`expr $E + $DD`
Dchoose SE
Derase $BACK
echo D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
sleep 2
Dnew SEl   1 6 72 94
Dchoose SEl
Derase black
Dtext << END
.S 80
 SE to NW
END

# view from the SOUTH
Dnew S   1 33 34 66
VN=`expr $S - $D`
VE=$CVE
Dchoose S
Derase $BACK
echo D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
D3d file=$MAP tn=$CVN te=$CVE th=$SV fn=$VN fe=$VE fh=$FH exag=$EXAG ef=$EF lf=$LF
sleep 2
Dnew Sl   1 6 39 61
Dchoose Sl
Derase black
Dtext << END
.S 80
  S to N
END

Dchoose C
