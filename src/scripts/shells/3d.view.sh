
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

d.frame -e

# Center map display
d.frame -c frame=C  at=38,62,34,66
d.frame -s C
d.rast $MAP
d.frame -c frame=Ch at=62,66,34,66
d.frame -s Ch
(echo ".S 80" ; cat $LOCATION/../PERMANENT/MYNAME) | d.text
d.frame -c frame=Cf at=34,38,34,66
d.frame -s Cf
(echo ".S 80" ; echo $MAP) | d.text

# Get the window edges
g.region -p | sed "s/\..*//" > $TMP
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
d.frame -c frame=SW at=1,33,1,33
VN=`expr $S - $DD`
VE=`expr $W - $DD`
d.frame -s SW
d.erase $BACK
d.3d from=$VE,$VN,$FH to=$CVE,$CVN,$SV exag=$EXAG map=$MAP elev=$EF lines=$LF
sleep 2
d.frame -c frame=SWl at=1,6,6,28
d.frame -s SWl
d.erase black
d.text << END
.S 80
 SW to NE
END

# view from the WEST
d.frame -c frame=W at=34,66,1,33
VN=$CVN
VE=`expr $W - $D`
d.frame -s W
d.erase $BACK
d.3d from=$VE,$VN,$FH to=$CVE,$CVN,$SV exag=$EXAG map=$MAP elev=$EF lines=$LF
sleep 2
d.frame -c frame=Wl at=34,39,6,28
d.frame -s Wl
d.erase black
d.text << END
.S 80
  W to E
END

# view from the NORTHWEST
d.frame -c frame=NW at=67,99,1,33
VN=`expr $N + $DD`
VE=`expr $W - $DD`
d.frame -s NW
d.erase $BACK
d.3d from=$VE,$VN,$FH to=$CVE,$CVN,$SV exag=$EXAG map=$MAP elev=$EF lines=$LF
sleep 2
d.frame -c frame=NWl at=67,72,6,28
d.frame -s NWl
d.erase black
d.text << END
.S 80
 NW to SE
END

# view from the NORTH
d.frame -c frame=N at=67,99,34,66
VN=`expr $N + $D`
VE=$CVE
d.frame -s N
d.erase $BACK
d.3d from=$VE,$VN,$FH to=$CVE,$CVN,$SV exag=$EXAG map=$MAP elev=$EF lines=$LF
sleep 2
d.frame -c frame=Nl at=67,72,39,61
d.frame -s Nl
d.erase black
d.text << END
.S 80
  N to S
END

# view from the NORTHEAST
d.frame -c frame=NE at=67,99,67,99
VN=`expr $N + $DD`
VE=`expr $E + $DD`
d.frame -s NE
d.erase $BACK
d.3d from=$VE,$VN,$FH to=$CVE,$CVN,$SV exag=$EXAG map=$MAP elev=$EF lines=$LF
sleep 2
d.frame -c frame=NEl at=67,72,72,94
d.frame -s NEl
d.erase black
d.text << END
.S 80
 NE to SW
END

# view from the EAST
d.frame -c frame=E at=34,66,67,99
VN=$CVN
VE=`expr $E + $D`
d.frame -s E
d.erase $BACK
d.3d from=$VE,$VN,$FH to=$CVE,$CVN,$SV exag=$EXAG map=$MAP elev=$EF lines=$LF
sleep 2
d.frame -c frame=El at=34,39,72,94
d.frame -s El
d.erase black
d.text << END
.S 80
  E to W
END

# view from the SOUTHEAST
d.frame -c frame=SE at=1,33,67,99
VN=`expr $S - $DD`
VE=`expr $E + $DD`
d.frame -s SE
d.erase $BACK
d.3d from=$VE,$VN,$FH to=$CVE,$CVN,$SV exag=$EXAG map=$MAP elev=$EF lines=$LF
sleep 2
d.frame -c frame=SEl at=1,6,72,94
d.frame -s SEl
d.erase black
d.text << END
.S 80
 SE to NW
END

# view from the SOUTH
d.frame -c frame=S at=1,33,34,66
VN=`expr $S - $D`
VE=$CVE
d.frame -s S
d.erase $BACK
d.3d from=$VE,$VN,$FH to=$CVE,$CVN,$SV exag=$EXAG map=$MAP elev=$EF lines=$LF
sleep 2
d.frame -c frame=Sl at=1,6,39,61
d.frame -s Sl
d.erase black
d.text << END
.S 80
  S to N
END

d.frame -s C


#              map   The raster map used to generate the color
#        elevation   The raster map used to generate the 3-d texture
#                    default: elevation
#  from_coordinate   coordinates of view point
#                    default: 683824,5442593,20000
#    to_coordinate   coordinates of center of view
#                    default: 703849,5461793,0
#     exaggeration   vertical exaggeration factor
#                    default: 2.0
#            lines   Frequency of vector lines in output
#                    default: 10
#            field   Field of view
#                    default: 40
#            color   Color of vector lines
#                    options:
#color,white,red,orange,yellow,green,blue,indigo,vio
#let,gray,black

