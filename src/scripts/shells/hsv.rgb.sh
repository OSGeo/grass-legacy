:
# Converts HSV to RGB using r.mapcalc - based on Foley
# and Van Dam HSV algorithm.
#
# Usage: $0 H S V
#  must supply three raster files as input (HSV)
#  output is raster files R,G,B
#  It is a big mistake to use the same names in and out


if [ $# != 3 ]
then
    echo Usage: `basename $0` H S V  >&2
    exit 1
fi
H="$1"
S="$2"
V="$3"
R=R
G=G
B=B
V=V

x="$H"
case "$x" in
   \#*) x="`echo $x | sed s/.//`" ;;
esac
ok=yes
eval `g.findfile element=cell file=$x`
if [ ! "$file" ]
then
    echo "$x - cell file not found" >&2
    ok=no
fi

x="$S"
case "$x" in
   \#*) x="`echo $x | sed s/.//`" ;;
esac
eval `g.findfile element=cell file=$x`
if [ ! "$file" ]
then
    echo "$x - cell file not found" >&2
    ok=no
fi

x="$V"
case "$x" in
   \#*) x="`echo $x | sed s/.//`" ;;
esac
eval `g.findfile element=cell file=$x`
if [ ! "$file" ]
then
    echo "$x - cell file not found" >&2
    ok=no
fi

if [ $ok = no ]
then
    exit 1
fi
echo "Computing Red ($R), Green ($G), and Blue ($B)"
echo "From Hue ($H), Saturation ($S), and Value ($V)"

r.mapcalc << EOF
$R = eval( s = $S/255.0, \
	  h = if($H>=360,$H-360,$H)/60.0, \
	  i = int(h), \
	  f = h-i, \
	  p=$V*(1-s), \
	  q=$V*(1-s*f), \
	  t=$V*(1-s*(1-f)), \
	  if(i==0,$V)+if(i==1,q)+if(i==2,p)+ \
	  if(i==3,p)+if(i==4,t)+if(i==5,$V))
$G = eval( s = $S/255.0, \
	  h = if($H>=360,$H-360,$H)/60.0, \
	  i = int(h), \
	  f = h-i, \
	  p=$V*(1-s), \
	  q=$V*(1-s*f), \
	  t=$V*(1-s*(1-f)), \
	  if(i==0,t)+if(i==1,$V)+if(i==2,$V)+ \
	  if(i==3,q)+if(i==4,p)+if(i==5,p))
$B = eval( s = $S/255.0, \
	  h = if($H>=360,$H-360,$H)/60.0, \
	  i = int(h), \
	  f = h-i, \
	  p=$V*(1-s), \
	  q=$V*(1-s*f), \
	  t=$V*(1-s*(1-f)), \
	  if(i==0,p)+if(i==1,p)+if(i==2,t)+ \
	  if(i==3,$V)+if(i==4,$V)+if(i==5,q))
EOF
