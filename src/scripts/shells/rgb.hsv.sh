:
# Converts RGB to HSV using r.mapcalc - based on Foley
# and Van Dam HSV algorithm.
#
# Usage: $0 R G B
#  must supply three cell files as input (RGB)
#  output is cell files H,S,V
#  It is a big mistake to use the same names in and out


if [ $# != 3 ]
then
    echo Usage: `basename $0` R G B  >&2
    exit 1
fi
R="$1"
G="$2"
B="$3"
H=H
V=V
S=S

ok=yes
x="$R"
case "$x" in
   \#*) x="`echo $x | sed s/.//`" ;;
esac
eval `g.findfile element=cell file=$x`
if [ ! "$file" ]
then
    echo "$x - cell file not found" >&2
    ok=no
fi

x="$G"
case "$x" in
   \#*) x="`echo $x | sed s/.//`" ;;
esac
eval `g.findfile element=cell file=$x`
if [ ! "$file" ]
then
    echo "$x - cell file not found" >&2
    ok=no
fi

x="$B"
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
echo "Computing Hue ($H), Saturation ($S), and Value ($V)"
echo "From Red ($R), Green ($G), and Blue ($B)"

r.mapcalc << EOF
$V  =  max ($R, $G, $B)                   
$S  =  255.0 * ($V - min($R,$G,$B)) / V    
$H  =  if ($S, eval (                   \
         m = float(min ($R,$G,$B))  ,    \
         r = ($V - $R) / ($V - m)  ,     \
         g = ($V - $G) / ($V - m)  ,     \
         b = ($V - $B) / ($V - m)  ,     \
         h = if($R==$V,b-g)              \
           + if($G==$V,2+r-b)            \
           + if($B==$V,4+g-r)  ,        \
         if(h <= 0, h+6, h) * 60 ))
EOF
