:
if [ $# = 1 ]
then
  dec=$1
else
  dec=2
fi
dec=`expr $dec + 1`
while read line
do
set $line
echo "east:          $1"
echo "north:         $2"
echo "xoffset:"
echo "yoffset:"
echo "ref:           center"
echo "font:          standard"
echo "color:         black"
echo "size:          500"
echo "width:         1"
echo "hcolor:        none"
echo "hwidth:        0"
echo "background:    none"
echo "border:        black"
echo "opaque:        yes"

echo ""
set `echo $3 | tr '.' ' ' `
text=$1
text2=""
if [ $# = '2' ]
then
  text2=`echo $2 | colrm $dec`
  echo "text:$text.$text2"
else
  echo "text:$text"
fi
echo ""
done
  
