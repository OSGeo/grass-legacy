# ******************************************************************
# IN:	pide	Fichero (con categorias, tags)  en formato dado por
#		genamap (r2ex).

# OUT:	pide	Fichero en formato GRASS.
#	  "	Fichero GRASS de categorias.
# ******************************************************************

echo "Nombre del fichero exportado en genamap con r2ex:"
echo "Por ejemplo: work_area/name_file.EE"
read nomdelta
entrada=/home/deltam/$nomdelta

if test -s $entrada
then

destino=$LOCATION
echo "Nombre del mapa GRASS que se va a crear:"
read nomgrass
salida1=$LOCATION/dig_ascii/$nomgrass
salida2=$LOCATION/dig_att/$nomgrass

if test -s $salida1
then
	echo $salida1"
	echo " **** FICHERO YA CREADO  "
	echo " **** ANTES EJECUTAR:  u.rm $1  "
else
awk '
{
if(substr($1,length($1)-3,4)=="EDGE"){
	print "A",$2 >> "'$salida1'"
	print $1,$2," *** ","A",$2
	}
else 
if(substr($1,length($1)-4,5)=="POINT"){
	print "P",$2 >> "'$salida1'"
	print $1,$2," *** ","P",$2
	}
else 
if(substr($1,length($1)-3,4)=="LINE"){
	print "A",$4 >> "'$salida1'"
	print $1,$4," *** ","A",$4
	}
else 
  if(substr($1,length($1)-2,3)=="TAG"){
	categ=$2
	linea=getline
	X=$1
	Y=$2
	printf "A%15.2f%15.2f%7d\n",X,Y,categ >> "'$salida2'"
	}
  else
	{
	X=$1
	Y=$2
	printf "%13.2f%13.2f\n",Y,X >> "'$salida1'"
	}

}
' $entrada

cat cab $salida1 > $salida1.cab
mv $salida1.cab $salida1
chown grass4 $salida1
chown grass4 $salida2
mv $salida1 $destino/dig_ascii/$salida1
mv $salida2 $destino/dig_att/$salida1
fi
else
	echo "No existe $entrada"
fi
