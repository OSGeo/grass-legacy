:

clear
echo "SITES"

eval `g.gisenv`

cd
etc=${GISBASE?}/etc
PATH="$etc/sites:$etc:$PATH"
export PATH
exec $etc/sites/driver
