#!/bin/sh
# %W% %G%

clear
echo "SITES"

eval `gisenv`

cd
etc=${GISBASE?}/etc
PATH="$etc/sites:$etc:$PATH"
export PATH
exec $etc/sites/driver
