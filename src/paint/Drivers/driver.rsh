export GISBASE
PAINTER=${1?}
PAINT_DRIVER=$GISBASE/etc/paint/driver/$1
export PAINTER PAINT_DRIVER
TRANSPARENT=${2-a}
export TRANSPARENT
exec $GISBASE/etc/paint/driver.sh/$1
