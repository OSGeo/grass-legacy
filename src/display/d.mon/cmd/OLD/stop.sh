GIS_LOCK=${GIS_LOCK-$$}
export GIS_LOCK
${GISBASE?}/etc/stop.mon $*
