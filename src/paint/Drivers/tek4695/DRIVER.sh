: ${PAINTER?} ${PAINT_DRIVER?}

# for networked machines, set the host which has the printer, and uncomment
# printer_host=. printer_host_alias=.
# case `hostname` in
#     $printer_host|$printer_host_alias);;
#     *) exec rsh $printer_host $GISBASE/etc/paint/driver.rsh $PAINTER n
#        exit 0;;
# esac

MAPLP=${MAPLP-/dev/$PAINTER}

BAUD=9600

HRES=121.0
VRES=120.0
NCHARS=80

TEXTSCALE=1.0
TEXTFUDGE=0
TEXTSPACE=2
BLOCKSIZE=35
BLOCKSPACE=25
NBLOCKS=10

export MAPLP BAUD
export HRES VRES NCHARS
export TEXTSCALE TEXTSPACE TEXTFUDGE BLOCKSIZE BLOCKSPACE NBLOCKS

exec ${PAINT_DRIVER?}
