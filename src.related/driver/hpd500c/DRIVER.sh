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

HRES=300.0
VRES=300.0
NCHARS=80
NPIXELS=2400

TEXTSCALE=1.0
TEXTFUDGE=0
TEXTSPACE=3
BLOCKSIZE=35
BLOCKSPACE=50
NBLOCKS=10

export MAPLP BAUD
export HRES VRES NCHARS NPIXELS
export TEXTSCALE TEXTSPACE TEXTFUDGE BLOCKSIZE BLOCKSPACE NBLOCKS

exec ${PAINT_DRIVER?}
