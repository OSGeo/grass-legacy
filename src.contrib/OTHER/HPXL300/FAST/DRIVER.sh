: ${PAINTER?} ${PAINT_DRIVER?}

# for networked machines, set the host which has the printer, and uncomment
# printer_host=. printer_host_alias=.
printer_host=Dale
 case `hostname` in
     $printer_host|$printer_host_alias);;
     *) exec rsh $printer_host $GISBASE/etc/paint/driver.rsh $PAINTER n
        exit 0;;
 esac

MAPLP=${MAPLP-/dev/$PAINTER}

BAUD=19200

HRES=150.0
VRES=150.0
NCHARS=120

TEXTSCALE=1
TEXTFUDGE=0
TEXTSPACE=2
BLOCKSIZE=90
BLOCKSPACE=70
NBLOCKS=15

export MAPLP BAUD
export HRES VRES NCHARS
export TEXTSCALE TEXTSPACE TEXTFUDGE BLOCKSIZE BLOCKSPACE NBLOCKS

exec ${PAINT_DRIVER?}
