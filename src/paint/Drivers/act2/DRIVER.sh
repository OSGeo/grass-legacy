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

HRES=85.8
VRES=87.0
NCHARS=110

TEXTSCALE=1.0
TEXTFUDGE=3
TEXTSPACE=1
BLOCKSIZE=23
BLOCKSPACE=13
NBLOCKS=25

if test $SHORT
then
    NCHARS=75
    NBLOCKS=15
fi

export MAPLP BAUD
export HRES VRES NCHARS
export TEXTSCALE TEXTSPACE TEXTFUDGE BLOCKSIZE BLOCKSPACE NBLOCKS

exec ${PAINT_DRIVER?}
