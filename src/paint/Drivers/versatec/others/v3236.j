: ${GISBASE?}
# Set these 2 variables for your system (see the README file)
# TMPDIR1 is where the paint driver will write its file
# TMPDIR2 is where versatec sprint utility will write its temp files
#######################################################
TMPDIR1=/versatec
TMPDIR2=/versatec
#######################################################

if test "$TMPDIR1" = "$TMPDIR2"
then
    echo "PAINT - using $TMPDIR1 for temporary files" >& 2
else
    echo "PAINT - using $TMPDIR1 and $TMPDIR2 for temporary files" >& 2
fi

PAINT_DRIVER=$GISBASE/etc/paint/driver/versatec
MAPLP=/dev/vp0

ZOOM=1

case "$ZOOM" in
    1) NPIXELS=13600
       HRES=400.00
       VRES=400.00
       ;;
    2) NPIXELS=3400
       HRES=100.00
       VRES=100.00
       ;;
    3) NPIXELS=2266
       HRES=66.66667
       VRES=66.66667
       ;;
    4) NPIXELS=1700
       HRES=50.00
       VRES=50.00
       ;;
esac

TEXTSCALE=3.0
NCHARS=80
TEXTFUDGE=0
TEXTSPACE=3
BLOCKSIZE=25
BLOCKSPACE=23
NBLOCKS=20

RASTERFILE=$TMPDIR1/_paint
SPRINT="/bin/sprint >&2"
SPRINT_COMMAND="$SPRINT $RASTERFILE -ot direct -v -p 8936-4 -w $TMPDIR2 -x $ZOOM -y $ZOOM"

export NPIXELS
export RASTERFILE SPRINT_COMMAND
export MAPLP
export HRES VRES NCHARS 
export TEXTSCALE TEXTSPACE TEXTFUDGE BLOCKSIZE BLOCKSPACE NBLOCKS

PATDIR=$GISBASE/etc/paint
export PATDIR

exec ${PAINT_DRIVER?}
