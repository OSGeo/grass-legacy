: '%W% %G%'  ${PAINTER?} ${PAINT_DRIVER?}

MAPLP=/dev/ttyb              
BAUD=9600

HRES=203.2
VRES=200.0
NCHARS=80

TEXTSCALE=1.0
TEXTFUDGE=0
TEXTSPACE=2
BLOCKSIZE=70
BLOCKSPACE=10
NBLOCKS=16

export MAPLP BAUD
export HRES VRES NCHARS
export TEXTSCALE TEXTSPACE TEXTFUDGE BLOCKSIZE BLOCKSPACE NBLOCKS

exec ${PAINT_DRIVER?}
