GRAPH=$1/mapgen
SHELL=/bin/sh
MSRC=`pwd`
INCL=`pwd`/include
MLIB=`pwd`/lib/$2
BIN=$GRAPH/bin
GRAPHB=$GRAPH/bin/plotter:$GRAPH/gfonts/:sr:
RANLIB=${RANLIB:-echo}
PATH=$PATH:$BIN
export SHELL GRAPH BIN INCL MLIB GRAPHB RANLIB PATH

