#include <ctype.h>
#include <stdio.h>

/* exit codes */
#define SP_FATAL     1	 [ G_fata_error () returns 1 ]
#define SP_NOCHANGE  2
#define SP_UNKOWN    3

#define NUNITS 12
#define UNIT_LENGTH 30

#define LL       0
#define UTM      1
#define STP      2
#define AEA      3
#define LCC      4
#define MERC     5
#define TMERC    6
#define LEAC     7
#define LAEA     8
#define AEQD     9
#define AIRY    10
#define AITOFF  11
#define ALSK    12
#define APIAN   13
#define AUGUST  14
#define BACON   15
#define BIPC    16
#define BOGGS   17
#define BONNE   18
#define CASS    19
#define CC      20
#define CEA     21
#define CHAMB   22
#define COLLG   23
#define CRAST   24
#define DENOY   25
#define ECK1    26
#define ECK2    27
#define ECK3    28
#define ECK4    29
#define ECK5    30
#define ECK6    31
#define EQC     32
#define EQDC    33
#define EULER   34
#define FAHEY   35
#define FOUC    36
#define FOUC_S  37
#define GALL    38
#define GINS8   39
#define GN_SINU 40
#define GNOM    41
#define GOODE   42
#define GS48    43
#define GS50    44
#define HAMMER  45
#define HATANO  46
#define IMW_P   47
#define KAV5    48
#define KAV7    49
#define LABRD   50
#define LAGRNG  51
#define LARR    52
#define LASK    53
#define LEE_OS  54
#define LOXIM   55
#define LSAT    56
#define MBT_S   57
#define MBT_FPS 58
#define MBTFPP  59
#define MBTFPQ  60
#define MBTFPS  61
#define MIL_OS  62
#define MILL    63
#define MOLL    64
#define MPOLY   65
#define MURD1   66
#define MURD2   67
#define MURD3   68
#define NELL    69
#define NELL_H  70
#define NICOL   71
#define NSPER   72
#define NZMG    73
#define OB_TRAN 74
#define OCEA    75
#define OEA     76
#define OMERC   77
#define ORTEL   78
#define ORTHO   79
#define PCONIC  80
#define POLY    81
#define PUTP1   82
#define PUTP2   83
#define PUTP3   84
#define PUTP3P  85
#define PUTP4P  86
#define PUTP5   87
#define PUTP5P  88
#define PUTP6   89
#define PUTP6P  90
#define QUA_AUT 91
#define ROBIN   92
#define RPOLY   93
#define SINU    94
#define SOMERC  95
#define STERE   96
#define TCC     97
#define TCEA    98
#define TISSOT  99
#define TPEQD  100
#define TPERS  101
#define UPS    102
#define URM5   103
#define URMFPS 104
#define VANDG  105
#define VANDG2 106
#define VANDG3 107
#define VANDG4 108
#define VITK1  109
#define WAG1   110
#define WAG2   111
#define WAG3   112
#define WAG4   113
#define WAG5   114
#define WAG6   115
#define WAG7   116
#define WEREN  117
#define WINK1  118
#define WINK2  119
#define WINTRI 120

#define NPROJES 121

/* LATSTUFF  -90 <> +90 */
#define LAT0     0
#define LAT1     1
#define OLAT1    2
#define LAT2     3
#define OLAT2    4
#define LAT3     5
#define LATTS    6
#define LATB     7
#define OLATP    8
#define LATHIGH  9

/* LONSTUFF -180 <> 180 */
#define LONLOW   8
#define LON0     9
#define LON1    10
#define OLON1   11
#define LON2    12
#define OLON2   13
#define LON3    14
#define LONC    15
#define OLONC   16
#define OLONP   17
#define ALPHA   18
#define OALPHA  19
#define THETA   20
#define LONHIGH 21

/* DECIMAL DEGREES */
#define FLOLOW  20
#define AZIM    21
#define TILT    22

/* FLOATS */
#define HEIGH   23
#define KFACT   24
#define MFACT   25
#define MSFACT  26
#define NFACT   27
#define QFACT   28
#define WFACT   29
#define X0      30
#define Y0      31
#define FLOHIGH 32

/* INTEGERS */
#define INTLOW  31
#define ZONE    32
#define SNUM    33
#define SPATH   34
#define INTHIGH 35

/* BOOLEAN */
#define BOOLOW  34
#define GUAM    35
#define LOTSA   36
#define NOCUT   37
#define NODEFS  38
#define NOROT   39
#define NOSKEW  40
#define NOUOFF  41
#define ROTCONV 42
#define SOUTH   43
#define BOOHIGH 44

/* OTHER PROJECTION FOR ob_tran */
/* #define OPROJ   44 */

#define NOPTIONS 44
#define NLLSTUFF 21
#define NLATOPTS  9
#define NLONOPTS 12

#define RADIUS_DEF 6370997.

int ier, proj_index, zone, snum, spath;

double radius, kfact, mfact, msfact, nfact, qfact, wfact, unit_fact, x_false, y_false, heigh, azim, tilt;

double prompt_num_double();

struct opt_req {
	int ask;
	int def_exists;
	double deflt;
};

struct conv_fact {
	char unit[UNIT_LENGTH];
	char units[UNIT_LENGTH];
	double fact;
};


struct used_opt {
	int was;
	double val;
};

struct used_opt USED_in[NOPTIONS];
struct used_opt USED_out[NOPTIONS];

struct conv_fact UNITS[NUNITS];

struct opt_req TABLE[NPROJES][NOPTIONS];

char DESC[NOPTIONS][63];

double LLSTUFF[NLLSTUFF];


