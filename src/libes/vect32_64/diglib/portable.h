#define PORT_DOUBLE  8
#define PORT_FLOAT  4
#define PORT_LONG  4
#define PORT_SHORT 2

/*
** assumptions:
**    double =    8 byte IEEE
**    float  =    4 byte IEEE
**    long   =    4 byte int
**    short  =    2 byte int
**
*/
/* Native Machine Size tests */
#define NATIVE_DOUBLE 8
#define NATIVE_FLOAT 4
#define NATIVE_LONG 8
#define NATIVE_INT 4
#define NATIVE_SHORT 2

Translation matrices for PVF data
#define NATIVE_END_DOUBLE 0
#define NATIVE_END_FLOAT 0
#define NATIVE_END_LONG 0
#define NATIVE_END_SHORT 0
/* Double format: */
static int dbl_cnvrt[] = {7, 6, 5, 4, 3, 2, 1, 0};

/* Float format : */
static int flt_cnvrt[] = {3, 2, 1, 0};

/* Long format  : */
static int lng_cnvrt[] = {3, 2, 1, 0, 255, 255, 255, 255};

/* Short format : */
static int shrt_cnvrt[] = {1, 0};

