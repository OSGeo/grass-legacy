#define CM      Current_map

#define USE_PTR  0
#define USE_LINE 1
#define USE_BOX  2

/* Digitize menu */
#define MDC_MODE	'm'
#define MDC_TYPE	't'
#define MDC_LABEL	'l'
#define MDC_UNLABEL	'u'
#define MDC_REPLOT	'*'
#define MDC_DIGIT	' '

/* Edit menu */
#define MEC_REMOVE 	'r'
#define MEC_BLOCK 	'R'
#define MEC_RMVSIT 	'i'
#define MEC_INSEC 	'S'
#define MEC_MOVE	'm'
#define MEC_MOVEL	'M'
#define MEC_SNAP	's'
#define MEC_MARK	'p'
#define MEC_DISP	'd'
#define MEC_BREAK	'b'
#define MEC_NBREAK	'n'
#define MEC_TYPE	't'


/* Toolbox menu */
#define MTC_DUPLICATE	1
#define MTC_GARBAGE	2
#define MTC_MEMORY	3
#define MTC_WRITE	4
#define MTC_REGIST	5
#define MTC_NEAT	6
#define MTC_SHELL	7
#define MTC_NODELINES	8
#define MTC_ULAREAS	9	
#define MTC_BADAREAS	10
#define MTC_ISLES	11	

/* Label menu */
#define MLC_LAREA 	'a'
#define MLC_LLINE	'l'
#define MLC_LSITE	's'
#define MLC_PLINE	'p'
#define MLC_ULAREA	'A'
#define MLC_ULLINE	'L'
#define MLC_ULSITE	'S'
#define MLC_UPLINE	'P'
#define MLC_SLINES	'h'
#define MLC_SAREAS	'd'
#define MLC_LLINES	'B'
#define MLC_MLINES	'm'
#define MLC_UMLINES	'M'
#define MLC_CONTOUR	'c'
#define MLC_INTERV	'i'

/* Window menu */
#define MWC_WIND	12
#define MWC_PREV	13
#define MWC_CENT	14
#define MWC_LINES	15
#define MWC_SITES	16
#define MWC_SLABELS	17
#define MWC_NODES	18
#define MWC_LABELS	19
#define MWC_LLINES	20
#define MWC_LLABELS	21
#define MWC_CLEAR	22
#define MWC_WHERE	23
#define MWC_SCALE	24
#define MWC_OVERLAY	25
#define MWC_BACKDROP	26
#define MWC_ULAREAS	27
#define MWC_THRESH	28
#define MWC_ISLES	11
#define MWC_QUIT	30

#define NUM 0
#define PREV 1
#define NEXT 2
#define DONE 3
#define RIGHTMOUSE 3

#define ACCEPT 4
#define MIDDLEMOUSE 4
#define BACKUP 4

#define FIND 5
#define DIG_POINT 5
#define LEFTMOUSE 5

#define DRAW 6

#define FIRST 0
#define SECOND 1
#define ARC_RES 60
#define DOT_RES 6
#define MAX_PULL_PTS 3000	/* TODO !!! */

/*
#define ONEPIX U_to_D_xconv
#define TWOPIX 2.0*U_to_D_xconv
#define TRIPIX 3.0*U_to_D_xconv
*/

#define ONEPIX Pix_size
#define TWOPIX 2.0*Pix_size
#define TRIPIX 3.0*Pix_size

#define EPSILON .000001

typedef struct pnt_node{
	double pnt[2];
	struct pnt_node *prior;
	struct pnt_node *next;
} Pntlist;

typedef struct {
	double x[3], y[3];              /* only three control pts */
	double A[2], B[2], C[2], D[2];  /*coeff, 2 segs defined by 3 pts*/
} CubSpline;

typedef struct {
	double cen[2];
	double rad;
} Circle;
typedef double  (Dfunc_ptr)();
typedef int     (Func_ptr)();

