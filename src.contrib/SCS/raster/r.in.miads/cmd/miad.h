/* @(#)miad.h	1.0  8/2/87 */

struct MIAD_INFO
{
    double UTM_NORTH;     /* UTM Northing reference */
    double UTM_EAST;      /* UTM Easting reference */
    int ORIGIN_STRIP;     /* num. of strip containing reference */
    int ORIGIN_LINE;      /* num. of line in above strip contain. ref. */
    int ORIGIN_CELL;      /* num. of cell in above line contain. ref. */
    int CELL_SIZE;        /* size in meters of each MIADS cell */
    char MIADS_MAP_NAME[40];  /* MIADS data set name */
    char OUTPUT_FILE[40];     /* user output file of Mimportcell format */
    int minc;              /* minimum cell in a strip */
    int mins;              /* minimum strip number */
    int minl;              /* minimum line number in a strip */
    int maxc;              /* maximum cell in a strip */
    int maxs;              /* maximum strip number */
    int maxl;              /* maximum line number in a strip */
};

struct Cat_list
{
	char *label;
};

static int ier, cnt, i, k=0, n_read, DONE=0;
static int  record, reccnt, outcnt, strip_no, line_no;
static int  icode, row, col, recrd, recnt, rows, cols;
static int  swit, last_strip=0, cat_cnt=0, last_cat_cnt=0;
static int  data_cell_cnt=0, cellcnt=0,j,m,value();
static char dummy, null='\0';
static char buffer [82], sav_bufr[81];
static char *str, *lin, Data[73];
static char Bytes[3], STRIP[3], LINE[5], DUM[1];
static char Output_name[40], *tempname;
static char answer[30];
static char category_file[1000][3];
static double N_bnd, S_bnd, E_bnd, W_bnd;
static double maxN, minS, maxE, minW;
static unsigned char *ptr;
static int input_file;
/* proces.c */
int proces(struct MIAD_INFO *);
int mvbyt(int, char *, char *);
