#include <ctype.h>
#include <stdio.h>


#define LL     0 
#define UTM    1
#define STP    2 
#define AEA    3
#define LCC    4
#define MERC   5
#define TMERC  6 

#define NPROJES 7

#define LAT0   0
#define LON0   1 
#define LAT1   2 
#define LAT2   3 
#define LATTS  4 
#define SOUTH  5
#define ZONE   6
#define KFACT  7 

#define NOPTIONS 8 

#define NUNITS 12 
#define UNIT_LENGTH 30

#define RADIUS_DEF 6370997.

struct opt_req {
  int ask;
  int def_exists;
  double deflt;
};

struct used_opt {
  int was;
  double val;
};

struct conv_fact {
  char unit[UNIT_LENGTH];
  char units[UNIT_LENGTH];
  double fact;
};

struct opt_req TABLE [NPROJES] [NOPTIONS];

struct conv_fact UNITS[NUNITS];

struct used_opt USED_in[NOPTIONS];
struct used_opt USED_out[NOPTIONS];

char   DESC[NOPTIONS][40];

int    proj_index_in, proj_index_out;

FILE *In_file, *Out_file, *Mem_file;
static char aline[80], answer[24], null = '\0';
static char command[1024], sav_comd[1024], parms_in[256], parms_out[256];
static char buff[300], in_file[60], out_file[60];
char *ptr, STabbr[3], COname[30], FIPSfile[60], TXT_ZONE[5];
char units_in[50], units_out[50];
double unit_fact_in, unit_fact_out;
char ellps_name_in[20], ellps_name_out[20], proj_name_in[20], proj_name_out[20];
char proj_title_in[100], proj_title_out[100];
int ier, icode, SFIPS, CFIPS, fipscode, COzone, NUM_ZON, lookup;
int record, reccnt;
int i, conv_typ, conv_way;
int input_typ, output_typ, rec_cnt;
int IDEG,IMIN,JDEG,JMIN,ST_zone;
float XDEG,XMIN,YDEG,YMIN,Q,XSEC,YSEC;
double LAT, LON, X, Y, LAT_res, LON_res, 
cur_LAT, cur_LON /* current lat and lon - equal either LAT and LON or LAT_res, LON_res*/;
double NOR, EAS, NOR_res, EAS_res;
char check; 
double radius_in, radius_out;


