#define FNAME_SIZE  13
#define MODN_SIZE    5

/*  The following definitions conform to those in GRASS (the return
	values of G_projection()  */


#define UTM          1
#define SPCS         2
#define GEO          3
#define UPS          9 /*can we support UPS?*/
#define OTHR         99

struct SDTS_in_module {
   int index;
   int num;
   char *name;
   char *type;
};

#define IDEN  1
#define CATD  2
#define CATX  3
#define CATS  4
#define SCUr  5
#define IREf  6
#define XREF  7
#define SPDm  8
#define DDDf  9
#define DDOm  10
#define DDSh  11
#define STAT  12
#define DQHl  13
#define DQPa  14
#define DQAa  15
#define DQLc  16
#define DQCg  17
#define FFxx  18
#define Axxx  19
#define Bxxx  20
#define NOxx  21
#define NExx  22
#define NAxx  23
#define NLxx  24
#define NPxx  25
#define LExx  26
#define PCxx  27

#define FL_END 27


#ifdef MAIN
char *Mod_in [FL_END + 1] =
{
  NULL,
  "IDEN",
  "CATD",
  "CATX",
  "CATS",
  "SCUr",
  "IREf",
  "XREF",
  "SPDm",
  "DDDf",
  "DDOm",
  "DDSh",
  "STAT",
  "DQHl",
  "DQPa",
  "DQAa",
  "DQLc",
  "DQCg",
  "FFxx",
  "Axxx",
  "Bxxx",
  "NOxx",
  "NExx",
  "NAxx",
  "NLxx",
  "NPxx",
  "LExx",
  "PCxx"
};
#else
extern char *Mod_in[];
#endif

#define FIRST_OBJ_MODULE 21

struct scale_factors
{
   double x_sf;
   double y_sf;
   double x_org;
   double y_org;
};

struct file_list {
   char *name;
   char *next;
};

struct Sdts_manifold 
{
	int index;
	char file_suffix[4];
	char *mod_name [30];
	char *file_name [30];
	char *mfold_name;
	char *domain_name;
	char *map_name;
	char *theme_name;
	int mod_nrec[30];
	long n_lines;
	long n_entity_pnts;
	long n_polygons;
	long n_nodes;
	long n_attr_nodes;
	long n_attr_objs;
};

struct Sdts_catd{
	int index;
	char mod_name[10];
	char type[40];
	char file_name[20];
	int mod_nrec;
};

enum attr_type 
{ OTHER, INTEGER, REAL, CHARS, BINARY };

#ifdef MAIN

char *sql_type[6] = {
  "UNKNOWN",
  "integer",
  "float",
  "char",
  "char"
};

#else

char *sql_type [6];

#endif

enum attr_key
{ NOKEY = 0, PKEY, FKEY, PFKEY };

struct attr_field 
{
	char field[20];
	enum attr_type type;
	int  size;
	enum attr_key key_flag;
};

struct db_table 
{
	char mod_name[10];
	char *db_ready_filename;
	int type;
	int field_cnt;
	int allocated;
	struct attr_field *fields;
};

struct Sdts_info{
    int n_files; /*number of files in entire transfer=recs in CATD*/
	struct Sdts_catd *S_catd;
	int n_manifolds;
	struct Sdts_manifold *S_mfold;
};

struct Sdts_globals{
    char Iden_titl[100];
	char Iden_mpdt[20];
	char Iden_dcdt[20];
	int Iden_scal;
	char Iref_xlbl[10];
	char Iref_ylbl[10];
	double Iref_sfax, Iref_sfay;
	double Iref_xorg, Iref_yorg;
	double Iref_xhrs, Iref_yhrs;
	char Xref_rsnm[5];
	int Xref_zone;
};

struct Bounds{
  int min_x, min_y, max_x, max_y;
};

struct Sdts_LE{
	int rcid;
	int att_num;
	int pidl;
	int pidr;
	char pidl_modn[5];
	char pidr_modn[5];
	double *X;
	double *Y;
};

struct Sdts_NO{
	int rcid;
	int att_num;
    double x;
	double y;
};

struct Sdts_NE{
	int rcid;
	int att_num;
    double x;
	double y;
};

struct Sdts_NA{
	int rcid;
	int att_num;
	double x, y;
	int arid;
};

struct Sdts_PC{
	int atid_rcid;
	char atid_modn [30];
};

extern struct Sdts_PC *polygon_modns; 


struct att_ff_file
{
  char *fname;
  int processed;
};

struct att_ff_info
{
   int n_ap_files;
   int n_as_files;
   int n_ff_files;
   struct att_ff_file *AP, *AS, *FF;
};

struct obj_attr_list {
   int serial;
   int manifold; /*id number for manifold*/
   int fid; /*feature id for the manifold , unique in manifold but not in file*/
   char obj_code[20];
   char attr_code[20];
};

struct ff_attr_list {
   int serial;
   char ff_code[20];
   char attr_code[20];
};

struct ff_elem_list {
   int serial;
   char ff_code[20];
   char elem_code[20];
};

struct attr_pnt_list {
   int manifold; /*primary key = manifold + fid*/
   int fid;  
   char obj_code[20];
   int obj_type; /*LINE, AREA, DOT, ...*/
   double att_x, att_y;
};

struct area_pnt_list {
   char NA_code[20];
   char PC_code[20];
   double att_x, att_y;
};

#define MAX_ATTR_MODS   200

/*structure for all attribute module names that are pointed to by objects*/
/*use to determine many to many relations, etc*/

struct obj_attr_modn_list {
   int cnt;
   char *modn_list[MAX_ATTR_MODS];
};

