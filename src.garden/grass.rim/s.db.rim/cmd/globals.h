
#define TRUE 1
#define FALSE 0

#define FIELD_NAME_LENGTH 16
#define MAX_FIELDS 70
#define MAX_BUFFER (FIELD_NAME_LENGTH+1)*MAX_FIELDS
#define MAX_NEST_DEPTH 8
#define INP_SIZ 101

#define FIELD_TABLE 1
#define SCREEN_TABLE 2
#define DATA_TABLE 3

#define PROMPT "s.db"

/* This routine allows isatty() to be called on file pointers
   instead of file descriptors by finding the UNIX file descriptor
   that is buried inside a file pointer and using that.
   */
#define FP_ISATTY(file_ptr)   isatty(fileno(file_ptr))

#define SLEEP3 system("sleep 3")

struct field_info {
   char column_name[FIELD_NAME_LENGTH+1];
   int rec_offset;
   char column_type;
   int line_num;
   int column_num;
   int length;
   int next_field[2];  /*next field number in case of split field*/
   char *value; /* a pointer to the current value of the field */
} ;

struct query_site {
        int site_number;
        double east;
        double north;
} ;
#define SIZEOF_QUERY_SITE sizeof(struct query_site)

#ifdef TOPLEVEL
   char File_name[10], *Tableinfof = NULL, RIM_db_path[100];
   char *Tempdumpf;
#ifndef DBSITES
   FILE *Outfile, *Infile, *Input_files[MAX_NEST_DEPTH];
   int Input_nest_depth = 0;
#endif
   struct field_info Field_info[MAX_FIELDS];
   struct query_site *Site_list = NULL, *Last_site = NULL;
   struct Cell_head Active_wind;
   char Prompt[16], RIM_db_mapset[40]="";
   int Site_field, North_field, East_field;
   int Number_of_sites = 0;
   int Projection;
#else
   extern char File_name[10], *Tableinfof, RIM_db_path[100];
   extern char *Tempdumpf;
#ifndef DBSITES
   extern FILE *Outfile, *Infile, *Input_files[MAX_NEST_DEPTH];
   extern int Input_nest_depth;
#endif
   extern struct field_info Field_info[MAX_FIELDS];
   extern struct query_site *Site_list, *Last_site;
   extern struct Cell_head Active_wind;
   extern char Prompt[16], RIM_db_mapset[40];
   extern int Site_field, North_field, East_field;
   extern int Number_of_sites;
   extern int Projection;
#endif


#ifdef DBSITES
#define Outfile stdout
#define Infile stdin
#endif


