/* @(#)gis.h	2.2   7/8/87 */
#define NEWLINE     '\n'
#define MAXEDLINES  25
#define RECORD_LEN  80
#define MAX_CATS    256

#define malloc G_malloc
#define calloc G_calloc
#define realloc G_realloc

typedef unsigned char CELL;
typedef int SUPERCELL;

struct Cell_head
{
    char format      ;  /* 0 = uncompressed   1 = compressed            */
    int rows, cols   ;  /* number of rows and columns in the data       */
    int proj         ;  /* Projection: 1 = UTM  2 = State Plane         */
    int zone         ;  /* UTM or State-Plane zone                      */
    double ew_res    ;  /* East to West cell size                       */
    double ns_res    ;  /* North to South cell size                     */
    double north     ;  /* utm   coordinates of layer                   */
    double south     ;
    double east      ;
    double west      ;
} ;

struct Categories
{
    int num               ;   /* total number of categories              */
    char title[80]        ;   /* name of data layer                      */ 
    char *name[MAX_CATS]  ;   /* category names                          */
} ;

struct Reclass
{
    char name[20]           ; /* name of cell file being reclassed    */
    char mapset[128]        ; /* mapset in which "name" is found      */
    int num                 ; /* total number of categories           */
    int category[MAX_CATS]  ; /* room for MAX_CAT categories          */
} ;

struct Statistics
{
    int num                ; /* total number of categories           */
    int category[MAX_CATS] ; /* room for MAX_CAT area statistics     */
} ;

struct Colors
{
    int num              ; /* total number of categories             */
    float red[MAX_CATS]      ; /* room red   levels                  */
    float grn[MAX_CATS]      ; /* room green levels                  */
    float blu[MAX_CATS]      ; /* room blue  levels                  */
} ;

struct History
{
    char    mapid[RECORD_LEN];
    char    title[RECORD_LEN];
    char    mapset[RECORD_LEN];
    char    creator[RECORD_LEN];
    char    maptype[RECORD_LEN];
    char    datsrc_1[RECORD_LEN];
    char    datsrc_2[RECORD_LEN];
    char    keywrd[RECORD_LEN];
    int     edlinecnt;
    char    edhist[MAXEDLINES][RECORD_LEN];
} ;

#ifndef FILE
#include <stdio.h>
#endif


FILE *G_fopen_new() ;
FILE *G_fopen_old() ;
FILE *G_fopen_sites_old();
FILE *G_fopen_sites_new();
FILE *G_fopen_vector_old();
FILE *G_fopen_vector_new();

CELL *G_allocate_cell_buf() ;
SUPERCELL *G_allocate_supercell_buf() ;

char *G__file_name () ;
char *G__getenv () ;
char *G__home () ;
char *G__mapset_name () ;
char *G__location_path () ;
char *G_ask_any () ;
char *G_ask_any_ext () ;
char *G_ask_cell_in_mapset () ;
char *G_ask_cell_new () ;
char *G_ask_cell_old () ;
char *G_ask_in_mapset () ;
char *G_ask_in_mapset_ext () ;
char *G_ask_new () ;
char *G_ask_new_ext () ;
char *G_ask_old () ;
char *G_ask_old_ext () ;
char *G_ask_sites_any () ;
char *G_ask_sites_in_mapset () ;
char *G_ask_sites_new () ;
char *G_ask_sites_old () ;
char *G_ask_vector_any () ;
char *G_ask_vector_in_mapset () ;
char *G_ask_vector_new () ;
char *G_ask_vector_old () ;
char *G_calloc() ;
char *G_color_name() ;
char *G_date() ;
char *G_find_cell () ;
char *G_find_cell2 () ;
char *G_find_file () ;
char *G_find_file2 () ;
char *G_getenv () ;
char *G_gisbase() ;
char *G_gisdbase() ;
char *G_home () ;
char *G_location() ;
char *G_location_path() ;
char *G_malloc() ;
char *G_mapset() ;
char *G_mask_info() ;
char *G_myname() ;
char *G_program_name() ;
char *G_realloc() ;
char *G_store () ;
char *G_tempfile () ;
char *G_whoami() ;
