
/* global definitions for v.in.landmark */


/* MAXNCFCC is about 10 bigger than expected value of NCFCC */
#define MAXNCFCC 200
#define NMATCHES 10

#ifdef MAIN

char *sphere;
int proj,zone;
int interactive;
int both_conds_req;

struct tigerfile {
  char type;
  FILE *fp;
  char *file; /* tiger file name given on command line */
  char *name; /* tiger file name computed and used in pgm */
};
/* N_TIGERS also defined in tig_record.c */
#define N_TIGERS 12
struct tigerfile tiger[N_TIGERS+1];
FILE *sitefile;
int vectorfile;
char *mapset;
char vect_name[50];
char site_name[50];
char t1buf[250];
struct GModule *module;
struct Flag *flag1;
struct Flag *flag2;
struct Option *opt1; /* t1    */
struct Option *opt2; /* t2    */
struct Option *opt3; /* t3    */
struct Option *opt7; /* t7    */
struct Option *opt8; /* t8    */
struct Option *optA; /* tA    */
struct Option *optI; /* tI    */
struct Option *optP; /* tP    */
struct Option *optin; /* input */
struct Option *optv;  /* vect  */
struct Option *opts;  /* site  */
struct Option *optz ; /* UTM zone */
struct Option *optsph;/* UTM spheroid */

/* NCFCC = total number of CFCC categories */
int NCFCC;
char want[MAXNCFCC][4];    /* array to request cats */
char cat[MAXNCFCC][4];    /* array to signal cats */
         /* initialize too large, real size about 190 */
char feature_type[4][3]; /* array to mark Point, Area, Line requests */
char match_string[NMATCHES][32];


#else

extern char *sphere;
extern int proj, zone;
extern int interactive;
extern int both_conds_req;

struct tigerfile {
  char type;
  FILE *fp;
  char *file;
  char *name;
};
#define N_TIGERS 12
extern struct tigerfile tiger[N_TIGERS+1];
extern FILE  *sitefile;
extern int vectorfile;
extern char *mapset;
extern char vect_name[50];
extern char site_name[50];
extern char t1buf[250];
extern struct GModule *module;
extern struct Flag *flag1;
extern struct Flag *flag2;
extern struct Option *opt1; /* t1    */
extern struct Option *opt2; /* t2    */
extern struct Option *opt3; /* t3    */
extern struct Option *opt7; /* t7    */
extern struct Option *opt8; /* t8    */
extern struct Option *optA; /* tA    */
extern struct Option *optI; /* tI    */
extern struct Option *optP; /* tP    */
extern struct Option *optin; /* input */
extern struct Option *optv;  /* vect  */
extern struct Option *opts;  /* site  */
extern struct Option *optz ; /* UTM zone */
extern struct Option *optsph;/* UTM spheroid */

/* NCFCC = total number of CFCC categories */
extern int NCFCC;
extern char want[MAXNCFCC][4];    /* array to request cats */
extern char cat[MAXNCFCC][4];    /* array to signal cats */
extern char feature_type[4][3];   /* array to mark Point, Area, Line requests */
extern char match_string[NMATCHES][32]; /* for matching strings */

#endif

/* areas.c */
int cmp_int(const void *, const void *);
int do_areas(void);
int area_yes(char *);
int track(int);
/* cats_stuff.c */
int reset_cats(void);
int ask_cfcc_list(void);
int sub_actual_cat(void);
int show_list(char *);
int decode(char *);
char *skip_to_CC(char *);
/* cmd_parse.c */
int process(FILE *);
/* coords.c */
int get_point_coords(char *, double *, double *);
int ll_from_str(char *, double *, double *);
int make_utms(double *, double *, int *, int);
/* info_type.c */
int ask_info_type(void);
/* lines.c */
int do_lines(void);
/* main.c */
int report(int, int, int);
/* match_text.c */
int vask_matches(void);
int report_matches(char *);
int is_a_match(char *, char *);
int reset_m_strings(void);
int get_m_strings(char *);
int both_conds(void);
int good_one(int, char *, int);
/* parse_input.c */
int parse_input(int, char *[]);
/* points.c */
int do_points(void);
int point_yes(char *);
int open_site_file(void);
int save_site(char *);
int close_site_file(int);
/* tig_control.c */
int tig_open(int);
int make_open_file(int);
int tig_names(void);
int tig_close(int);
int tig_rewind(int);
/* tig_record.c */
int get_tiger_record(FILE *, int, char *);
