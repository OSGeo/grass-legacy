#define SITE_ATTR_CAT 1
#define SITE_ATTR_STR 2
#define SITE_ATTR_DBL 3
#define SITE_ATTR_COORD 4
#define SITE_ATTR_DIM 5

/* do_labels.c */
int initialize_options(void);
int do_labels(FILE *, struct Cell_head,  char *, char *, char *, char *, char *,
 char *, int, int,  int, int, char *);
int show_it(int);
int scan_ref(char *);
char* G_strchg(char*, char, char);
