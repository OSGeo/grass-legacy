#ifndef PATTERN
#define PATTERN struct _pattern_
PATTERN
{
    char **pat;
    short nrows;
    short ncols;
    short xcenter;
    short ycenter;
    short colors[10];
} ;
#define GLOBAL_PATTERNS 1
#define USER_PATTERNS 2
#define TEMP_PATTERNS 3
#endif
/* alloc_pats.c */
int allocate_pattern_array(void);
/* builtin.c */
int builtin_patterns(void);
/* pattern.c */
int add_pattern_to_list(char *, long, int, int, int, int);
int find_pattern(char *, long *, int *, int *, int *, int *);
int read_pattern(PATTERN *, int);
int get_pattern(char *, PATTERN *);
int begin_pattern(char *);
int store_pattern(char *);
int end_pattern(void);
int any_patterns(void);
int next_pattern(PATTERN *, int);
int open_pattern_file(int, int);
int close_pattern_file(void);
int unlink_pattern_file(void);
int print_pattern(PATTERN *);
/* predef_pat.c */
int read_predefined_patterns(void);
/* patcolor.c */
int scan_patcolor(char *, int *, int *);
/* set_pat.c */
int set_pattern (int , char *);
int set_all_patterns (void);
/* input_pat.c */
int input_pattern(char *);
