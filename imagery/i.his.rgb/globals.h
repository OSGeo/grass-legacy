#define NBANDS 3

#ifdef MAIN 

int              Rgb2His;           /* flag for rgb-his/his-rgb conversion   */
int              fd_input[NBANDS];  /* the input files                     */
int              fd_output[NBANDS]; /* the ouput files                     */

#else

extern int       Rgb2His;          
extern int       fd_input[NBANDS]; 
extern int       fd_output[NBANDS];

#endif



#ifdef MAIN
char inputfiles[NBANDS][255], outputfiles[NBANDS][255];

#else
extern char inputfiles[NBANDS][255], outputfiles[NBANDS][255];
#endif

/* closefiles.c */
int closefiles(CELL *[NBANDS]);
/* command.c */
int save_args(int, char *);
int set_default_args(void);
/* getinput.c */
void getinput(void);
/* his2rgb.c */
void his2rgb(CELL *[NBANDS], int);
/* openfiles.c */
void openfiles(CELL *[3]);
/* putoutput.c */
int putoutput(void);
/* rgb2his.c */
void rgb2his(CELL *[NBANDS], int);
