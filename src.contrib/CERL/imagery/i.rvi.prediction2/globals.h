#define NBANDS 3

#ifdef MAIN 

int              fd_input[NBANDS];  /* the input files                     */
int              fd_output; /* the ouput file                     */
int		 fd_modelfile; /* the model input file			*/

#else

extern int       fd_input[NBANDS]; 
extern int       fd_output;
extern int       fd_modelfile;

#endif



#ifdef MAIN

char inputfiles[NBANDS][255], outputfiles[255], modelfile[255]; 

#else
extern char inputfiles[NBANDS][255], outputfiles[255], modelfile[255];
#endif

/* closefiles.c */
int closefiles(CELL *[NBANDS]);
/* model.c */
void model(double [7], CELL *[NBANDS], int);
