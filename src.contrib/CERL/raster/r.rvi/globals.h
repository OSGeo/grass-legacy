#ifndef MAIN
#define GLOBAL extern
#else
#define GLOBAL
#endif
#define NBANDS 3

GLOBAL int *fd_input;    /* the input files */
GLOBAL int fd_output;    /* the ouput file */
GLOBAL char *modelfile;
GLOBAL char *outputfile;
GLOBAL char **inputfiles;
