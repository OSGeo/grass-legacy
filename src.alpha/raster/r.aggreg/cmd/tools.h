/* Macro for error handling */
#define TERMINATE(a) { fprintf(stderr,a); exit(-1); }

typedef struct filenames {
			char *input;
			char *output;
			char *neibordat;
			short neighborhood;
			} Filenames;

extern Filenames init_and_parse(int,char**);
