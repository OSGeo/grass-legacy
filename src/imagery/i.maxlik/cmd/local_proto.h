/* classify.c */
int classify(CELL *, CELL *, int);
/* dump_sigs.c */
int main(int, char *[]);
int test_signatures(struct Signature *);
int print_sig(struct One_Sig *, int);
int usage(char *);
/* hist.c */
int make_history(char *, char *, char *, char *);
/* invert.c */
int invert_signatures(void);
int invert(struct One_Sig *, int, int *, int *, double *);
/* main.c */
int main(int, char *[]);
/* open.c */
int open_files(void);
