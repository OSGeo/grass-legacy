/* Col.c */
int CountColumns(char *);
int GetColumn(char *, int, char *);
/* CountCats.c */
int CountCats(FILE *, int);
/* CountLines.c */
int CountLines(FILE *);
/* DO_txt_file.c */
int DO_txt_file(char *, char *);
/* FindAtt_Cat.c */
int FindAttAndCat(FILE *, int, int, int, int, char *, int *);
/* FindIDColumn.c */
int FindIDColumn(FILE *);
#ifdef GRASS_VECT_H
/* GenToDig.c */
int GenToDigArea(FILE *, struct Map_info *, int, char *);
int GenToDigLine(FILE *, struct Map_info *, int, char *);
int GenToDig(int, FILE *, struct Map_info *, int, char *);
#endif
/* process_inp.c */
int process_inp(char *);
