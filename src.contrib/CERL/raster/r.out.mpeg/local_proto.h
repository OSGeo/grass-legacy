/* main.c */
int load_files(void);
int use_r_out(void);
char **gee_wildfiles(char *, char *, int *);
/* write.c */
int write_ycc(char *, char *, char *, int, int, int *, int *, char *);
int write_ppm(char *, char *, char *, int, int, int *, int *, char *);
int write_params(char *, char *[], char *, int, int, int, int, int);
int clean_files(char *, char *[], int);
