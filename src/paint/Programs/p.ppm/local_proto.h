/* color.c */
int build_color_tables(void);
int build_dither_tables(void);
int printer_color_number(register int, register int, register int);
int red_dither(unsigned char *, int, int, int);
int grn_dither(unsigned char *, int, int, int);
int blu_dither(unsigned char *, int, int, int);
/* header.c */
int header(int *, int *, int *);
/* nextword.c */
int nextword(char *);
int eol(void);
/* paint.c */
int begin_paint(int, int);
int paint(unsigned char *, unsigned char *, unsigned char *, int, int);
/* readcolors.c */
int readcolors(register unsigned char *, register unsigned char *, register unsigned char *, register int, int);
/* xalloc.c */
char *xalloc(int);
