/* error.c */
int error(char *, int);
/* extract.c */
int extract(void);
/* getargs.c */
int getargs(int, char *[]);
/* io.c */
int seek_out(long);
int write_out(char *, int);
/* printgeo.c */
int printgeo(FILE *, long, char *);
/* readtape.c */
int readtape(void);
/* round.c */
int round_up(long *, int);
int round_down(long *, int);
/* scan_geo.c */
int scan_geo(char *, char *, int *);
/* uhl.c */
int uhl(int);
/* usage.c */
int usage(void);
/* write_hd.c */
int write_hd(void);
/* zero_out.c */
int zero_out(long);
