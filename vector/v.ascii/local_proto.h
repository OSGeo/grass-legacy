/* asc_to_bin.c */
int asc_to_bin(FILE *, struct Map_info *, int pnt);
/* bin_to_asc.c */
int bin_to_asc(FILE *, FILE *, struct Map_info *, int ver, int pnt);

int write_head ( FILE * dascii, struct Map_info *Map);
int read_head ( FILE * dascii, struct Map_info *Map );

int debugf(char *, ...);
int cp_file(char *, char *);
