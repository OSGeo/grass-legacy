/* asc_to_bin.c */
int asc_to_bin(FILE *, struct Map_info *, int pnt);

int write_head ( FILE * dascii, struct Map_info *Map);
int read_head ( FILE * dascii, struct Map_info *Map );

int debugf(char *, ...);
int cp_file(char *, char *);
