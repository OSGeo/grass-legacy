int asc_to_bin(FILE *, struct Map_info *);
int bin_to_asc(FILE *,FILE *);
/* head_info.c */
int get_head_info(struct dig_head *);
/* new_screen.c */
int new_screen(void);
/* rw_ascii.c */
int write_head_ascii(FILE *);
int read_head_ascii(FILE *);
