/* describe.c */
int set_quartiles(int);
int set_quiet(int);
int load_cached_sites(FILE *, int);
int load_sites_stats(FILE *, int);
int site_mem(Site *);
int compress_cached_site(Site *);
int addto_stats(Site *, int);
void sites_describe(void);
int free_cached_sites(void);
int do_new_sites(char *, char *, int);
