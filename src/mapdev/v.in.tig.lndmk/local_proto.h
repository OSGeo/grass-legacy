/* areas.c */
int cmp_polyid(struct landrec *, struct landrec *);
int cmp_int(int *, int *);
int do_areas(void);
int area_yes(char *);
int track(int);
/* cats_stuff.c */
int reset_cats(void);
int ask_cfcc_list(void);
int sub_actual_cat(void);
int show_list(char *);
int decode(char *);
char *skip_to_CC(char *);
/* cmd_parse.c */
int process(FILE *);
/* coords.c */
int get_point_coords(char *, double *, double *);
int ll_from_str(char *, double *, double *);
int make_utms(double *, double *, int *, int);
/* info_type.c */
int ask_info_type(void);
/* lines.c */
int do_lines(void);
/* main.c */
int report(int, int, int);
/* match_text.c */
int vask_matches(void);
int report_matches(char *);
int is_a_match(char *, char *);
int reset_m_strings(void);
int get_m_strings(char *);
int both_conds(void);
int good_one(int, char *, int);
/* parse_input.c */
int parse_input(int, char *[]);
/* points.c */
int do_points(void);
int point_yes(char *);
int open_site_file(void);
int save_site(char *);
int close_site_file(int);
/* tig_control.c */
int tig_open(int);
int make_open_file(int);
int tig_names(void);
int tig_close(int);
int tig_rewind(int);
/* tig_record.c */
int get_tiger_record(FILE *, int, char *);
