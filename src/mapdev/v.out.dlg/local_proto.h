/* area_one.c */
int build_area_one(struct Map_info *);
int fill_univ_info(struct Map_info *, double, double, double, double);
int get_univ_bbox(struct Map_info *, double *, double *, double *, double *);
/* export_dlg.c */
int main(int, char *[]);
/* shuffle_dots.c */
int shuffle_dots(struct Map_info *);
/* struct_swap.c */
int line_swap(struct Map_info *, int, int);
/* v.export_dlg.c */
int main(int, char **);
int debugf(char *, ...);
int export(char *, char *, char *);
/* write.c */
int start_ints(void);
int write_ints(FILE *, int, plus_t *);
int end_ints(FILE *);
int start_att(void);
int write_dlg_att(FILE *, int, int);
int end_att(FILE *);
int start_coords(void);
int write_coords(FILE *, int, double *, double *);
int end_coords(FILE *);
/* write_areas.c */
int write_dlg_areas ( struct Map_info *, FILE *);
/* write_head.c */
int write_dlg_head(struct Map_info *, struct dig_head *, FILE *);
int line1(struct Map_info *, struct dig_head *, FILE *);
int line2(struct Map_info *, struct dig_head *, FILE *);
int line3(struct Map_info *, struct dig_head *, FILE *);
int line4(struct Map_info *, struct dig_head *, FILE *);
int line5_9(struct Map_info *, struct dig_head *, FILE *);
int line10(struct Map_info *, struct dig_head *, FILE *);
int line11_14(struct Map_info *, struct dig_head *, FILE *);
int line15(struct Map_info *, struct dig_head *, FILE *);
char *dtype(double, int, int);
/* write_lines.c */
int write_dlg_lines ( struct Map_info *, FILE *);
/* write_nodes.c */
int write_dlg_nodes ( struct Map_info *, FILE *);

