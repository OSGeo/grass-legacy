/* interval.c */
int label_interval(struct Map_info *, double);
/* label_lines.c */
int label_lines(struct Map_info *, int, int);
int get_next_line(struct Map_info *, int);
int label_line(struct Map_info *, int, int);
/* line_center.c */
int get_line_center(double *, double *, struct line_pnts *);
/* v.cadlabel.c */
int debugf(char *, ...);
int setup(char *, char *, char *, char *, double);
int attach_labels(struct Map_info *, struct Map_info *);
