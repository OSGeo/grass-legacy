/* do_areas.c */
int do_area(struct Map_info *, struct Categories *, int);
int codes(int);
/* do_lines.c */
int do_line(struct Map_info *, struct Categories *, int);
/* edit_head.c */
int do_file_checks(struct Map_info *);
int get_head_info(int);
/* head_info.c */
int get_head_info(struct dig_head *);
/* isle_area.c */
int isle_area(struct Map_info *, int, double *);
/* perimeter.c */
double perimeter(register int, register double *, register double *);
