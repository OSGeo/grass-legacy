int Full, Area, Line, Isle, Attr, Node, Edit;
int aArea, aLine, aIsle, aAttr, aNode;
int got_cats;

/* do_areas.c */
int do_area(struct Map_info *, struct Categories *, int);
int codes(int);
/* do_lines.c */
int do_line(struct Map_info *, struct Categories *, int);
/* head_info.c */
int get_head_info(struct dig_head *);
/* isle_area.c */
int isle_area(struct Map_info *, int, double *);
/* perimeter.c */
double perimeter(register int, register double *, register double *);
