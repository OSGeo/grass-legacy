/* display_line.c */
int display_line(int, struct line_pnts *, int, struct Map_info *);
int erase_line(int, struct line_pnts *, int, struct Map_info *);
int highlight_line(int, struct line_pnts *, int, struct Map_info *);
int color_line(int, struct line_pnts *, int, struct Map_info *, int);
int _display_line(int, struct line_pnts *, int, struct Map_info *);
int _erase_line(int, struct line_pnts *, int, struct Map_info *);
int _highlight_line(int, struct line_pnts *, int, struct Map_info *);
int _color_line(int, struct line_pnts *, int, struct Map_info *, int);
int _display_line_label(struct Map_info *, int, int);
