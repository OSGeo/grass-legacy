/* font.c */
int init_font(char *);
int get_char_vects(int, int *, unsigned char **, unsigned char **);
int copy(char *, char *, int);
/* font_2_bin.c */
int scanint(int);
int newchar(int, long);
/* fontmap.c */
int fontmap(char *, int []);
/* showchar.c */
int showchar(int);
int draw(int, int);
int line_eq(int, int, int, int, int, int);
int draw_line(int, int, int, int);
int graph_line(register int, register int, int, int);
int graph_point(int, int);
/* splitfont.c */
int savechar(int, int);
