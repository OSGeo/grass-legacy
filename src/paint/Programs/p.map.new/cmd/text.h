#define XSIZE 25
#define YSIZE 25

#define LEFT 0
#define RIGHT 1
#define LOWER 0
#define UPPER 1
#define CENTER 2


#define BOX struct _box_
BOX
{
    int top;
    int bottom;
    int left;
    int right;
};
int text_bounds(char *, int, int, BOX *, int);
/* draw_text.c */
int draw_text(char *, int, int, int);
int text_line(int, int, int, int, int);
int set_text_background(int);
int set_text_border(int);
int set_text_color(int);
int set_text_rotation(int);
int set_text_size(double);
int set_text_width(int);
int set_text_xref(int);
int set_text_yref(int);
int set_text_scale(double);
int set_text_hwidth(int);
int set_text_hcolor(int);
int set_reasonable_text_size(void);
/* graph_text.c */
int graph_char(int *, int *, double, double, int, int);
/* int graph_text(int *, int *, double, double, register char *, int); */
/* font.c */
char *fontfilename(char *, char *);
int check_font(char *);
int select_font(char *);
int select_standard_font(void);
int list_fonts(void);
int get_font_char(int, int *, char **, char **);
