/* ask.c */
int yes(char *);
int just_click(char *);
int ask(char *[]);
int ask_rotate(void);
/* box.c */
int make_window_box(struct Cell_head *, double);
/* center.c */
int make_window_center(struct Cell_head *, double);
/* returns.c */
int get_wind_bot(void);
int get_wind_top(void);
int get_wind_rite(void);
int get_wind_left(void);
int get_map_bot(void);
int get_map_top(void);
int get_map_left(void);
int get_map_rite(void);
int get_wind_y_pos(float);
int get_wind_x_pos(float);
/* zoom.c */
int zoomwindow(int, int, double);
