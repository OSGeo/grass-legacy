/* bars.c */
int allocate_color_bars(void);
int fill_color_bar(char);
int display_color_bar(View *);
/* colors.c */
int display_color_assignment(void);
int load_colors(void);
int change_color_assignment(int, int);
/* driver.c */
int driver(void);
/* graphics.c */
int Init_graphics(void);
int Solid_box(int, int, int, int);
int Outline_box(int, int, int, int);
int Text_width(char *);
int Text(char *, int, int, int, int, int, int);
int Uparrow(int, int, int, int);
int Downarrow(int, int, int, int);
int Erase_view(View *);
/* image.c */
int draw_image(View *, int);
int draw_red_band(void);
int draw_grn_band(void);
int draw_blu_band(void);
/* info.c */
int display_red_color_info(void);
int display_grn_color_info(void);
int display_blu_color_info(void);
/* input.c */
int Input_pointer(Objects *);
int Input_box(Objects *, int, int);
int Input_keyboard(int (*)());
int Menu_msg(char *);
int Start_mouse_in_menu(void);
/* main.c */
int Beep(void);
/* mouse.c */
int Mouse_pointer(int *, int *, int *);
int Mouse_box_anchored(int, int, int *, int *, int *);
int Get_mouse_xy(int *, int *);
int Set_mouse_xy(int, int);
/* title.c */
int display_title(void);
