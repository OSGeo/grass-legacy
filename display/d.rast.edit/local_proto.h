/* cell.c */
int Dcell(char *, char *, int);
/* draw_grid.c */
int draw_grid(void);
/* edit.c */
int edit(void);
int edit_mouse_info(void);
int edit_mouse_info2(DCELL, DCELL);
int use_mouse(void);
/* main.c */
int main(int, char **);
int do_edit(int, int, double);
int error(int, char [128]);
int ext(void);
/* menu.c */
int main_menu(void);
int option_menu(void);
int color_menu(char *);
int map_type_menu(void);
int arrow_options(void);
int get_arrow_inputs(void);
int arrow_map(void);
/* mk_new_layer.c */
int make_new_cell_layer(void);
/* mk_tmp_file.c */
int make_temporary_file(void);
