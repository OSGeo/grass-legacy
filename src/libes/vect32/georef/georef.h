/* ask_driver.c */
int ask_yes_no(char *);
int ask_driver_yes_no(char *);
int ask_driver(void);
int get_digitizer_button_xy(double *, double *);
int ask_driver_raw(double *, double *);
int coll_a_pnt(double *, double *);
int _coll_a_pnt(double *, double *);
/* curses.c */
int Init_curses(void);
int Close_curses(void);
int _Write_base(int, char *);
int _Base_string(int, int, char *);
int Base_string(int, int, char *);
int Write_base(int, char *);
int _Write_info(int, char *);
int Write_info(int, char *);
int Clear_base(void);
int _Clear_base(void);
int Clear_info(void);
int _Clear_info(void);
int Replot_screen(void);
int Get_curses_char(char *);
int Get_curses_text(char []);
int _Base_refresh(void);
int curses_yes_no(int, char *);
int mysuspend(void);
int myrespend(void);
int _Info_refresh(void);
int _Curses_on(void);
int _Curses_off(void);
int Curses_state(void);
void close_down(int);
/* flush_input.c */
int flush_keyboard(void);
/* geo_file.c */
int geo_read_control(FILE *, char *, char *, char *, int *);
int geo_save_control(FILE *, char *, char *, char *, int);
/* geo_get_pt.c */
int geo_get_point(double *, double *);
int get_point_explain(void);
int get_point_buttons(double *, double *);
int get_point_generic(double *, double *);
/* geo_point.c */
int geo_point_main(char *, char *);
/* geo_reg.c */
int geo_reg_main(char *, char *, char *, char *, char *);
/* geo_reset.c */
int geo_reset_transform(void);
/* leave.c */
int leave(void);
/* map_ask.c */
int ask_map_coor(int);
int shrink_map_coor(void);
/* map_coor.c */
int load_coor_from_file(FILE *);
int save_coor_to_file(FILE *);
/* map_reg.c */
int register_map_coor(int);
int show_reg_menu(void);
int get_reg_response(double *, double *);
/* map_resid.c */
int show_residual_results(int, int);
int show_coor_only(int, int, int);
/* map_scale.c */
int calculate_map_scale(void);
double get_map_scale(void);
/* map_setup.c */
int setup_map_reg(void);
/* pr_instruct.c */
int pr_instructions(void);
/* reset_map.c */
int reset_map(void);
int check_map_explain(void);
int check_map_buttons(void);
int check_map_generic(void);
int check_map_ft_swtch(void);
/* set_key.c */
int set_keyboard(void);
int unset_keyboard(void);
int set_keyboard(void);
int unset_keyboard(void);
int key_hit(char *);
/* set_prior.c */
int set_priority_3b2(void);
int init_priority(void);
int set_priority(void);
int unset_priority(void);
int set_uid_to_user(void);
/* tty.c */
int Get_old_tty(void);
int Get_new_tty(void);
int Old_tty(void);
int New_tty(void);
