#ifdef NEED_DIGIT_EXTRAS
/* coll_pts.c */
int collect_points(int mode, int type, int *n_points, double **xarr, double **yarr);
/* dig_curses.c */
int Init_curses(void);
int Close_curses(void);
int _Write_base_win(void);
int Write_base_win(void);
int Write_edit_win(void);
int _Write_generic_win(struct Menu_head *menu);
int Write_generic_win(struct Menu_head *menu);
int _write_generic_title(char *name);
int Dig_menu_opts(void);
int _Write_mouse_covr(void);
int _Write_dig_win(void);
int Write_dig_win(void);
int _Write_header_info(void);
int Write_header_info(void);
int _Write_type_info(void);
int Write_type_info(void);
int _Write_base(int line, char *message);
int _Base_string(int y, int x, char *message);
int Base_string(int y, int x, char *message);
int Write_base(int line, char *message);
int _Write_info(int line, char *message);
int Write_info(int line, char *message);
int Clear_help(void);
int _Clear_help(void);
int Clear_base(void);
int _Clear_base(void);
int Clear_info(void);
int _Clear_info(void);
int Replot_screen(void);
int Get_curses_char(char *answer);
int Get_curses_text(char answer[]);
int _show_mode(int mode, int type, int label);
int show_mode(int mode, int type, int label);
int _Base_refresh(void);
int curses_yes_no(int n, char *s);
int curses_yes_no_default(int n, char *str, int def);
int suspend(void);
int respend(void);
int vask_suspend(void);
int vask_respend(void);
int get_type_cnt(int type);
int _Write_help(int line, char *message);
int _Help_string(int y, int x, char *message);
int Help_string(int y, int x, char *message);
int Write_help(int line, char *message);
int Show_help(void);
int Hide_help(void);
int _Info_refresh(void);
int _Help_refresh(void);
int help_get_key(void);
int _Curses_on(void);
int _Curses_off(void);
int Curses_state(void);
int curses_getchar(void);
int _Write_covr(int line, char *message);
int _Covr_string(int y, int x, char *message);
int Covr_string(int y, int x, char *message);
int Write_covr(int line, char *message);
int Clear_covr(void);
int _Clear_covr(void);
int _Covr_refresh(void);
int _Show_version(void);
int Curses_error(char *str);
#endif
/* dig_dev.c */
int D_digit_init(void);
int D_readall(int *Xraw, int *Yraw);
int D_ask_if_err(void);
int D_get_scale(float *scale);
int D_end_digit(void);
int D__read(int alarm_time);
/* int jtimeout(void);
*/
int D_open_failed(void);
int D_write_digit(char *string);
int D_flush(void);
int D_open_serial(char *tty_name);
int D_open_serial(char *tty_name);
/* interface.c */
int D_setup_origin(void);
int D_cursor_buttons(void);
int D_start_button(void);
int D_foot_switch(void);
int D_clear_driver(void);
int D_ask_driver_raw(double *x, double *y);
int D_read_raw(int *Xraw, int *Yraw);
/* int delay(int n); */
/* setup_driver.c */
int D_setup_driver(char *dev);
