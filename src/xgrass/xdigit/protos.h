/* add_scale.c */
int add_scale();

/* arastcolr.c */
void XgdAllocRastColors();
Status xAllocColor();
/* area.c */
int build_area();
int Del_area();

/* ask_cat.c */
int be_done();
int set_autolabel();
int make_ask_cat_popup();
int ask_cat();

/* ask_driver.c */
int yes_no_hit();
Widget make_yn_popup();
int ask_driver_yes_no();
int ask_driver();
int get_digitizer_button_xy();
int ask_driver_raw();
int ask_all();
int coll_a_pnt();
int _coll_a_pnt();

/* backdrop.c */
void show_backdrop();
void no_backdrop();
void ask_backdrop();
int display_backdrop();

/* bad_areas.c */
int unfinished_areas();

/* bin_digit.c */
int main();

/* break.c */
int break_line();
int break_line_w_point();

/* build_neat.c */
int build_neat();

/* check_map.c */
int button_hit();
int check_map();
Widget make_explain();
int update_coords();
int check_map_buttons();
int make_check_popup();

/* coll_pts.c */
int Collect_points();

/* compress.c */
int write_out();
int compress();

/* contour.c */
int label_contour();
int cmp_x();
int cmp_y();
int line_in_bbox();

/* coor_pop.c */
int action_taken();
int make_coor_popup();
int set_coors();
int get_coors();
int show_coor_ask();

/* custommenu.c */
void make_vbrowse();
void make_rbrowse();
int make_custom_menu();
void digdevcb();
void threshcb();
void textcb();
void windevcb();
void pntdevcb();
void downcb();
void showcb();
void OkCallback();
char *get_browser_string();
Widget make_browser_popup();
int show_digth();
int show_snapth();
void digthreshcb();

/* debugf.c */
int init_debug();
int close_debug();
int debugf();

/* debugmenu.c */
int make_debug_menu();
void dpycb();
void elcb();
void el2cb();
void findcb();
void infocb();
Widget make_find_dialog();
void start_find();
void find();
void quit_find();
int show_lines();
int m_line_info();
int show_areas();
int show_isles();
int show_nodes();
int m_node_info();
int d_area_info();
int d_line_info();
int d_node_info();
void start_info();

/* dialogs.c */
void add_dlog_list();
void clear_dlogs();
void init_dlogs();
void alldown();

/* dig_dev.c */
int D_readall();
int D_ask_if_err();
int D_get_scale();
int D_end_digit();

/* dig_input.c */

/* dig_pop.c */
int take_dig_action();
int make_dig_popup();
Widget make_button_area2();
int show_dig_menu();
int close_dig_menu();
int xdig_readall();

/* digitize.c */
int _set_autolabel();
int Digitize();
int do_digitize();

/* digitmenu.c */
int autolabel();
int make_digit_menu();
void modecb();
void typecb();
void autolabcb();
void dig_on_off();
int write_type_info();

/* discio.c */
int openf();
int closef();
char *reada();
char *reada_old();

/* display_area.c */
int reset_area();
int _reset_area();
int erase_area();
int _erase_area();
int highlight_area();
int _highlight_area();
int display_area();
int _display_area();
int _area_display();
int highlight_area_label();
int display_area_label();
int undisplay_area_label();
int unset_dot();
int reset_isle();
int _reset_isle();
int erase_isle();
int _erase_isle();
int highlight_isle();
int _highlight_isle();
int display_isle();
int _display_isle();
int _isle_display();

/* display_line.c */
int display_line();
int erase_line();
int highlight_line();
int color_line();
int _display_line();
int _erase_line();
int _highlight_line();
int _color_line();
int _display_line_label();

/* display_node.c */
int display_node();
int highlight_node();
int color_node();
int erase_node();

/* display_site.c */
int display_site();
int erase_site();
int highlight_site();
int color_site();
int _display_site();
int _erase_site();
int _highlight_site();
int _color_site();
int _display_site_label();

/* do_proj.c */
int G_do_proj();

/* draw.c */
void redisplay();
int set_draw_mode();
int move_rel();
int move_abs();
int cont_abs();
int cont_rel();
int set_canvas();
int set_gc();
int draw_string();
int init_graphics();
int standard_color();
int translate_color();
int erase_window();
void fill_pix();
void resize();
int get_location_with_pointer();
int get_location_with_line();
int get_location_with_box();
int polyline_abs();
int polygon_abs();

/* drawcell.c */
int drawcell();

/* duplicate.c */
int display_duplicate_lines();

/* edit.c */
void Edit();

/* expose.c */
void expose();
int zero_display_list();
int add_to_display_list();
int display_win_elements();
int remove_from_list();

/* editmenu.c */
int make_edit_menu();
Widget make_dialog();

/* find_w_dig.c */
int find_node_with_dig();
int find_point_with_dig();
int new_point_with_dig();
int find_line_with_dig();
int make_dig_menu();
int end_menu();

/* find_w_mouse.c */
int set_done();
int find_node_with_mouse();
int find_point_with_mouse();
int new_point_with_mouse();
int find_line_with_mouse();
int _find_line_with_mouse();
int show_select_dialog();
int set_accept();
int actioncb();
int make_select_dialog();
int Check_for_action();
int destroy();

/* geom.c */
double v2angle();
double distance();
double distancep();
double distance2();
double distance2p();
int get_circle_rad();
int get_circle();
double clockangle();

/* get_deg.c */
int get_deg();

/* get_diginput.c */
int get_diginput();

/* get_point.c */
int get_point();
int new_point_with_digitizer();

/* get_proj.c */
int G_get_proj_PROJINFO();
int G_get_proj_string();
int G_zero_proj();

/* graph_input.c */
long raw_diginput();
long decode_digpkt();
long start_ginput();
int start_run_mode();
int start_point_mode();
int start_query_mode();
long query_ginput();
long stop_ginput();
long ginput_ctl();
long read_digfile();
long read_value();
long digdefaults();
long set_diginternals();
long ginput_setup();
int dig_input();
long bslash();
long dparse();
int print_binary_int();
int print_binary_char();
int change_mode();

/* initialize.c */
int initialize();
int init_plus();
int init_init();
int init_colors();

/* interface.c */
int D_cursor_buttons();
int D_start_button();
int D_foot_switch();
int D_clear_driver();
int D_ask_driver_raw();
int D_read_raw();
int delay();

/* intersect.c */
int intersect_line();

/* intro.c */
Widget make_info_pop();
void changes();
void gototop();
void put_head_info();
void get_head_info();
void close_infopop();
void showintro();
void set_map_name();

/* isle.c */
int Del_isle();

/* label.c */
int Label();
int label_area();
int label_lines();
int label_sites();
int label_line();
int unlabel_area();
int unlabel_lines();
int unlabel_sites();
int tell_line_label();
int make_area_label();
int tell_area_unlabel();
int tell_area_label();
int check_area();
int get_line_center();
int display_labeled_areas();
int display_all_areas();
int display_labeled_lines();
int label_all_lines();

/* label_mult.c */
int label_mlines();
int unlabel_line();

/* labelmenu.c */
int make_label_menu();
void catcb();
void intervalcb();
void hilgtcb();
void labelcb();
void unlabelcb();
void elemcb();
void change_pix();

/* make_pixmaps.c */
int make_pixmaps();

/* map_ask.c */
int ask_map_coor();
int shrink_map_coor();

/* map_coor.c */
int load_coor_from_file();
int save_coor_to_file();

/* map_reg.c */
int register_map_coor();

/* map_scale.c */
int check_scale();
int calculate_map_scale();
double get_map_scale();
int scale_done();
int ask_scale();

/* message.c */
void On();
void Off();
int make_yes_no();
void make_monolog();
void make_1st_monolog();
void message_callback();
void end_message();

/* mk_window.c */
int set_window_w_mouse();
int draw_default_window();
int Widen();

/* mouse_coll.c */
int mouse_collect_points();

/* mouse_yn.c */
int mouse_yes_no();
int next_prevcb();
int make_next_prev();
int show_next_prev();
int ask_next_prev();

/* move_line.c */
int move_line();

/* move_point.c */
int move_point();

/* new_line.c */
int new_line();

/* new_map.c */
void new_map();
void checkfile();
void old_map();
int check_proj();
int change_map();
int init_head();
int setup_win();

/* node_color.c */
int dig_node_color();

/* node_lines.c */
int node_lines();
char *tell_type();

/* oplot.c */
int plot_points();

/* options.c */
void make_changes();
void displaycb();
void Reset2();
void togglecb();
void toggle();
Widget make_options_menu();

/* overlay.c */
int overlay_off();
void show_overlay();
void no_overlay();
void ask_overlay();
int display_overlay();
int plot_overlay();
int free_name_info();

/* panel.c */
int clear();
int start_panel();
void make_display();
Widget make_button();
int add_inverse_map();
void boardchange();
void showinfo();
void buttonchange();
void savefile();
Widget make_menu();
Widget make_info_area2();
void make_menus_active();
void no_new_map();
void make_file_browser();

/* pull.c */
int accept_line();
int pull_it();

/* pull_display.c */
int redisplay_current_edit();
int draw_x();
int recalc_circle();
int recalc_dot();
int draw_circle();
int draw_dot();
int draw_disc();
int display_pullpoint ();
int erase_pullpoint ();
int display_sculptool ();
int display_blot_tool ();
int erase_anchors ();
int display_anchor ();
int display_circle ();
int display_disc ();
int display_dot ();
int recalc_dist ();
int recalc_pullbuf();
int display_pullseg ();
int ghost_line();

/* pull_panel.c */
void scale_cb();
void check_changes();
int init_pull();
Widget install_control_panel();
Widget make_slider();
int show_pull();
int set_func();
void do_pull();
void do_blot();
void do_plreset();
void do_newpl();
void close_pl();
void do_sculpt();
void do_whole_line();
void set_centroid();
void display_plscale();
void pl_scale();
int reset_line();
void test_circles();
void set_anchor_points();
void swap_anchors();
void update_pfac();

/* plot.c */
int nplot_points();

/* quit.c */
void Quit();
void Really_quit();
int last_words();
int do_file_checks();
int close_down();

/* read_cap.c */
int read_cap_line();
int get_driver_name();

/* reg_pop.c */
int take_reg_action();
int make_reg_popup();
Widget make_button_area();
int set_active();
int set_reg_coors();
int show_reg_ask();
int show_residual_results();
int close_reg_ask();
int get_reg_response();

/* remove.c */
int remove_line();
int _remove_line();

/* remove_block.c */
int remove_block();

/* replot.c */
void redraw();
int replot();
int line_in_window();
int _line_in_window();
int _line_really_in_window();
int display_nodes();
int display_all_lines();

/* reset_map.c */
int reset_map();
int save_coor();

/* retype_line.c */
int retype_line();
int check_next();

/* save.c */
Widget make_saveas_pop();

/* scal_window.c */
int scal_window_w_mouse();

/* screen_plt.c */
int First();
int Next();
int Adot();
int Dot();
int Blot();
int _Blot();
int _BigBlot();

/* sculpt.c */
int get_corners();
Pntlist *copy_pbuf_tolist();
Pntlist *copy_pl();
void copy_list_to_buf();
void empty_pl();
Pntlist *copy_line_tolist();
int sculpt_it();
int blot_it();
/* select_digit.c */
void get_new_dgtzer();
int set_dig_name();
void make_dig_select();
int unlock_file();

/* set_prior.c */
int set_priority_3b2();
int init_priority();
int set_priority();
int unset_priority();
int swap_re_uids();
int set_uid_to_user();

/* set_thresh.c */
int set_thresh();
int reset_snap_thresh();
int reset_thresh();
int calc_snap_thresh();
int calc_thresh();
int map_to_dig_thresh();
double _map_to_dig_thresh();

/* set_window.c */
int init_window();
int set_window();

/* sleep.c */
long sleep_ltp();
long time_ltp();
long time_ltp();

/* slid_window.c */
int slid_window_w_mouse();

/* snap.c */
int snap_nodes();
int near_zero();

/* states.c */
int init_states();
int set_default_display();
int set_default_display();
int get_mem();

/* str.c */
char *trim();
char *lowerc();
long strsplit();

/* stuff.c */
int keep_line();
void ringbell();
int fill_pbuf_orig();
int fill_pbuf_line();
int fill_pbuf_arc();
int fill_pbuf_arc2();
double pull_spline();
double pull_line();
double pull_trial();
double pull_cos();
double pull_spl1();
double pull_spl2();
double pull_spl3();
double pull_semi();
int get_closest_point();
int get_closest_point_on_list();

/* text.c */
int set_text_widget();
void showtext();
int set_info_widget();
int set_info2();
void write_info();
void add_info();

/* thresh.c */
Widget make_thresh_popup();

/* toolboxmenu.c */
int make_toolbox_menu();
Widget make_thresh_popup();
void tbox();

/* watch.c */
/*
int stop();
*/
int Check_for_interrupt();
int TimeOutCursor();

/* where_am_i.c */
int where_am_i();


/* wind_2_box.c */
int G_adjust_window_to_box();

/* wind_conv.c */
int window_conversions();
int utm_to_screen();
int screen_to_utm();
int outline_window();
int get_D_west();
int get_D_east();
int get_D_north();
int get_D_south();

/* window.c */
int win_men();
int display_cents();
int display_alabels();
int highlight_llabel();
int display_llabel();
int display_llabels();
int display_llines();
int display_lareas();
int display_unlabeled_areas();
int display_islands();
int expand_window();
int move_window();
int area_outside_window();
int line_outside_window();
int clear_window();
int Save_Disp_settings();
int Restore_Disp_settings();
int Zero_Disp_settings();
int slid_window();
int scal_window();

/* window_rout.c */
int window_rout();
int fit_window();

/* windowmenu.c */
int make_win_menu();

/* withinthresh.c */
int get_thresh();
int within_a_thresh();

/* xcolor.c */
Widget make_colors_menu();
void alloc_colors();
void toggle_color();
void change_color_value();
void new_button_color();
void write_curr_color();
void change_curr_color();
void Reset();

/* zoom.c */
int set_window_w_box();
Widget make_zoom();
int Zoomcb();
int Pancb();
void Zoom();
int destroy_shell();

#undef P
