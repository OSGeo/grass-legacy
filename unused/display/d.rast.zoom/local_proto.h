/* cfm_window.c */
int confirm_window(struct Cell_head *, char [128], char *);
/* cur_frm_db.c */
int cur_from_db(struct Cell_head *, char *, char *);
/* cur_to_db.c */
int cur_to_db(struct Cell_head *);
/* final_check.c */
int final_check(struct Cell_head *, char [128], char *);
/* mke_window.c */
int make_window(struct Cell_head *, char [128], char *);
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
/* unzoom.c */
int unzoom(struct Cell_head *, char [128], char *);
/* what.c */
int what(char [128], struct Cell_head *);
