/* cur_frm_db.c */
int cur_from_db(void);
/* cur_frm_df.c */
int cur_from_def(void);
/* cur_to_db.c */
int cur_to_db(void);
/* edit.c */
int edit_window(struct Cell_head *);
/* from_cell.c */
int from_cellhd(void);
/* from_vect.c */
int from_vect(void);
/* from_view.c */
int from_view(void);
/* lister.c */
int lister(char *, char *, char *);
/* main.c */
int main(int, char *[]);
/* mke_curr.c */
int make_current(struct Cell_head *, char *);
/* modify_cur.c */
int modify_cur(void);
/* modify_db.c */
int modify_db(void);
/* new_db.c */
int new_db(void);
/* set.c */
int set_window(struct Cell_head *, char *);
/* yes.c */
int yes(char *, int);
