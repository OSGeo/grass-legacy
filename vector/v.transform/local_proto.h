/* ask_trans.c */
int ask_transform_coor(int);
/* command.c */
int set_default_options(struct file_info *, struct file_info *, struct file_info *, struct command_flags *);
/* creat_trans.c */
int create_transform_conversion(struct file_info *, int);
int create_transform_from_user(void);
int yes_no_quest(char *);
int create_transform_from_file(struct file_info *, int);
/* get_coor.c */
int get_coor_from_file(FILE *);
/* main.c */
int main(int, char *[]);
/* open_files.c */
int open_att_files(struct file_info *, struct file_info *);
/* print_trans.c */
int print_transform_resids(int);
/* setup_trans.c */
int setup_transform(int);
int init_transform_arrays(void);
int print_transform_error(int);
/* trans_att.c */
int transform_att_file(FILE *, FILE *);
/* trans_cats.c */
void trans_dig_cats (const char * /* from */, 
                     const char * /* from_mapset */, 
                     const char * /* to */);
/* trans_digit.c */
int transform_digit_file(struct Map_info *, struct Map_info *, float, float);
