struct Menu_item
{
    char *text;
    char command;
    char enabled;
};

struct Menu_head
{
    struct Menu_item *item;
    char *name;
    char changed;
    int (*help)() ;
};

int Help(struct Menu_head *);
int help_valid_key(int, struct Menu_head *);
int _is_enabled(int, struct Menu_head *);
int global_menu(int, struct Menu_head *);
int update_menu(struct Menu_head *);
int get_menu_command(struct Menu_head *, int *);
int set_menu_item(struct Menu_head *, int, int);
int Display_settings(struct Menu_head *);
/* dig_curses.c */
int _Write_generic_win(struct Menu_head *);
int Write_generic_win(struct Menu_head *);
