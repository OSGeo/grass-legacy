typedef struct {char *text; int choice;} MENU[];
/* menu_hndlr.c */
int menu_handler (MENU,char *);
/* scan_int.c */
int scan_int(char *, int *);
/* scn_double.c */
int scan_double(char *, double *);
