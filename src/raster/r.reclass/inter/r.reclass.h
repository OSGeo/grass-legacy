/* do_reclass.c */
int do_reclass(char *, char *, char *, struct Categories *, struct Categories *, CELL *, CELL, CELL);
/* dots.c */
int dots(char *, char *, int);
/* makemask.c */
int makemask(void);
int hitreturn(void);
/* maketable.c */
int maketable(struct Categories *, CELL *, CELL, CELL, int);
/* maskinfo.c */
int maskinfo(void);
int reclass_text(char *, struct Reclass *, int);
int do_text(char *, long, long);
/* set_cats.c */
CELL set_new_cats(struct Categories *, struct Categories *, CELL *, CELL, CELL);
