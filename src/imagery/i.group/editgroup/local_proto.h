/* ask_files1.c */
int ask_oldfiles(struct Ref *, struct Ref *, char *);
/* ask_files2.c */
int ask_newfiles(FILE *, struct Ref *, char *);
int new_mapset(FILE *, char *);
int ask_newfiles_in_mapset(FILE *, char *, struct Ref *, char *);
int get_name(FILE *, char *);
/* cellfiles.c */
int find_all_cellfiles(FILE *, struct Ref *);
/* transfer.c */
int transfer_file(struct Ref *, int, struct Ref *);
