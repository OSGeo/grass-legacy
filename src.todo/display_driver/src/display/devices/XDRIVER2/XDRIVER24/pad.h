typedef struct _list {
    char *value;
    struct _list *next;
} LIST;

typedef struct _item_ {
    char *name;
    LIST *list;
    struct _item_ *next, *prev;
} ITEM;

typedef struct _pad_ {
    char *name;
    ITEM *items;
    struct _pad_ *next, *prev;
} PAD;

ITEM *find_item(PAD *pad, char *name);
int delete_pad(PAD *);
PAD *find_pad(char *);
int append_item(PAD *, char *, char *);
int delete_item(PAD *, char *);
PAD* get_current_pad();

/* Serve_Xevent.c */
int _time_stamp(PAD *);
