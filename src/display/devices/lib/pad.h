
#ifndef _PAD_H_
#define _PAD_H_

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

PAD *pad_list(void);
PAD *find_pad(char *);
int delete_pad(PAD *);
int create_pad(char *);
int append_item(PAD *, char *, char *, int);
int invent_pad(char *);
int delete_item(PAD *, char *);
ITEM *find_item(PAD *, char *);

#endif /* _PAD_H_ */

