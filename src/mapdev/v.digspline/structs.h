
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

