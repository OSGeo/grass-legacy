struct datum_list
{
    char *name, *longname, *ellps;
    double dx, dy, dz;
    struct datum_list *next;
};

struct datum_list *read_datum_table(void);
void free_datum_list(struct datum_list *);
