
#define SITE_LIST struct _sitelist
#define SITE struct _site

SITE
{
    double north;
    double east;
    char *desc;
    SITE *next;
};

SITE_LIST
{
    char name[100];
    char desc[100];
    SITE *cur;
    SITE *first;
    SITE *last;
};
