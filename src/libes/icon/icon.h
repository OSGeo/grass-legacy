#define ICON struct _icon
struct _icon
{
    int nrows;
    int ncols;
    int xref;
    int yref;
    char **map;
} ;

char *ask_icon_old();
char *ask_icon_new();
char *ask_icon_any();
