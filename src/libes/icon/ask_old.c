#include "gis.h"

char *
ask_icon_old (prompt, name)
    char *prompt;
    char *name;
{
    return G_ask_old (prompt, name, "icons", "icon");
}
