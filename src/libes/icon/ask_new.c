#include "gis.h"

char *
ask_icon_new (prompt, name)
    char *prompt;
    char *name;
{
    return G_ask_new (prompt, name, "icons", "icon");
}
