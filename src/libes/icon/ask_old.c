#include "gis.h"
#include "icon.h"

char *
ask_icon_old (
    char *prompt,
    char *name)
{
    return G_ask_old (prompt, name, "icons", "icon");
}
