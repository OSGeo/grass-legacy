#include "gis.h"
#include "icon.h"

char *
ask_icon_new (char *prompt, char *name)
{
    return G_ask_new (prompt, name, "icons", "icon");
}
