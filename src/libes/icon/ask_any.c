#include "gis.h"
#include "icon.h"

char *
ask_icon_any (char *prompt, char *name,int warn)
{
    return G_ask_any (prompt, name, "icons", "icon", warn);
}
