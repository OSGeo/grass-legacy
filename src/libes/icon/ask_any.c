#include "gis.h"

char *
ask_icon_any (prompt, name, warn)
    char *prompt;
    char *name;
{
    return G_ask_any (prompt, name, "icons", "icon", warn);
}
