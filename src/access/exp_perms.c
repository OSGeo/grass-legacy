#include "gis.h"
char *
explain_perms (group, other, will)
{
    static char buf[128];
    char *who;
    char *verb;

    verb="have";
    if (group && other)
    {
	who  = "Everyone";
	verb = "has";
    }
    else if (group)
    {
	who = "Only users in your group";
    }
    else if (other)
    {
	who = "Only users outside your group";
    }
    else
    {
	who = "Only you";
    }
    if (will) verb = "have";

    sprintf (buf, "%s %s %s access to mapset %s",
	who, will?"will":"now", verb, G_mapset());
    return buf;
}
