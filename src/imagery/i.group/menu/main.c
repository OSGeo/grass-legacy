#include "imagery.h"
#include "local_proto.h"

static char title[80];
static char *info[] =
{
title,
"",
"This program edits imagery groups. You may add data layers to, or remove",
"data layers from an imagery group. You may also create new groups",
"",
"Please enter the group to be created/modified",
0
};


int 
main (int argc, char *argv[])
{
    char group[30];
    int new;

    G_gisinit (argv[0]);

    I_location_info (title, argv[0]);
    while(1)
    {
	if (!I_vask_group_new (info, group, "EXIT"))
	    break;
	new = !I_find_group (group);
	if (new)
	{
	    fprintf (stderr, "\n%s - does not exist, ", group);
	    if(!G_yes("do you wish to create a new group? ", 0))
		continue;
	}
	menu (group, new);
    }

    return 0;
}
