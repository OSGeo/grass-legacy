#include "gis.h"

make_location (gisdbase, location_name)
    char *gisdbase, *location_name;
{
    struct Cell_head window;
    char title[75];
    char buf[1024];
    int i,t,n,s,e,p,r;
    char myname[75];
    char *mapset;
    int stat;
    char *name;
    char *G__projection_name();

    G_clear_screen();
    printf ("To create a new LOCATION, you will need the following information:\n");
    printf ("\n");
    printf ("1. The coordinate system for the database\n");
    name = G__projection_name(0);
    printf ("        %s (for imagery and other unreferenced data)\n", name);
    printf ("        %s\n", G__projection_name(3));
    printf ("        or any of several other geo-referenced data projections)\n");
    printf ("2. The zone for the database (if required)\n");
    printf ("       (except for %s",G__projection_name(0));
    printf (" and %s databases)\n", G__projection_name(PROJECTION_LL));
    printf ("3. The coordinates of the area to become the default region\n");
    printf ("   and the grid resolution of this region\n");
    printf ("4. A short, one-line description or title for the location\n");
    printf ("\n");

    printf ("Do you have all this information for location <%s> ? ", location_name);
    if (!yes_no())
	return 0;

    G_zero (&window, sizeof(window));
    while(1)
    {
        G_clear_screen();
  	printf ("Please specify the coordinate system for location <%s>\n\n",
		location_name);
        i = 0;
	printf ("%4d %s\n", i, G__projection_name(i));
        i = 3;
	printf ("%4d %s\n", i, G__projection_name(i));
        i = 99;
	printf ("%4d All other projections\n",i);
	printf ("RETURN to cancel\n");
	printf ("\n");
	printf ("> ");
	if (!G_gets(buf))
	    continue;
	G_strip (buf);
	if (*buf == 0) return 0;
	if (sscanf (buf, "%d", &i) != 1)
	    continue;
	if (i == 99) get_proj(name);
	else name = G__projection_name(i);
	if (name == NULL)
	    continue;
	printf ("\n");
	sprintf (buf, "\n%s coordinate system? ", name);
	if (G_yes (buf, 1))
	    break;
    }

    window.proj = i;
    while (window.proj != 0 && window.proj != PROJECTION_LL)
    {
        G_clear_screen();
	if (strncmp(name,"utm",3) != 0) break;
	printf ("Please specify the utm zone for location <%s>\n",
					     location_name);
	printf ("or RETURN to cancel\n");
	printf ("\n");
	printf ("> ");
	if (!G_gets(buf))
	    continue;
	G_strip (buf);
	if (*buf == 0) return 0;
	if (sscanf (buf, "%d", &i) != 1)
	    continue;
	printf ("\n");
	sprintf (buf, "zone %d? ", i);
	if (G_yes (buf, 1))
	{
	    window.zone = i;
	    break;
	}
    }

    while(1)
    {
        G_clear_screen();
	printf ("Please enter a one line description for location <%s>\n\n",
		location_name);
	printf ("> ");
	if (!G_gets(buf))
	    continue;
	G_squeeze(buf);
	buf[sizeof(myname)] = 0;
	G_squeeze(buf);
	printf ("=====================================================\n");
	printf ("%s\n", buf);
	printf ("=====================================================\n");
	if (G_yes("ok? ", *buf != 0))
	    break;
    }
    strcpy (myname, buf);
    if(G_edit_cellhd(&window, -1) < 0) 
	return 0;

    mapset = "PERMANENT";
    G__setenv ("MAPSET", mapset);
    G__setenv ("LOCATION_NAME", location_name);

    sprintf (buf, "mkdir %s/%s", gisdbase, location_name);
    if(system(buf)) return 0;
    sprintf (buf, "mkdir %s/%s/%s", gisdbase, location_name, mapset);
    if(system(buf)) return 0;
    G__put_window (&window, "", "DEFAULT_WIND");
    G__put_window (&window, "", "WIND");
    sprintf (buf, "echo '%s' >  %s/%s/%s/MYNAME", myname, gisdbase, location_name, mapset);
    system(buf);
    return 1;
}
