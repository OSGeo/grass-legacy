/* setup:  Program for GRASS user to first use.  Sets environment
 * variables:  LOCATION_NAME MAPSET GISDBASE
 */

static char *intro[] =
{
"                   PLEASE SET SESSION INFORMATION",
"",
"LOCATION: This is the name of an available geographic location.  -spearfish-",
"          is the sample data base for which all tutorials are written.",
"",
"MAPSET:   Every GRASS session runs under the name of a MAPSET.  Associated",
"          with each MAPSET is a rectangular COORDINATE REGION and a list ",
"          of any new maps created.",
"",
"DATABASE: This is the unix directory containing the geographic databases",
"",
"         The REGION defaults to the entire area of the chosen LOCATION.",
"         You may change it later with the command: g.region",
"- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
0};
static char *loc_text=
"LOCATION:                              (enter list for a list of locations)";
static char *map_text=
"MAPSET:                                (or mapsets within a location)";

#include "gis.h"

main(argc, argv) 
    char *argv[] ;
{
    char *gets() ;

    char version[80];
    char gisdbase[51]   ;
    char location_name[41]   ;
    char location[1024]  ;
    char mapset[41]   ;
    int line;
    int yes;

    char *GISDBASE;
    char *LOCATION_NAME;
    char *MAPSET;

    int  repeat ;

/* GISBASE
 *   comes from the unix environment and must be set
 *   make sure it is NOT in the .gisrc file
 *   note: G_getenv() make a special case of this variable
 */
    G_no_gisinit();
    umask(0);
    G_unsetenv ("GISBASE");      /* this takes it out of .gisrc */
    G_getenv ("GISBASE") ;         /* this reads it from the unix environment */

/* Gather all existing variables **********************************************/
    GISDBASE      =	G__getenv("GISDBASE") ;
    LOCATION_NAME =	G__getenv("LOCATION_NAME") ;
    MAPSET        =	G__getenv("MAPSET") ;

    if (!MAPSET)        MAPSET        = G_whoami() ;
    if (!LOCATION_NAME) LOCATION_NAME = D_LOCATION_NAME ;
    if (!GISDBASE)      GISDBASE      = D_GISDBASE ;

    strcpy (mapset,        MAPSET);
    strcpy (location_name, LOCATION_NAME);
    strcpy (gisdbase,      GISDBASE);
    G__setenv ("GISDBASE",      gisdbase) ;

    sprintf (version, "                            GRASS %s", VERSION_NUMBER);

    for (repeat = 1; repeat ; hit_return())
    {
	V_clear() ;
	V_line (0, version);
	for (line = 1; intro[line]; line++)
	    V_line (line, intro[line]);

	line++;
	V_line (line, loc_text);
	V_ques (location_name, 's', line++, 12, 14);

	V_line (line, map_text);
	V_ques (mapset, 's', line++, 12, 14);

	line++;
	V_line (line, "DATABASE:");
	V_ques (gisdbase, 's', line++, 12, sizeof(gisdbase) - 1);

	V_intrpt_ok();
	if (!V_call())
	    exit(1) ;

	G_strip (gisdbase);
	if (*gisdbase == 0)
	{
	    printf ("No DATABASE specified\n");
	    strcpy (gisdbase, D_GISDBASE);
	    continue;
	}
	if (*gisdbase != '/')
	{
	    char temp[200];
	    printf ("DATABASE <%s> - must start with /\n", gisdbase);
	    sprintf (temp, " %s", gisdbase);
	    strcpy (gisdbase, temp);
	    continue;
	}
	if (access (gisdbase,0) != 0)
	{
	    printf ("DATABASE <%s> - not found\n", gisdbase);
	    continue;
	}
	G__setenv ("GISDBASE",      gisdbase) ;

	/* take only first word of responses */
	first_word (location_name);
	first_word (mapset);

	if (*location_name && (G_legal_filename(location_name) < 0))
	{
	    printf ("LOCATION <%s> - illegal name\n", location_name);
	    continue;
	}
	if (*mapset && (G_legal_filename(mapset) < 0))
	{
	    printf ("MAPSET <%s> - illegal name\n", mapset);
	    continue;
	}

	if (*location_name == 0 || strcmp (location_name, "list") == 0)
	{
	    list_locations (gisdbase);
	    *location_name = 0;
	    continue;
	}
	sprintf (location,"%s/%s",gisdbase,location_name);
	if (access (location,0) != 0)
	{
	    printf ("LOCATION <%s> - doesn't exist\n", location_name);
	    list_locations (gisdbase);
	    if (!can_make_location (gisdbase, location_name))
		continue;
	    printf ("\nWould you like to create location <%s> ? ", location_name);
	    if (yes_no())
	    {
		if(make_location (gisdbase, location_name))
		    printf ("LOCATION <%s> created!\n", location_name);
		else
		    printf ("LOCATION <%s> NOT created\n", location_name);
	    }
	    continue;
	}
	G__setenv ("LOCATION_NAME", location_name);
	if (*mapset == 0 || strcmp (mapset, "list") == 0)
	{
	    list_mapsets (location_name, location);
	    *mapset = 0;
	    continue;
	}
	G__setenv ("MAPSET",        mapset) ;

	repeat = 0 ;

	switch (G__mapset_permissions(mapset) )
	{
	case -1:
		if(strcmp(mapset, G_whoami()) == 0)
		{
		    yes = 1;
		}
		else
		{
		    repeat = 1 ;
		    printf("\n\nMapset <<%s>> is not available\n", mapset) ;
		    list_mapsets (location_name, location);
		    printf("\nWould you like to create < %s > as a new mapset? ",
				mapset ) ;
		    yes = yes_no();
		}

		if (yes && (make_mapset(location, mapset) == 0))
		    repeat = 0 ;
		break ;
	case 0:
		printf("\n\nSorry, no access to <<%s>>.\n", mapset) ;
		list_mapsets (location_name, location);
		repeat = 1 ;
		break ;
	case 1:
		break ;
	}
	if (!repeat) break;
    }

    G__write_env() ;
    exit(0);
}

list_locations (gisdbase)
    char *gisdbase;
{
    char buf[1024];
    printf("\nAvailable locations:\n") ;
    printf("----------------------\n") ;
    sprintf(buf, "ls -C %s", gisdbase) ;
    system(buf) ;
    printf("----------------------\n") ;
}

list_mapsets (location_name, location)
    char *location_name, *location;
{
    char buf[1024];
    FILE *fd, *popen();
    int any, ok, any_ok;
    int len, tot_len;

    sprintf (buf, "ls %s", location);
    printf ("\nMapsets in location <%s>\n", location_name);
    printf("----------------------\n") ;
    fd = popen (buf, "r");
    any = 0;
    any_ok = 0;
    tot_len = 0;
    if (fd)
    {
	while (fscanf (fd, "%s", buf) == 1)
	{
	    any = 1;
	    len = strlen (buf)+1;
	    len /= 20;
	    len = (len+1)*20;
	    tot_len += len;
	    if (tot_len > 75)
	    {
		printf ("\n");
		tot_len = len;
	    }
	    if(ok = (G__mapset_permissions(buf) == 1))
		any_ok = 1;
	    printf ("%s%-*s", ok?"(+)":"   ", len, buf);
	}
	pclose (fd);
	if (tot_len)
	    printf ("\n");
	if (any_ok)
	    printf ("\nnote: you only have access to mapsets marked with (+)\n");
	else if (any)
	    printf ("\nnote: you do not have access to any of these mapsets\n");
    }
    printf("----------------------\n") ;
}

first_word (buf)
    char *buf;
{
    char temp[80];

    *temp = 0;
    sscanf (buf, "%s", temp);
    strcpy (buf, temp);
}

hit_return()
{
    char buf[80];

    printf ("\nHit RETURN -->");
    G_gets(buf);
}

yes_no()
{
    char buf[80];
    for(;;)
    {
	do
	{
	    printf ("(y/n) ");
	} while (!G_gets(buf));
	G_strip (buf);
	switch (*buf)
	{
	case 'y': 
	case 'Y': return 1;
	case 'n': 
	case 'N': return 0;
	}
    }
}

