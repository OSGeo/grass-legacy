/* @(#)mke_loc.c	2.1 12/2/87 */
#include "gis.h"

static char *intro[]=
{
"",
"To create a new LOCATION, you will need the following information:",
"",
"1. The projection and zone for the database",
"2. The coordinates of the area to become the default window",
"   and the grid resolution of this window",
"3. A short, one-line description or title for the location",
"",
0};

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


    for (i = 0; intro[i]; i++)
	printf ("%s\n", intro[i]);
    printf ("Do you have all this information for location <%s> ? ", location_name);
    if (!yes_no())
	return 0;
    
    *myname = 0;
    sprintf (title, "Please enter the information for location <%s>", location_name);
    G_zero (&window, sizeof(window));
    V_clear();
    V_line (0, title);
    V_line (t=2, "TITLE: ");

i = t+3;
V_line (i, "                         DEFAULT WINDOW") ; i++;
V_line (i, "                 =============================") ; i++;
V_line (i, "                 | NORTH EDGE:               |") ; n=i++;
V_line (i, "                 |                           |") ; i++;
V_line (i, "      WEST EDGE  |                           |EAST EDGE") ; i++;
V_line (i, "                 |                           |") ; e=i++;
V_line (i, "                 |                           |") ; i++;
V_line (i, "                 | SOUTH EDGE:               |") ; s=i++;
V_line (i, "                 =============================") ; i++;

i+=2;
V_line (i, "GRID RESOLUTION                      PROJECTION:") ; p=i++;
V_line (i, " East-West:                          ZONE:") ; r=i++;
V_line (i, " North-South:") ; i++;

    /* V_ques ( variable, type, row, col, length) ; */
    V_ques (myname, 's', t, 8, 70);

    V_ques ( &window.north ,  'd',  n, 30, 11) ;
    V_ques ( &window.south ,  'd',  s, 30, 11) ;
    V_ques ( &window.west  ,  'd',  e,  6, 11) ;
    V_ques ( &window.east  ,  'd',  e, 46, 11) ;
    V_ques ( &window.ew_res,  'd',  r, 14, 11) ;
    V_ques ( &window.ns_res,  'd',  r+1, 14, 11) ;

    V_ques ( &window.proj,    'i', p, 50,  3) ;
    V_ques ( &window.zone,    'i', p+1, 50,  3) ;


    for(;;)
    {
	V_intrpt_ok();
	if (!V_call())
	    return 0;
	stat = check_info (myname, &window);
	if (stat < 0) return 0;
	if (stat > 0) break;
    }

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
