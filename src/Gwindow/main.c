#include "gis.h"

main (argc, argv) char *argv[];
{
    int i;
    int print_flag;
    int set_flag;
    double x;
    struct Cell_head window, align, window2;
    char msg[200];
    char value[100];
    char name[100];
    char *mapset;
    char *err;
    char *G_align_window();

    static char *parms1[] =
    {
	"-",
	"gprint",
	"print",
	"default",
	NULL
    };
    static char *parms2[] =
    {
	"n=", "n+=", "n-=", "n=s+",
	"s=", "s+=", "s-=", "s=n-",
	"e=", "e+=", "e-=", "e=w+",
	"w=", "w+=", "w-=", "w=e-",
	"res=", "nsres=", "ewres=",
	"window=", "save=",
	"layer=", "zoom=", "align=",
	NULL
    };

    G_gisinit (argv[0]);

/* get current window.
 * if current window not valid, set it from default
 * note: G_get_default_window() dies upon error
 */
    if (G__get_window (&window, "", "WIND", G_mapset()) < 0)
    {
	G_get_default_window (&window);
	G_put_window (&window);
    }

    if (argc <= 1)
    {
	print_flag = 1;
	set_flag = 0;
    }
    else
    {
	print_flag = 0;
	set_flag = 1;
    }

    for (i = 1; i < argc; i++)
    {
	int hit, j;

	hit = 0;
	for (j = 0; !hit && parms1[j]; j++)
	    hit = strcmp (argv[i], parms1[j]) == 0;
	for (j = 0; !hit && parms2[j]; j++)
	    hit = strncmp (argv[i], parms2[j], strlen(parms2[j])) == 0;
	if (!hit)
	{
	    fprintf (stderr, "%s: illegal parameter: %s\n", argv[0],argv[i]);
	    usage();
	}
    }

    for (i = 1; i < argc; i++)
    {
	if (strcmp (argv[i], "-") == 0)
	{
	    set_flag = 0;
	    continue;
	}
	if (strcmp (argv[i], "print") == 0)
	{
	    print_flag = 1;
	    continue;
	}
	if (strcmp (argv[i], "gprint") == 0)
	{
	    print_flag = 2;
	    continue;
	}
	if (strcmp (argv[i], "default") == 0)
	{
	    G_get_default_window (&window);
	    continue;
	}
	if (sscanf (argv[i], "window=%[^\n]", name) == 1)
	{
	    G_strip (name);
	    mapset = G_find_file ("windows", name, "");
	    if (!mapset)
	    {
		sprintf (msg, "window <%s> not found", name);
		G_fatal_error (msg);
	    }
	    if (G__get_window (&window, "windows", name, mapset) < 0)
	    {
		sprintf (msg, "can't read window <%s> in <%s>", name, mapset);
		G_fatal_error (msg);
	    }
	    continue;
	}
	if (sscanf (argv[i], "save=%[^\n]", name) == 1)
	{
	    G_strip (name);
	    if (G_legal_filename (name) < 0)
	    {
		sprintf (msg, "<%s> - illegal window name", name);
		G_fatal_error (msg);
	    }
	    G_copy (&window2, &window, sizeof(window));
	    adjust_window (&window2);
	    if (G__put_window (&window2, "windows", name) < 0)
	    {
		sprintf (msg, "can't write window <%s>", name);
		G_fatal_error (msg);
	    }
	    continue;
	}
	if (sscanf (argv[i], "layer=%[^\n]", name) == 1)
	{
	    G_strip (name);
	    mapset = G_find_cell (name, "");
	    if (!mapset)
	    {
		sprintf (msg, "layer <%s> not found", name);
		G_fatal_error (msg);
	    }
	    if (G_get_cellhd (name, mapset, &window) < 0)
	    {
		sprintf (msg, "can't read header for <%s> in <%s>",
			name, mapset);
		G_fatal_error (msg);
	    }
	    continue;
	}
	if (sscanf (argv[i], "align=%[^\n]", name) == 1)
	{
	    G_strip (name);
	    mapset = G_find_cell (name, "");
	    if (!mapset)
	    {
		sprintf (msg, "layer <%s> not found", name);
		G_fatal_error (msg);
	    }
	    if (G_get_cellhd (name, mapset, &align) < 0)
	    {
		sprintf (msg, "can't read header for <%s> in <%s>",
			name, mapset);
		G_fatal_error (msg);
	    }
	    if (err = G_align_window (&window, &align))
	    {
		sprintf (msg, "%s: %s\n", argv[i], err);
		G_fatal_error (msg);
	    }
	    continue;
	}
	if (sscanf (argv[i], "zoom=%[^\n]", name) == 1)
	{
	    G_strip (name);
	    mapset = G_find_cell (name, "");
	    if (!mapset)
	    {
		sprintf (msg, "layer <%s> not found", name);
		G_fatal_error (msg);
	    }
	    zoom (&window, name, mapset);
	    continue;
	}
	if (sscanf (argv[i], "n=%[^\n]", value) == 1)
	{
	    if (!G_scan_northing (value, &x, window.proj))
		die(argv[i],"n=");
	    window.north = x;
	    continue;
	}
	if (sscanf (argv[i], "n+=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"n+=");
	    window.north += x;
	    continue;
	}
	if (sscanf (argv[i], "n-=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"n-=");
	    window.north -= x;
	    continue;
	}
	if (sscanf (argv[i], "n=s+%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"n=s+");
	    window.north = window.south + x;
	    continue;
	}
	if (sscanf (argv[i], "s=%[^\n]", value) == 1)
	{
	    if (!G_scan_northing (value, &x, window.proj))
		die(argv[i],"s=");
	    window.south = x;
	    continue;
	}
	if (sscanf (argv[i], "s+=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"s+=");
	    window.south += x;
	    continue;
	}
	if (sscanf (argv[i], "s-=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"s-=");
	    window.south -= x;
	    continue;
	}
	if (sscanf (argv[i], "s=n-%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"s=n-");
	    window.south = window.north - x;
	    continue;
	}
	if (sscanf (argv[i], "e=%[^\n]", value) == 1)
	{
	    if (!G_scan_easting (value, &x, window.proj))
		die(argv[i],"e=");
	    window.east = x;
	    continue;
	}
	if (sscanf (argv[i], "e+=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"e+=");
	    window.east += x;
	    continue;
	}
	if (sscanf (argv[i], "e-=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"e-=");
	    window.east -= x;
	    continue;
	}
	if (sscanf (argv[i], "e=w+%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"e=w+");
	    window.east = window.west + x;
	    continue;
	}
	if (sscanf (argv[i], "w=%[^\n]", value) == 1)
	{
	    if (!G_scan_easting (value, &x, window.proj))
		die(argv[i],"w=");
	    window.west = x;
	    continue;
	}
	if (sscanf (argv[i], "w+=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"w+=");
	    window.west += x;
	    continue;
	}
	if (sscanf (argv[i], "w-=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"w+=");
	    window.west -= x;
	    continue;
	}
	if (sscanf (argv[i], "w=e-%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"w=e-");
	    window.west = window.east - x;
	    continue;
	}
	if (sscanf (argv[i], "res=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"res=");
	    window.ns_res = window.ew_res = x;
	    continue;
	}
	if (sscanf (argv[i], "nsres=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"nsres=");
	    window.ns_res = x;
	    continue;
	}
	if (sscanf (argv[i], "ewres=%[^\n]", value) == 1)
	{
	    if (!G_scan_resolution (value, &x, window.proj))
		die(argv[i],"ewres=");
	    window.ew_res = x;
	    continue;
	}
	usage();
    }
    adjust_window (&window);
    if (set_flag)
    {
	if (G_put_window (&window) < 0)
	    G_fatal_error ("unable to update current window");
    }
    if (print_flag)
    {
	print_window (&window, print_flag);
    }
}

static
print_window(window,print_flag)
    struct Cell_head *window;
{
    char *G_projection_name();
    char *prj;
    int x;
    char north[30], south[30], east[30], west[30], nsres[30], ewres[30];

    if (print_flag == 2)
	x = -1;
    else
	x = window->proj;

    G_format_northing (window->north, north, x);
    G_format_northing (window->south, south, x);
    G_format_easting  (window->east,  east,  x);
    G_format_easting  (window->west,  west,  x);
    G_format_resolution  (window->ew_res,  ewres,  x);
    G_format_resolution  (window->ns_res,  nsres,  x);
    if (print_flag == 1)
    {
	prj = G_projection_name(window->proj);
	if (!prj) prj = "** unknown **";
	printf ("%-11s %d (%s)\n","projection:", window->proj, prj);
	printf ("%-11s %d\n","zone:",  window->zone);

	printf ("%-11s %s\n","north:", north);
	printf ("%-11s %s\n","south:", south);
	printf ("%-11s %s\n","east:",  east);
	printf ("%-11s %s\n","west:",  west);
	printf ("%-11s %s\n","nsres:", nsres);
	printf ("%-11s %s\n","ewres:", ewres);

	printf ("%-11s %d\n","rows:", window->rows);
	printf ("%-11s %d\n","cols:", window->cols);
    }
    else
    {
	printf ("n=%s\n",    north);
	printf ("s=%s\n",    south);
	printf ("e=%s\n",    east);
	printf ("w=%s\n",    west);
	printf ("nsres=%s\n",nsres);
	printf ("ewres=%s\n",ewres);
    }
}

static
die(arg,key)
    char *arg, *key;
{
    char msg[100];

    sprintf (msg, "%s: illegal value for '%s'", arg, key);
    G_fatal_error (msg);
}
