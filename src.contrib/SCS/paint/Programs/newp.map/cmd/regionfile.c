#include "gis.h"
#include "misc.h"
#include "regionline.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color  color",
    "width  #",
	"hcolor	color",
	"hwidth #",
	"masked	[y|n]",
	"style  solid|[0-9]",
    ""
};

regionfile (name,mapset)
    char *name;
    char *mapset;
{
	FILE	*fd;
    char fullname[100];
    char buf[1024];
	char temp[30];
    char *key, *data, *dp;
    int width;
	int colors[9];
	int color;
	int hcolor;
	int hwidth;
	int masked;
    int r,g,b;
    int i;
	char linestyle[30];
    double east, west, incr;
    struct Cell_head window;

    sprintf (fullname, "%s in %s", name, mapset);

    if (G__get_window (&window, "windows", name, mapset) < 0)
    {
	error (fullname,"","can't read region definition file");
	gobble_input();
	return 0;
    }

    width  = 1 ;


	strcpy (linestyle, "NULL");
	for (i=0; i<9; i++)
	colors[i]	= BLACK;

	width	= 1;
	hcolor	= WHITE;
	hwidth	= 0;
	masked	= 0;




    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("width"))
	{
	    width = -1;
	    if (sscanf (data, "%d%s", &width, mapset) != 1 || width < 1)
	    {
		width = 1;
		error (key,data,"illegal width");
	    }
	    continue;
	}

	if (KEY("color"))
	{
	if (sscanf (data, "%d%[^\n]", &i, temp) == 2)
	{
	if (i>=1 && i<=9 && scan_color (temp, &color, &r, &g,&b))
	{
		colors[i-1]	= color;
		continue;
	}
	}

	if (!scan_color (data, &color, &r, &g, &b)) {
		error (key, data, "illegal color request");
		continue;
	}

	for (i=0; i<9; i++)
		colors[i]	= color;

	continue;
	}

	if (KEY("hcolor"))
	{
	if (!scan_color (data,&color,&r,&g,&b)) {
		error (key, data, "illegal hcolor request");
		continue;
	}

	hcolor	= color;
	if (!hwidth) hwidth	= 1;
	continue;
	}


	if (KEY("hwidth"))
	{
	hwidth	= -1;
	if (sscanf (data, "%d", &hwidth) != 1 || width< 1)
	{
		error(key, data, "illegal hwidth");
	}
	hwidth	= hwidth;
	continue;
	}

	if (KEY("masked"))
	{
		masked= yesno(key, data);
		continue;
	}


	if (KEY("style"))
	{
	G_strip (data);
	if (strcmp (data, "solid") == NULL)
	{

	continue;
	}

	for (dp = data; *dp; dp++)
	if (*dp < '0' || *dp > '9')
		break;
	if (*dp != 0 || dp == data)
	{
		error(key, data, "illegal line style");
		continue;
	}

	strcpy (linestyle, data);
	continue;
	}




	error (key,"","illegal request");
    }


	if (regline.other== NULL)
	{
		regline.other = (char *) G_tempfile();
		if ( (fd = fopen(regline.other, "w")) != NULL)
			 fclose(fd);
	
	}

	fd = fopen (regline.other, "a");
	if (fd == NULL)
	{
	error ("misc regline.other", "", "can't open");
	return;
	}


	for (i=0; i<9; i++)
	fprintf(fd, "color:%d\n", colors[i]); 

	fprintf (fd, "width:%d\n", width);
	fprintf (fd, "hwidth:%d\n", hwidth);
	fprintf (fd, "hcolor:%d\n", hcolor);
	fprintf (fd, "style:%s\n", linestyle);
	





/* draw horizontal lines in 3 pieces - lat-lon lines must not
 * extend more than half the globe
 */
    west = window.west;
    incr = (window.east-window.west)/3;
    for (i = 0; i < 3; i++)
    {
	east = west+incr;
	sprintf (buf, "L: %d %lf %lf %lf %lf",
	    masked, west, window.north, east, window.north);

	fprintf (fd, "%s\n", buf);

	sprintf (buf, "L: %d %lf %lf %lf %lf",
	    masked, west, window.south, east, window.south);

	fprintf (fd, "%s\n", buf);

		/*
	sprintf (buf, "L 0 %lf %lf %lf %lf %d %d",
	    west, window.north, east, window.north,
	    color, width);
	add_to_plfile (buf);

	sprintf (buf, "L 0 %lf %lf %lf %lf %d %d",
	    west, window.south, east, window.south,
	    color, width);
	add_to_plfile (buf);
*/

	west = east;
    }

    sprintf (buf, "L: %d %lf %lf %lf %lf",
	masked, window.east, window.north, window.east, window.south);
	fprintf (fd, "%s\n", buf);
	/*
    sprintf (buf, "L 0 %lf %lf %lf %lf %d %d",
	window.east, window.north, window.east, window.south,
	color, width);
	/*
    add_to_plfile (buf);
	*/

    sprintf (buf, "L: %d %lf %lf %lf %lf",
	masked, window.west, window.north, window.west, window.south);
	fprintf (fd, "%s\n", buf);
	/*
    sprintf (buf, "L 0 %lf %lf %lf %lf %d %d",
	window.west, window.north, window.west, window.south,
	color, width);
    add_to_plfile (buf);
	*/

	fprintf (fd, "end:\n");

	fclose (fd);
    return 1;
}
