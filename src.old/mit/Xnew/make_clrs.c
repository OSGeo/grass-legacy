#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include "gis.h"

extern Display* the_display;
extern int the_screen;
Colormap mycolormap;

static char *vect_colrs[] = 
	{"red", "orange", "yellow", "green", "blue",
	"violet", "white", "black", "gray", "brown",
	"magenta", "cyan", "gold", "khaki", 
	"turquoise", "pink"};


extern char buf[];

#define NVECTCOLRS	16 
#define	UNTOUCHED 	24	

Colormap make_fixed_colormap(mycolormap, no_cells)
	Colormap mycolormap;
	int no_cells;
{
	Colormap  def;
	int limit, side;
	int i, status;
	int red, green, blue;
	float incr;
	XColor *color, screen, exact, ans;
	char *calloc();

	/* first 24 cells from default existing at creation */
	limit = no_cells - NVECTCOLRS - UNTOUCHED;

	side = 1;
	while(1)
	{
	if(limit < (side * side * side))
		{
		side--;
		break;
		}
	side++;
	}		/* end of while */



	color = (XColor *)calloc(no_cells, sizeof(XColor));

	def = DefaultColormap(the_display, the_screen);

	for(i = 0; i < (UNTOUCHED + NVECTCOLRS) ; i++)
	{
	ans.pixel = (unsigned long) i;
	XQueryColor(the_display, def, &ans);
	color[i].pixel = (unsigned long) i; 
	color[i].red   = ans.red;
        color[i].green = ans.green;
        color[i].blue  = ans.blue;
        color[i].flags = DoRed | DoGreen | DoBlue;
	}


	incr = 65535.0/(side - 1) ;


	i = UNTOUCHED + NVECTCOLRS;
	for(red = 0; red < side; red++)
	{
	  for(green = 0; green < side; green++)
	  {
 	    for(blue = 0; blue < side; blue++)	
	    {
	color[i].pixel = (unsigned long) i;  
	color[i].red   = (short) (red * incr);
	color[i].green = (short) (green * incr);
	color[i].blue  = (short) (blue * incr);
	color[i].flags = DoRed | DoGreen | DoBlue;

	i++;
	    }
	  }
	} 

	XStoreColors(the_display, mycolormap,
		color, no_cells);
	
	free (color);

	return (mycolormap);
}	/* end */ 



/*___________________________________________________*/


Colormap load_vect_colrs(clrmap)
	Colormap clrmap;
{
	int i;

	/* store vector colors for float mode */
        for(i = 0;  i <  NVECTCOLRS ; i++)
        {
        XStoreNamedColor(the_display, clrmap,
                vect_colrs[i],
                (unsigned long) (UNTOUCHED + i),
                DoRed | DoGreen | DoBlue);
        }

	return(clrmap);
}

/*___________________________________________________*/


Colormap make_float_clr_table(clrmap, name)
        Colormap clrmap;
        char *name;
{
        struct Colors colors;
        XColor color;
        unsigned char *red, *grn, *blu;
        char* mapset;
        unsigned long pix_list[256];
        int i, total, index, x;

        mapset =  G_find_cell2(name, "");

       if(mapset == NULL)
       {
          sprintf(buf,"Cellfile [%s] not available", name);
          G_fatal_error(buf);
       }

        /* Set the colors for the display */
        if (G_read_colors(name, mapset, &colors) == -1)
        {
        G_fatal_error("Color file not available");
        }


        red = colors.red;
        grn = colors.grn;
        blu = colors.blu;

        color.flags = DoRed | DoGreen | DoBlue;

        total = 0;

	color.pixel = NVECTCOLRS + UNTOUCHED;
        color.red = colors.r0 * colors.r0;
        color.green = colors.g0 * colors.g0;
        color.blue = colors.b0 * colors.b0;

	XStoreColor(the_display, clrmap, &color);

        for(i = colors.min; i<=colors.max; i++)
        {
        if (i < 0) continue;

        if(total == 215) break;

        x = (unsigned long) (i % 216);

	if(x == 0) continue;

        index = i - colors.min;

        color.pixel = NVECTCOLRS + UNTOUCHED + x;
        color.red = red[index] * red[index];
        color.green = grn[index] * grn[index];
        color.blue = blu[index] * blu[index];

        XStoreColor(the_display, clrmap, &color);
        total++;
        }

        return(clrmap);
}
