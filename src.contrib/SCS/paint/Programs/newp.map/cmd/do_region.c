#include "regionline.h"
#include "gis.h"

#define FIELD(x) strcmp(x,field)==0


do_region(after_masking)
{
	
    FILE *fd;
	char	field[1024];
	char 	value[1024];
	char	buf[1024];
	int i;
	int clrct;
	double e1, e2, n1, n2;
	int	masked;

	clrct	= 0;


	fd = fopen(regline.other, "r");	
	if (fd == NULL)
	{
		return;
	}


	while (fgets (buf, sizeof buf, fd)) 
	{ 
	*value	= 0;
	*field	= 0;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;

	if (FIELD("width"))
	{
		dline.width	= atoi(value);
		continue;
	}

	if (FIELD("hwidth"))
	{
		dline.hwidth	= atoi(value);
		continue;
    }

	if (FIELD("hcolor"))
	{
		dline.hcolor	= which_color(value);
		continue;
	}

	if (FIELD("color")) {
		dline.colors[clrct] = which_color(value);
		clrct++;
		continue;
	}



	if (FIELD("style")) {
		G_strip(value);
		if ( (strcmp (value, "solid")== 0) || 
			(strcmp (value, "NULL") == 0) ){
		dline.linestyle = NULL;
		continue;
		}
		dline.linestyle	= G_store(value);
		continue;
	}


	if (FIELD("L"))
	{
	if (sscanf (buf, "L: %d %lf %lf %lf %lf",
			&masked, &e1, &n1, &e2, &n2) == 5)
	{
		if (masked && after_masking) continue;
		if (!masked && !after_masking) continue;
		drwline(e1, n1, e2, n2);
	}
		continue;

	}

	if (FIELD("end"))
	{
		clrct = 0;
		continue;

	}



}

	fclose(fd);

}






static
which_color (value)
char *value;
{
int n;
int r,g,b;

if (!scan_color (value, &n,&r,&g,&b))
n = -1;
return n;
}


