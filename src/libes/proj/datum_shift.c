#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "projects.h"

/* for datum conversion */
int (*proj_f)(double *, double *, struct pj_info *, struct pj_info *);

void set_datumshift(
	char *in_datum, 
	char *in_ellipse, 
	char *out_datum,
	char *out_ellipse)
{
	if(strcmp(in_datum,out_datum)!=0)
	{
		if(!strcmp(in_datum,"nad83") && !strcmp(out_datum,"nad27"))
		{
		    fprintf(stderr,"Shifting NAD83->NAD27\n");
		    if(strcmp(in_ellipse,"grs80")!=0)
		        G_warning("Ellipse for nad83 should be grs80");
		    if(strcmp(out_ellipse,"clark66")!=0)
		        G_warning("Ellipse for nad27 should be clark66");

		    INVERSE_FLAG=1;
		    CONVERSION_TABLE=nad_init("/usr/local/grass5/etc/nad/conus");
		    proj_f=pj_do_proj_nad;
		    return;
		}
		else if(!strcmp(in_datum,"nad27") && !strcmp(out_datum,"nad83"))
		{
		    fprintf(stderr,"Shifting NAD27->NAD83\n");
		    if(strcmp(in_ellipse,"clark66")!=0)
		        G_warning("Ellipse for nad27 should be clark66");
		    if(strcmp(out_ellipse,"grs80")!=0)
			G_warning("Ellipse for nad83 should be grs80");
		    INVERSE_FLAG=0;
		    CONVERSION_TABLE=nad_init("/usr/local/grass5/etc/nad/conus");
		    proj_f=pj_do_proj_nad;
		    return;
		}
		else
		{
		/* some other datum shift */
		    proj_f=pj_do_proj;
		    return;
		}

	}

	/* no datum shift */
	proj_f=pj_do_proj;
	return;
}
