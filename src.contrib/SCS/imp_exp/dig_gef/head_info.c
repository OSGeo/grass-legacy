/*  @(#)head_info.c	1.0  2/26/90  */
#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "export_gef.h"

get_head_info(ghead)
    struct ghead *ghead;
{
        char buf[100];
	int  i, ier, repeat = 1   ;
	char answer[10], *gets() ;

	strcpy(ghead->map_name, Map.head.map_name);
	strcpy(ghead->organization, "USDA, SCS");
	strcpy(ghead->source_date, Map.head.source_date );
	strcpy(ghead->data_typ, "SSURGO");

	ghead->orig_scale = Map.head.orig_scale;
	ghead->plani_zone = Map.head.plani_zone;

	while (repeat)
	{
	V_clear() ;
	ier = 0;
	V_line(1,"Provide the following information:") ;

	V_line(3,"Data Set Name") ;
	V_line(4,"Digitizing Scale        1:") ;
	V_line(5,"St.abbr. & Area Name") ;
	V_line(6,"Organization") ;
	V_line(7,"Date of Digitizing") ;
	V_line(8,"Data type (ie. SSURGO)") ;

	V_ques( ghead->map_name,     's', 3,  24, 42) ;
	V_ques( &ghead->orig_scale,  'i', 4,  26, 9) ;
	V_ques( ghead->area_name,    's', 5,  24, 32) ;
	V_ques( ghead->organization, 's', 6,  24, 32) ;
	V_ques( ghead->source_date,  's', 7,  24, 11) ;
	V_ques( ghead->data_typ,     's', 8,  24, 17) ;


	V_intrpt_ok();	
	if (!V_call())
		exit(1) ;
	V_clear();

	/*   Check values */

	if (strlen(ghead->map_name) == 0)
		{
		fprintf(stderr,"\n\n\tA Data Set Name is required.\n");
		ier = 1;
		}

	if (ghead->orig_scale == 0)
		{
		fprintf(stderr,"\n\n\tA digitized scale is required.\n");
		ier = 1;
		}

	if (strlen(ghead->area_name) == 0)
		{
		fprintf(stderr,"\n\n\tAn Area Name is required.\n");
		ier = 1;
		}

	if (strlen(ghead->organization) == 0)
		{
		fprintf(stderr,"\n\n\tAn organization name is required.\n");
		ier = 1;
		}

	if (strlen(ghead->source_date) == 0)
		{
		fprintf(stderr,"\n\n\tA source date is required.\n");
		ier = 1;
		}

	if (strlen(ghead->data_typ) == 0)
		{
		fprintf(stderr,"\n\n\tA Data type is required.\n");
		ier = 1;
		}

	/*   
             Check for error flag */
	if (ier > 0)
		{
		fprintf(stderr,"\n\t\tHit RETURN -->");
		gets(answer);
		}
	if (ier == 0) repeat = 0;

	}
	/*
	     got changes, ok   */
        return;

}
