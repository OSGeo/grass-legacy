/*  @(#)rw_ascii.c	2.1  6/26/87  */
#include <stdio.h>
#include "head.h"

/*
*   2.0 digit files have "UTM ZONE",  3.0 digit files have "ZONE"
*/

write_head_ascii(dascii)
	FILE *dascii ;
{

	fprintf(dascii, "ORGANIZATION: %s\n", head.organization) ;
	fprintf(dascii, "DIGIT DATE:   %s\n", head.date) ;
	fprintf(dascii, "DIGIT NAME:   %s\n", head.your_name) ;
	fprintf(dascii, "MAP NAME:     %s\n", head.map_name) ;
	fprintf(dascii, "MAP DATE:     %s\n", head.source_date) ;
	fprintf(dascii, "MAP SCALE:    %d\n", head.orig_scale) ;
	fprintf(dascii, "OTHER INFO:   %s\n", head.line_3) ;
	fprintf(dascii, "ZONE:         %d\n", head.plani_zone) ;
	fprintf(dascii, "WEST EDGE:    %12.2lf\n", head.W) ;
	fprintf(dascii, "EAST EDGE:    %12.2lf\n", head.E) ;
	fprintf(dascii, "SOUTH EDGE:   %12.2lf\n", head.S) ;
	fprintf(dascii, "NORTH EDGE:   %12.2lf\n", head.N) ;
	fprintf(dascii, "MAP THRESH:   %12.2lf\n", head.map_thresh) ;
	fprintf(dascii, "VERTI:\n") ;

	return(0) ;
}

read_head_ascii(dascii)
	FILE *dascii ;
{
	char buff[1024] ;
	char *ptr ;
	char *rindex() ;

	for(;;)
	{
		fgets(buff, sizeof(buff), dascii) ;
		for(ptr=buff; *ptr!='\n'; ptr++) ;   /* Remove new-line char */
		*ptr = NULL ;

		if (strncmp(buff, "VERTI:", 6) == 0 )
			return(0) ;

		if ( ! (ptr = rindex(buff,':')) )
			return(-1);
		ptr++ ;                 /* Search for the start of text */
		while (*ptr == ' ')
			ptr++ ;

printf("%s\n", ptr) ;
		if (strncmp(buff, "ORGANIZATION:", 12) == 0)
		{
			if (strlen(ptr) >= sizeof(head.organization))
				ptr[sizeof(head.organization)-1] = 0;
			strcpy( head.organization, ptr) ;
		}
		else if (strncmp(buff, "DIGIT DATE:  ", 12) == 0)
		{
			if (strlen(ptr) >= sizeof(head.date))
				ptr[sizeof(head.date)-1] = 0;
			strcpy( head.date, ptr) ;
		}
		else if (strncmp(buff, "DIGIT NAME:  ", 12) == 0)
		{
			if (strlen(ptr) >= sizeof(head.your_name))
				ptr[sizeof(head.your_name)-1] = 0;
			strcpy( head.your_name, ptr) ;
		}
		else if (strncmp(buff, "MAP NAME:    ", 12) == 0)
		{
			if (strlen(ptr) >= sizeof(head.map_name))
				ptr[sizeof(head.map_name)-1] = 0;
			strcpy( head.map_name, ptr) ;
		}
		else if (strncmp(buff, "MAP DATE:    ", 12) == 0)
		{
			if (strlen(ptr) >= sizeof(head.source_date))
				ptr[sizeof(head.source_date)-1] = 0;
			strcpy( head.source_date, ptr) ;
		}
		else if (strncmp(buff, "MAP SCALE:   ", 12) == 0)
			sscanf(ptr, "%d", &head.orig_scale) ;
		else if (strncmp(buff, "OTHER INFO:  ", 12) == 0)
		{
			if (strlen(ptr) >= sizeof(head.line_3))
				ptr[sizeof(head.line_3)-1] = 0;
			strcpy( head.line_3, ptr) ;
		}
		else if (strncmp(buff, "ZONE:        ", 12) == 0
		      || strncmp(buff, "UTM ZONE:    ", 12) == 0)
			sscanf(ptr, "%d", &head.plani_zone) ;
		else if (strncmp(buff, "WEST EDGE:   ", 12) == 0)
			sscanf(ptr, "%lf", &head.W) ;
		else if (strncmp(buff, "EAST EDGE:   ", 12) == 0)
			sscanf(ptr, "%lf", &head.E) ;
		else if (strncmp(buff, "SOUTH EDGE:  ", 12) == 0)
			sscanf(ptr, "%lf", &head.S) ;
		else if (strncmp(buff, "NORTH EDGE:  ", 12) == 0)
			sscanf(ptr, "%lf", &head.N) ;
		else if (strncmp(buff, "MAP THRESH:  ", 12) == 0)
			sscanf(ptr, "%lf", &head.map_thresh) ;
		else
			return(-1) ;
	}
}
