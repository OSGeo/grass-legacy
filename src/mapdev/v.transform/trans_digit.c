
/*
*  Most of this is from the ../src/mapdev programs to convert
*  ascii digit files to binary and vice versa.
*
*  Written by the GRASS Team, 02/16/90, -mh.
*/

#include "Vect.h"

transform_digit_file(current_file, new_file)
	FILE *current_file, *new_file ;
{
	char ctype ;
	char buff[128] ;
	char *fgets() ;
	double x, y ;
	double new_x, new_y ;
	int end_of_file ;

	end_of_file = 0 ;

	if (NULL == fgets(buff,128,current_file))
	    return ;

	for(;;)
	{
		sscanf(buff, "%1c", &ctype) ;
		switch(ctype)
		{
		case 'A':
		case 'L':
		case 'P':
		case 'a':
		case 'l':
		case 'p':
	    		fprintf(new_file, "%s", buff);
			break ;

		case 'E': case 'e':
			return ;
		default:
			printf("ERROR:  Unknown type, don't know what this line means: ?? %s\n", buff) ;
			printf("Giving it up!\n") ;
			exit(-1) ;
		}

	/* Collect the points */
		if (NULL == fgets(buff,128,current_file))
		    break;

		for(;;)
		{
			char buf1[100], buf2[100];

			sscanf(buff, "%1c%lf%lf", &ctype, &y, &x) ;

			/*  beginning of a new line */
			if (ctype != ' ')
				break ;

			transform_a_into_b( x, y, &new_x, &new_y ) ;

			/*  8/92  dpg
	    		fprintf(new_file, " %12.2lf %12.2lf\n", new_y, new_x) ;
			*/

			G_format_northing (new_y, buf1, -1);
			G_format_easting (new_x, buf2, -1);
			fprintf(new_file, " %-12s %-12s\n", buf1, buf2);
			
			if (NULL == fgets(buff,128,current_file) )
			{
				end_of_file = 1 ;
				break ;
			}
		}

		if (end_of_file)
			return ;
	}
}


