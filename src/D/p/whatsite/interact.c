#include <stdio.h>
#define MAX_FIELDS	128
#include "sites.h"
#include "options.h"

static char *list[128] ;
static struct field
{
	int row;
	int col;
	int len;
} field[MAX_FIELDS];

interact(mapset, name)
	char *mapset, *name ;
{
	FILE *fd ;
	long get_table_entry() ;
	char *record[128] ;
	char buff[128] ;
	char data[128] ;
	char format[128] ;
	double n, e ;
	double min_dist ;
	int f ;
	int fields ;
	int nfields ;
	int num ;
	int row, col ;
	int table ;
	int screen_x, screen_y ;
	int button ;
	int cur_screen_x, cur_screen_y ;
	double fabs() ;
	D_d_to_u_col() ;
	D_d_to_u_row() ;

/* establish names */
	sprintf(data, "%s/%s/%s/%s", 
		G_getenv("GISDBASE"),
		G_getenv("LOCATION_NAME"),
		mapset,
		"site") ;

	sprintf(format, "%s/%s/%s/%s/%s", 
		G_getenv("GISDBASE"),
		G_getenv("LOCATION_NAME"),
		mapset,
		"site_format",
		name) ;

	Qopen();
	Qforms(0);

	if (get_field_list(data, name, list, &nfields))
	{
		sprintf(buff, "Reading fields, db: %s layer: %s\n", data, name) ;
		G_fatal_error(buff) ;
	}

/* Show the standard form */
	if ( ! ( fd = fopen (format, "r"))) 
	{
		Qclose();
		perror (format);
		exit(1);
	}

	f = 0 ;
	for (; fgets(buff, sizeof buff, fd);)
	{
		if (*buff == 't')        /* have a text record */
		{
			sscanf(buff,"%*s %d %d %d", &row, &col, &fields) ;
			if (fields > nfields)
			{
				printf("Field referenced in format file that doesn't exist in data\n") ;
				exit(-1) ;
			}
			Qtext (row, col, list[fields-1]) ;
		}
		else if (*buff == 'f')   /* have a field record */
		{
			sscanf(buff,"%*s %d %d %d %d", 
				&field[f].row,
				&field[f].col,
				&field[f].len,
				&num ) ;
			if (++f != num)
			{
				Qclose ();
				fprintf(stderr,
					"Error in format file.  Fields not numbered consecutively.\n") ;
				exit(1) ;
			}
		}
	}

	fclose (fd);

	nfields = f;
	if (nfields <= 0) 
	{
		Qclose ();
		fprintf(stderr,"%s: no fields\n", format);
		exit(1);
	}

	if ((table = open_table(data, name, 0)) < 0)
	{
		Qclose ();
		sprintf(buff,"Error in opening table (%d)\n", table) ;
		G_fatal_error(buff) ;
	}

/* put cursor into middle of screen first time around */
	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;
	do
	{
		int i ;
		int site ;
		double mindist, newdist ;
		double east, north ;

		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		east = D_d_to_u_col((double)screen_x) ;
		north = D_d_to_u_row((double)screen_y) ;
		mindist = 999999999. ;
		site = -1 ;
		for (i=0; i<nsites; i++)
		{
			newdist=(fabs(east-sites[i].e)+fabs(north-sites[i].n)) ;
			if (newdist > mark_size)
				break ;
			if (newdist < mindist)
			{
				mindist = newdist ;
				site = i ;
			}
		}
		if (site == -1 )
			break ;
		
		if (-1 == get_table_entry(table, record, sites[site].offset))
			break ;

		for(f=0; f<nfields; f++)
		{
			Qsetfield(field[f].row, field[f].col, field[f].len);
			/* strip (buff); */
			Qput (record[f]);
		}
		Qflush() ;
		sscanf(record[1],"%lf",&e) ;
		sscanf(record[2],"%lf",&n) ;
		R_color(color2) ;
		mark_site(n, e) ;
		R_flush() ;
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		R_color(color1) ;
		mark_site(n, e) ;
		R_flush() ;
	} while (button != 3) ;

	fclose (fd);
	Qclose ();
}
