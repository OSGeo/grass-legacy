#include <stdio.h>


FILE * get_fileptr ();

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buffer[1024] ;
	char *buf_ptr ;
	int lat_d, lat_m, lat_s ;
	int lon_d, lon_m, lon_s ;
	double lat, lon ;
	char lat_type, lon_type ;
	int n_points ;
	int line_segment_no ;
	int rank ;
	FILE *fp;

	if(argc != 2)
	{
		fprintf(stderr,"Usage: %s area_name\n", argv[0]) ;
		exit(-1) ;
	}


	init_arrays ();
	while(1)
	{
		if (NULL == gets(buffer))
			exit(0) ;

		for(buf_ptr=buffer; *buf_ptr; buf_ptr++)
		    if(*buf_ptr == ' ')
			*buf_ptr = '0' ;

		sscanf(buffer,"%7d%2d%6d", &line_segment_no, &rank, &n_points) ;

		fp = get_fileptr (argv[0], rank);
		fprintf(fp,"L %d\n", n_points) ;

		while(n_points--)
		{
			if (NULL == gets(buffer))
			{
				sprintf(stderr, "Unexpected end of file\n") ;
				exit(-1) ;
			}
			for(buf_ptr=buffer; *buf_ptr; buf_ptr++)
				if(*buf_ptr == ' ') *buf_ptr = '0' ;

			sscanf(buffer,"%2d%2d%2d%1c%3d%2d%2d%1c",
				&lat_d, &lat_m, &lat_s, &lat_type,
				&lon_d, &lon_m, &lon_s, &lon_type) ;

			lat = (double)lat_d + (double)lat_m/60. + (double)lat_s/3600 ;
			if (lat_type == 'S') lat = -lat ;
			lon = (double)lon_d + (double)lon_m/60. + (double)lon_s/3600 ;
			if (lon_type == 'W') lon = -lon ;

			fprintf(fp, " %.6lf %.6lf\n", lat, lon) ;
		}
	}
}

#define RANK_LIMIT 50
static struct ranks {
    int num;
    FILE *fp;
} ranks[RANK_LIMIT];


init_arrays ()
{
    int i;

    for (i = 0 ; i < RANK_LIMIT ; i++)
	ranks[i].num = 0;
}

FILE *
get_fileptr (name, rank)
    char *name;
    int rank;
{
	FILE *fp;
	char filenam[100];

	if (ranks[rank].num)
	    return (ranks[rank].fp);
	else
	{
	    sprintf (filenam, "%s.%02d", name, rank);
	    if (NULL == (fp = fopen (filenam, "w")))
	    {
		fprintf (stderr, "Could not open file '%s' for write\n", filenam);
		exit (-1);
	    }
	    digit_header(fp, name, rank) ;
	    ranks[rank].num = 1;
	    return (fp);
	}
}
