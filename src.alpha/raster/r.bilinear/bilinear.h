#define		WEST	0
#define		EAST	1

struct Cell_head    in_region, out_region;

    int 	verbose;
	int		npole_set, spole_set;	/* argument use indicators */
    char 	name[100], *mapset;
    char 	result[100];
    CELL 	npole, spole;
    int 	infd, outfd, maskfd;
	double	in_north, in_south, out_north;
	int 	in_rows, in_cols;
    int 	out_row,  				/* next row to write to output raster */
			out_rows, out_cols;
