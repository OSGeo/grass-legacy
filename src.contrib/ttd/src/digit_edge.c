#include <stdio.h>
#include "header.h"

static int record_type ;
static int record_id ;
static int xmin, ymin, zmin ;
static int xmax, ymax, zmax ;
static int positive_dir_id ;
static int pos_line_feat_comp_count ;
static int left_face ;
static int next_DE_left ;
static int end_node ;
static int negative_dir_id ;
static int neg_line_feat_comp_count ;
static int right_face ;
static int next_DE_right ;
static int start_node ;
static int coor_count ;
static int *pos_line_pointers ;
static int *neg_line_pointers ;
static double *xcoors ;
static double *ycoors ;
static int *zcoors ;
static int n_pos_line_pointers = 0 ;
static int n_neg_line_pointers = 0 ;
static int n_coors = 0 ;

read_EDGE(record, length)
	char *record ;
	int length ;
{
	int i ;
	char *new_record ;
	char *ptr ;

/* Read information out of EDGI record */
	change(record, length, 037, 040)  ;

	i = sscanf(record, "%*9c%1d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d",
		&record_type,
		&record_id,
		&xmin, &ymin, &zmin,
		&xmax, &ymax, &zmax,
		&positive_dir_id,
		&pos_line_feat_comp_count,
		&left_face,
		&next_DE_left,
		&end_node,
		&negative_dir_id,
		&neg_line_feat_comp_count,
		&right_face,
		&next_DE_right,
		&start_node,
		&coor_count) ;
	if(i!=19)
	{
		printf("Error parsing EDGI record:\n") ;
		printf("%s\n", record) ;
		exit(-1) ;
	}
    
	check_alloc(&n_pos_line_pointers,
		pos_line_feat_comp_count,
		sizeof(int),
		&pos_line_pointers) ;
	check_alloc(&n_neg_line_pointers,
		neg_line_feat_comp_count,
		sizeof(int),
		&neg_line_pointers) ;
	i = n_coors ;
	check_alloc(&i, coor_count, sizeof(double), &xcoors) ;
	i = n_coors ;
	check_alloc(&i, coor_count, sizeof(double), &ycoors) ;
	i = n_coors ;
	check_alloc(&i, coor_count, sizeof(int), &zcoors) ;

/* Read first IDLS record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "IDLS", 4) != 0)
	{
		printf("Expecting first IDLS record for EDGE data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}

	for(i=0; i<pos_line_feat_comp_count; i++)
		sscanf(new_record + 6 + i * 5,"%d", &pos_line_pointers[i]) ;

/* Read second IDLS record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "IDLS", 4) != 0)
	{
		printf("Expecting second IDLS record for EDGE data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}

	for(i=0; i<neg_line_feat_comp_count; i++)
		sscanf(new_record + 6 + i * 5,"%d", &neg_line_pointers[i]) ;

/* Read COOL record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "COOL", 4) != 0)
	{
		printf("Expecting COOL record for EDGE data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}

	change(new_record, length, 037, 040)  ;

	ptr = new_record + 9 ;
	i=0 ;
	for(i=0;;i++)
	{
		int j ;
		sscanf(ptr,"%lf%lf%d",
			&xcoors[i], &ycoors[i], &zcoors[i]) ;
		if (i == coor_count-1)
			break ;
		for(j=0; j<3;)
			if (*(++ptr) == ' ') j++ ;
	}
}

/* Converts edge TTD coordinates to coordinates in seconds */
convert_EDGE()
{
	int i ;
	double sixty_thousandths ;
	int minutes ;
	double lat, lon ;
	static int zone = 0 ;

	for(i=0; i<coor_count; i++)
	{
		minutes = ycoors[i] / 100000 ;
		sixty_thousandths = ycoors[i] - minutes * 100000. ;
		lat = DSP.latitude_of_origin_in_sec +
			60. * minutes + sixty_thousandths / 1000.;

		minutes = xcoors[i] / 100000 ;
		sixty_thousandths = xcoors[i] - minutes * 100000. ;
		lon = DSP.longitude_of_origin_in_sec -
			(60. * minutes + sixty_thousandths / 1000.);

		CC_ll2u (lat, lon, &xcoors[i], &ycoors[i], &zone) ;
	}
}

digit_EDGE()
{
	int i ;

	coor_count = prune (xcoors, ycoors, coor_count, 5.0) ;

	printf("L %d\n", coor_count) ;
	for(i=0; i<coor_count; i++)
		printf(" %.2lf %.2lf\n", ycoors[i], xcoors[i]) ;
}
