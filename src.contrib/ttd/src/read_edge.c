#include <stdio.h>
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
	int j ;
	char *ptr ;
	char *new_record ;

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
	n_coors = i ;

/* Read first IDLS record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "IDLS", 4) != 0)
	{
		printf("Expecting first IDLS record for EDGE data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}

	change(new_record, length, 037, 040)  ;
	ptr = new_record+9 ;
	for(i=0; ; i++)
	{
		sscanf(ptr,"%d", &pos_line_pointers[i]) ;
		if (i<pos_line_feat_comp_count)
			while (*(++ptr) != ' ')
				;
		else
			break ;
	}

/* Read second IDLS record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "IDLS", 4) != 0)
	{
		printf("Expecting second IDLS record for EDGE data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}

	ptr = new_record+9 ;
	for(i=0; ; i++)
	{
		sscanf(ptr,"%d", &neg_line_pointers[i]) ;
		if (i<neg_line_feat_comp_count)
			while (*(++ptr) != ' ')
				;
		else
			break ;
	}

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
		sscanf(ptr,"%lf %lf %lf",
			&xcoors[i], &ycoors[i], &zcoors[i]) ;
		if (i == coor_count-1)
			break ;
		for(j=0; j<3;)
			if (*(++ptr) == ' ') j++ ;
	}
#ifdef DEBUG
	write_EDGI() ;
#endif
}

write_EDGI()
{
	int j ;
	int i ;

	printf("EDGE %d %d:\n", record_type, record_id) ;
	printf("  Bounding rectangle:\n") ;
	printf("     xmin:%d ymin:%d zmin:%d\n", xmin, ymin, zmin) ;
	printf("     xmax:%d ymax:%d zmax:%d\n", xmax, ymax, zmax) ;
	printf("  Positive direction:\n") ;
	printf("     ID:%d  #-Feature-Components:%d\n",
		positive_dir_id,
		pos_line_feat_comp_count) ;
	printf("     left face:%d  next DE left:%d end node:%d\n",
		left_face,
		next_DE_left,
		end_node) ;
	printf("     line ID list:\n       ") ;
	for(i=0, j=0; i<pos_line_feat_comp_count; i++, j++)
	{
		if (j==10)
		{
			printf("\n       ") ;
			j=0 ;
		}
		printf("%d  ", pos_line_pointers[i]) ;
	}
	printf("\n") ;
	printf("  Negative direction:\n") ;
	printf("     ID:%d  #-Feature-Components:%d\n",
		negative_dir_id,
		neg_line_feat_comp_count) ;
	printf("     left-face:%d  next-DE-rite:%d start-node:%d\n",
		right_face,
		next_DE_right,
		start_node) ;
	printf("     line ID list:\n       ") ;
	for(i=0, j=0; i<neg_line_feat_comp_count; i++, j++)
	{
		if (j==10)
		{
			printf("\n       ") ;
			j=0 ;
		}
		printf("%d  ", neg_line_pointers[i]) ;
	}
	printf("\n") ;
	printf("  Coordinate count:%d\n", coor_count) ;
}
