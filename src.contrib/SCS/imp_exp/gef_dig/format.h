/*  @(#)format.h	2.1  6/26/87  */

/*  do they want bounding box (new format), no bounding box is old format.  */
int	new_format ;
int	cat_cnt ;
int 	gef_format ;

struct bdig_head
{
	char organization[30] ;
	char date[20] ;
	char your_name[20] ;
	char map_name[41] ;
	char source_date[16] ;
	char area[30];
	char in_proj[10];
	char out_proj[10];
	double orig_scale ;
	char other_info[73] ;
	int plani_zone, st, cnty, stzone ;
	double W, E, S, N ;
	double digit_thresh ;
	double map_thresh ;
	double xofset, yofset;
};

