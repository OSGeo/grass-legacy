/*  @(#)dig.h	2.1  6/26/87  */
struct dig_head
{
	char organization[30] ;
	char date[20] ;
	char your_name[20] ;
	char map_name[41] ;
	char source_date[11] ;
	int  orig_scale ;
	char line_3[73] ;
	int plani_zone ;
	double W, E, S, N ;
	double digit_thresh ;
	double map_thresh ;
} ;

