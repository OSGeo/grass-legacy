
struct bdig_head
{
	char organization[30] ;
	char date[20] ;
	char your_name[20] ;
	char map_name[41] ;
	char source_date[11] ;
	long  orig_scale ;
	char line_3[73] ;
	int plani_zone ;
	double W, E, S, N ;
	double digit_thresh ;
	double map_thresh ;
};

extern struct head head;
extern double sample_thresh ;

