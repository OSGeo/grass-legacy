#ifdef MAIN
#  define EXTERN
#else
#  define EXTERN extern
#endif

/*
EXTERN double x ;
EXTERN double y ;
EXTERN int n_lines ;
EXTERN int n_atts ;
EXTERN int n_isles ;
EXTERN int *lines ;
EXTERN int *atts ;
EXTERN int start_node ;
EXTERN int end_node ;
EXTERN int left_area ;
EXTERN int right_area ;
EXTERN int n_coors ;
EXTERN int n_atts ;
*/



EXTERN int nlines ;
EXTERN char banner[73] ;
EXTERN char cart_unit[41] ;
EXTERN char source_date[11] ;
EXTERN char orig_scale[9] ;
EXTERN char line_3[73] ;
EXTERN int level_code ;
EXTERN int plani_code ;
EXTERN int plani_zone ;
EXTERN int plani_units ;
EXTERN double resolution ;
EXTERN int trans_param ;
EXTERN int misc_records ;
EXTERN int num_sides ;
EXTERN int num_cats ;

EXTERN struct
{
	char corner[3] ;
	double lat ;
	double lon ;
	double utm_n ;
	double utm_e ;
} coors[4] ;

EXTERN double params[15] ;
EXTERN double int_params[4] ;

EXTERN struct 
{
	int read ;
	char name[21] ;
	int form_code ;
	int num_nodes ;
	int act_nodes ;
	int nta_link ;
	int ntl_link ;
	int num_areas ;
	int act_areas ;
	int atn_link ;
	int atl_link ;
	int area_list ;
	int num_lines ;
	int act_lines ;
	int line_list ;
} cats[32] ;
