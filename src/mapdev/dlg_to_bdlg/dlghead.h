/*  @(#)dlghead.h	2.1  6/26/87  */
int nlines ;
char banner[73] ;
char cart_unit[41] ;
char source_date[11] ;
char orig_scale[9] ;
char line_3[73] ;
int level_code ;
int plani_code ;
int plani_zone ;
int plani_units ;
double resolution ;
int trans_param ;
int misc_records ;
int num_sides ;
int num_cats ;

struct
{
	char corner[3] ;
	double lat ;
	double lon ;
	double utm_n ;
	double utm_e ;
} coors[4] ;

double params[15] ;
double int_params[4] ;

struct 
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
