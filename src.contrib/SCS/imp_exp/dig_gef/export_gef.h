#define UNIT_FILE "PROJ_UNITS"
#define PROJECTION_FILE "PROJ_INFO"

struct Map_info Map ;
struct Key_Value *in_proj_keys;
struct line_pnts *Points ;

struct ghead
{
	char map_name[50] ;
	long  orig_scale ;
	char proj_name[8] ;
	char area_name[30] ;
	char organization[30] ;
	char source_date[12] ;
	char data_typ[18] ;
	char coord_sys[9] ;
	int plani_zone ;
} ghead ;
