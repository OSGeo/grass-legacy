#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

	GLOBAL int box_color ;
	GLOBAL int line_color ;
	GLOBAL char file[256] ;
	GLOBAL char file_mapset[128] ;
	GLOBAL double from_easting ;
	GLOBAL double from_northing ;
	GLOBAL double from_height ;
	GLOBAL double to_easting ;
	GLOBAL double to_northing ;
	GLOBAL double to_height ;
	GLOBAL int line_freq ;
	GLOBAL double exag ;
	GLOBAL char elevfile[256] ;
	GLOBAL char elevfile_mapset[128] ;
	GLOBAL double field ;
	GLOBAL int direction ;
	GLOBAL struct Cell_head window ;
	GLOBAL int lines_only ;
	GLOBAL int do_zero ;
	GLOBAL int do_average ;
