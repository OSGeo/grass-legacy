/* %W%  %G%  */

/* Command argument positions */
#define FILENAME		1
#define FROM_EASTING	2
#define FROM_NORTHING	3
#define FROM_HEIGHT		4
#define TO_EASTING		5
#define TO_NORTHING		6
#define TO_HEIGHT		7
#define EW_RES			8
#define NS_RES			9
#define LINE_FREQ		10
#define EXAG			11
#define ELEVFILE		12
#define VIEWANGLE		13
#define DEFAULT			14

#ifdef MAIN
	struct variables
	{
		char *alias ;
		int position ;
	} variables[] = {
		"file", FILENAME,
		"name", FILENAME,
		"from_easting", FROM_EASTING,
		"fe", FROM_EASTING,
		"from_northing", FROM_NORTHING,
		"fn", FROM_NORTHING,
		"from_height", FROM_HEIGHT,
		"fh", FROM_HEIGHT,
		"to_easting", TO_EASTING,
		"te", TO_EASTING,
		"to_northing", TO_NORTHING,
		"tn", TO_NORTHING,
		"to_height", TO_HEIGHT,
		"th", TO_HEIGHT,
		"east_west", EW_RES,
		"ew", EW_RES,
		"north_south", NS_RES,
		"ns", NS_RES,
		"line_freq", LINE_FREQ,
		"lf", LINE_FREQ,
		"exaggeration", EXAG,
		"exag", EXAG,
		"elev_file", ELEVFILE,
		"ef", ELEVFILE,
		"view_angle", VIEWANGLE,
		"va", VIEWANGLE,
		"default", DEFAULT
	} ;
	static int n_variables = 27 ;
#define GLOBAL
#else
#define GLOBAL extern
#endif

	GLOBAL char file[64] ;
	GLOBAL char file_mapset[64] ;
	GLOBAL double from_easting ;
	GLOBAL double from_northing ;
	GLOBAL double from_height ;
	GLOBAL double to_easting ;
	GLOBAL double to_northing ;
	GLOBAL double to_height ;
	GLOBAL int line_freq ;
	GLOBAL double exag ;
	GLOBAL char elevfile[64] ;
	GLOBAL char elevfile_mapset[64] ;
	GLOBAL double field ;
	GLOBAL int direction ;
	GLOBAL struct Cell_head window ;
	GLOBAL int show_default ;
