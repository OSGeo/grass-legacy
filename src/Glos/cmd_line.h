/****************************************************************/
/*								*/
/*	Glos_cmd_line.h		in 	~/src/Glos		*/
/*								*/
/*	This header file declares the global variables and the 	*/
/*	structures that are to be used for command line		*/
/*	processing						*/
/*								*/
/****************************************************************/

#define		ELEV_LAYER	1
#define		PATT_LAYER	2
#define		EAST 		3
#define		NORTH		4
#define		OBS_ELEV	5
#define		MAX_DIST	6
#define		OUT_LAYER	7

#ifdef MAIN

	struct variables
	{
		char *alias;
		int position;
	}

	variables [] = {
		
		"elev_layer",ELEV_LAYER,
		"patt_layer",PATT_LAYER,
		"east",EAST,
		"north",NORTH,
		"obs_elev",OBS_ELEV,
		"max_dist",MAX_DIST,
		"out_layer",OUT_LAYER

			};

	static int n_variables = 7;
	double east;
	double north;
	double obs_elev;
	double max_dist;
	char elev_layer[64];
	char patt_layer[64];
	char out_layer[64];

#else
	extern double east;
	extern double north;
	extern double obs_elev;
	extern double max_dist;
	extern char elev_layer[];
	extern char patt_layer[];
	extern char out_layer[];
#endif

								
/************* END OF "GLOS_CMD_LINE.H" *************************/
