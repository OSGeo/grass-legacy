/****************************************************************/
/*								*/
/*	cmd_line.h	in	~/src/Gtraj			*/
/*								*/


#define		ELEV_LAYER	1
#define		EAST		2
#define		NORTH		3
#define		WEAPON		4
#define		AMMUNITION	5
#define		WEAPON_ELEV	6
#define		AZIMUTH1	7
#define		AZIMUTH2 	8	
#define		OUT_LAYER 	9	


#ifdef MAIN

	struct variables
	{
		char *alias;
		int position;
	}

	variables [] = {
		
		"elev_layer",ELEV_LAYER,
		"east",EAST,
		"north",NORTH,
		"weapon",WEAPON,
		"ammunition",AMMUNITION,
		"weapon_elev",WEAPON_ELEV,
		"azimuth1",AZIMUTH1,
		"azimuth2",AZIMUTH2,
		"out_layer",OUT_LAYER

			};

	static int n_variables = 9;
	double east;
	double north;
	double weapon_elev;
	char azimuth1[64];
	char azimuth2[64];
	char elev_layer[64];
	char out_layer[64];
	char weapon[64];
	char ammunition[64];

#else
	extern double east;
	extern double north;
	extern double weapon_elev;
	extern char azimuth1[];
	extern char azimuth2[];
	extern char elev_layer[];
	extern char weapon[];
	extern char ammunition[];
	extern char out_layer[];
#endif

/****************************************************************/
