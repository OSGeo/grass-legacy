/****************************************************************/
/*								*/
/*	Gpat_place_cmd_line.h	in  ~/src/Gpat_place		*/
/*								*/
/*	This header file declares the global variables and	*/
/*	the structures to be used for the command line		*/
/*	processing.						*/
/*								*/
/****************************************************************/

#define 	PAT_TYPE	1	
#define		BDLG_OUT_FILE	2
#define		UTM_X		3
#define		UTM_Y		4
#define		ROTATION	5

#ifdef MAIN

        struct variables
        {
                char *alias;
                int position;
        }

        variables [] = {

	"pat_type",PAT_TYPE,
	"bdlg_out_file",BDLG_OUT_FILE,
	"utm_x",UTM_X,
        "utm_y",UTM_Y,
	"rotation",ROTATION
			};

	static int n_variables = 5;
	char pat_type[64];
	char bdlg_out_file[64];
	double utm_x;
        double utm_y;
	double translation_x;
	double translation_y;
	double rotation;
	double cos_theta;
	double sin_theta;
	double origin_x;
	double origin_y;
#else
	extern char pat_type[];
	extern char bdlg_out_file[];
	extern double utm_x;
	extern double utm_y;
	extern double rotation;
	extern double origin_x;	
	extern double origin_y;
	extern double cos_theta;
	extern double sin_theta;
#endif

/************ END OF "GPAT_PLACE_CMD_LINE.H" ********************/

