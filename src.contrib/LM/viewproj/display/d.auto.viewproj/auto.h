/*
****************************************************************************
*
* MODULE:       d.auto.viewproj
* AUTHOR(S):    Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      Automatically set the region and projection parameters.
* COPYRIGHT:    (C) 2003 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/


#define MAX_PROJ_PARAM_LEN	 50
#define MAX_PROJ_PARAMS		 15
#define MAX_PROJ_LIST		 2048

typedef enum {
	CYL,	/* Cylindrical */
	PCYL,	/* Pseudocylindrical */
	CONIC,	/* Conic */
	AZIM,	/* Azimuthal */
	MISC	/* Miscellaneous */
} projtype;


typedef struct {
    char	proj_name[8];		/* Name for proj */
    char	proj_args[64];		/* Arguments for proj */
    char	proj_default[64];	/* Default argument for proj */
    double	max_lat_span, max_lon_span;	/* Maximum range */
    double	polar_lat_limit;		/* 0.0=non_polar */
    projtype	proj_type;
    char 	full_name[64];		/* Full name of the projection */
} proj_def_struct;


typedef struct {	/* Region in map coordinates */
    double	north;	/* upper lat */
    double	south;	/* lower lat */
    double	east;	/* right lon */
    double	west;	/* left lon */
} zoom_struct;


int 	auto_proj_region (proj_def_struct *map_proj, zoom_struct *map_region);
int 	auto_proj_param (proj_def_struct *map_proj, zoom_struct map_region,
    		char proj_param[MAX_PROJ_PARAMS][MAX_PROJ_PARAM_LEN]);
int 	auto_proj_list (char *proj_list);
proj_def_struct *auto_translate_proj (char *proj_str);


