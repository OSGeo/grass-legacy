/* %W%  %G%  */
#include "options.h"
#include "gis.h"

usage(cmd)
	char *cmd ;
{
	printf("\nUSAGE:\n") ;
	printf("%s [file=MAPNAME] [fe=FROM_EASTING] [fn=FROM_NORTHING] [fh=FROM_HEIGHT]\n", cmd) ;
	printf("[te=TO_EASTING] [tn=TO_NORTHING] [th=TO_HEIGHT]\n") ;
	printf("[ew=east-west resolution] [ns=north-south resolution] [lf=LINE_FREQ]\n") ;
	printf("[exag=EXAG] [ef=ELEVFILE] [va=VIEWANGLE]\n") ;
}

set_default_options()
{
	char *strcpy() ;
	strcpy (file, "elevation") ;
	strcpy (file_mapset, "PERMANENT") ;
	strcpy (elevfile, "elevation") ;
	strcpy (elevfile_mapset, "PERMANENT") ;
	to_easting =  (window.east  + window.west ) / 2 ;
	to_northing = (window.north + window.south) / 2 ;
	to_height = 0 ;
	from_easting = window.west - (window.east - window.west) ;
	from_northing = window.south - (window.north - window.south) ;
	from_height = 5000. ;
	exag = 2.0 ;
	line_freq = 10 ;
	field = 20.0 ;
	show_default = 0 ;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
	char *mapset ;
	char buffer[128] ;

	switch(pos)
	{
	case FILENAME :
		mapset = G_find_cell (option, "") ;
		strcpy(file, option) ;
		strcpy(file_mapset, mapset) ;
		if ((strlen(file) == 0) || (strlen(file_mapset) == 0))
		{
			sprintf(buffer, "Map: [%s] not found", file) ;
			G_warning(buffer) ;
			return -1;
		}
		break ;
	case FROM_EASTING :
		sscanf(option,"%lf",&from_easting) ;
		break ;
	case FROM_NORTHING :
		sscanf(option,"%lf",&from_northing) ;
		break ;
	case FROM_HEIGHT :
		sscanf(option,"%lf",&from_height) ;
		break ;
	case TO_EASTING :
		sscanf(option,"%lf",&to_easting) ;
		break ;
	case TO_NORTHING :
		sscanf(option,"%lf",&to_northing) ;
		break ;
	case TO_HEIGHT :
		sscanf(option,"%lf",&to_height) ;
		break ;
	case EW_RES :
		sscanf(option,"%lf",&window.ew_res) ;
		break ;
	case NS_RES :
		sscanf(option,"%lf",&window.ns_res) ;
		break ;
	case LINE_FREQ :
		sscanf(option,"%d",&line_freq) ;
		break ;
	case EXAG :
		sscanf(option,"%lf",&exag) ;
		break ;
	case ELEVFILE :
		mapset = G_find_cell (option, "") ;
		strcpy(elevfile, option) ;
		strcpy(elevfile_mapset, mapset) ;
		if ((strlen(elevfile) == 0) || (strlen(elevfile_mapset) == 0))
		{
			sprintf(buffer, "Map: [%s] not found", elevfile) ;
			G_warning(buffer) ;
			return -1;
		}
		break ;
	case VIEWANGLE :
		sscanf(option,"%lf",&field) ;
		break ;
	case DEFAULT :
		show_default=1 ;
		break ;
	default:
		printf("Unknown option\n") ;
		return(-1) ;
		break ;
	}
	return(0) ;
}
