/****************************************************************/
/*								*/
/*	Glos_command.c	in	~/src/Glos			*/
/*								*/

#include "cmd_line.h"
#include "gis.h"

/****************************************************************/
/*								*/
/*	This function sets the command line variables to	*/
/*	their default values					*/
/*								*/

set_default_options()
{
	extern struct Cell_head window;

	east = 0.0;
	north = 0.0;
	obs_elev = 1.75;
	max_dist = 100.0 * window.ns_res;
	strcpy(elev_layer,"");
	strcpy(patt_layer,"");
	strcpy(out_layer,"");
}
	
/*********** END OF FUNCTION "SET_DEFAULT_OPTIONS" **************/




/*	This function assigns the values of the command line	*/
/*	arguments to the appropriate variables depending on	*/
/*	the positions occupied by those arguments on the	*/
/*	command line						*/

stash_away(pos,option)
	int pos;
	char *option;
{
	switch(pos)
	{

	case ELEV_LAYER:
		strcpy(elev_layer,option);
		break;

	case PATT_LAYER:
		strcpy(patt_layer,option);
		break;

	case EAST:	
		if(! sscanf(option,"%lf",&east))
			return(-1) ;
		break;

	case NORTH:
		if(! sscanf(option,"%lf",&north))
			return(-1); 
                break; 

	case OBS_ELEV:
		if(! sscanf(option,"%lf",&obs_elev))
                        return(-1);  
                break;  
 
	case MAX_DIST:
		if(! sscanf(option,"%lf",&max_dist))
                        return(-1);   
                break;   
  
	case OUT_LAYER:
 		strcpy(out_layer,option);      
                break;

	default:
		printf("Unknown option\n");
		return(-1);
		break;
	}
	return(0);
}

/********** END OF FUNCTION "STASH_AWAY" ************************/
 
 



		
		
