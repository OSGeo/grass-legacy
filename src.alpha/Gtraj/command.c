/****************************************************************/
/*								*/
/*	command.c		in		~/src/Gtraj	*/


#include "cmd_line.h"
#include "gis.h"

set_default_options()
{

	east = 0.0;
	north = 0.0;
	weapon_elev = 0.0;
	strcpy(weapon,"");
	strcpy(ammunition,"");
	strcpy(azimuth2,"");
	strcpy(azimuth1,"");
	strcpy(elev_layer,"");
	strcpy(out_layer,"");
}
	

stash_away(pos,option)
	int pos;
	char *option;
{
	switch(pos)
	{

	case ELEV_LAYER:
		strcpy(elev_layer,option);
		break;

	case EAST:	
		if(! sscanf(option,"%lf",&east))
			return(-1) ;
		break;

	case NORTH:
		if(! sscanf(option,"%lf",&north))
			return(-1); 
                break; 

	case WEAPON:
		strcpy(weapon,option);
                break;	

	case AMMUNITION:
		strcpy(ammunition,option);      
                break;

	case WEAPON_ELEV:
		if(! sscanf(option,"%lf",&weapon_elev))
                        return(-1);  
                break;  

	case AZIMUTH1:
		strcpy(azimuth1,option);
                break;

	case AZIMUTH2:
		strcpy(azimuth2,option);
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

/****************************************************************/
