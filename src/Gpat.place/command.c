/****************************************************************/
/*								*/
/*	Gpat_place_command.c	in  ~/src/Gpat_place		*/
/*								*/
/*								*/

#include "command.h"

/****************************************************************/ 
/*								*/
/*	This function sets the command line variables to	*/
/*	their default values.					*/


set_default_options()
{
	strcpy(pat_type,"");
	strcpy(bdlg_out_file,"");
	utm_x 	= 0.0;
	utm_y	= 0.0;
	rotation = 0.0;

}

/************** END OF FUNCTION "SET_DEFAULT_OPTIONS" ***********/



/*	This function assigns the values of the command line	*/
/*	arguments to the appropriate variables depending on	*/
/*	the positions occupied by those arguments on the	*/
/*	command line.						*/


stash_away(pos,option)
        int pos;
        char *option;
{
        switch(pos)
        {

        case PAT_TYPE:
		strcpy(pat_type,option);
		break;
	
	case BDLG_OUT_FILE:
		strcpy(bdlg_out_file,option);
		break;

	case UTM_X:      
                if(! sscanf(option,"%lf",&utm_x))
                        return(-1) ;
                break;

        case UTM_Y:
                if(! sscanf(option,"%lf",&utm_y))
                        return(-1); 
                break; 

	case ROTATION:
		if(! sscanf(option,"%lf",&rotation))
                        return(-1);  
                break;  

	default:
                printf("Unknown option\n");
                return(-1);
                break;
        }
        return(0);
}

/************** END OF FUNCTION "STASH_AWAY *********************/
