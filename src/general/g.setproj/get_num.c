#include <math.h>
#include <gis.h>
#include "table.h"

char answer[200];

get_KFACT()
{
   sprintf(answer,"Enter %s ",DESC[KFACT]);
   kfact = prompt_num_double (answer, 1.0, 1);
   return(1);
}

get_x0()
{
   sprintf(answer,"Enter %s ",DESC[X0]);
   x_false = prompt_num_double (answer, 0.0, 1);
   return(1);
}



get_zone()
{
   sprintf(answer,"Enter Zone");
   zone = prompt_num_int (answer, 0, 0);
   return(1);
}


/*
*      Get the Prime Meridian value and std parallel value
*****/
get_LL_stuff(lat,index)
int lat;
int index;
{
    char buff[256];

/*  get LONCEN value arguements */
        if (TABLE[proj_index][index].def_exists == 1) {
          if (lat == 1)
	    {
	       G_format_northing(TABLE[proj_index][index].deflt, buff, PROJECTION_LL);
               fprintf(stderr,"\n    Enter %s (%s) :",
                  DESC[index], buff);
            }
          else
	    {
	       G_format_easting((TABLE[proj_index][index].deflt), buff, PROJECTION_LL);
               fprintf(stderr,"\n    Enter %s (%s) :",
                  DESC[index], buff);
            }
	  G_gets(answer);
          if (strlen(answer) == 0)
	   {
	     LLSTUFF[index] = TABLE[proj_index][index].deflt;
	     return(1);
	   }
        }
        else {
          fprintf(stderr,"\n    Enter %s :", DESC[index]);
          if (strlen(answer) == 0)
	   {
	     LLSTUFF[index] = 0.0;
	     return(0);
           }
        }
        if (lat == 1) {
	   if (!get_deg(answer,1))
	    {
	    return(0);
	    }
        }
        else {
	   if (!get_deg(answer,0))
	    {
	    return(0);
	    }
        }
	sscanf(answer,"%lf", &(LLSTUFF[index]));
	return(1);
}

double
prompt_num_double (str, deflt, is_default)
    char *str;
    double deflt;
{
    char answer[300];
    double tmp;

    while (1)
    {
	if (is_default)
	    fprintf (stderr, "\n%s [%.10lf]: ", str, deflt);
	else
	    fprintf (stderr, "\n%s: ", str);

	G_gets(answer);
	G_strip (answer);
	if (strlen(answer) == 0 && is_default)
	    return deflt;
	else
	{
	    if(sscanf(answer, "%lf", &tmp)==1)
		break;
	}
    }
    return tmp;
}


prompt_num_int (str, deflt, is_default)
    char *str;
    int deflt;
{
    char answer[300];
    int tmp;

    while (1)
    {
	if (is_default)
	    fprintf (stderr, "\n%s [%d]: ", str, deflt);
	else
	    fprintf (stderr, "\n%s: ", str);

	G_gets(answer);
	G_strip (answer);
	if (strlen(answer) == 0 && is_default)
	    return deflt;
	else
	{
	    if (1 == sscanf (answer, "%d", &tmp))
		break;
	}
    }
    return tmp;
}
