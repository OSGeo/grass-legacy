/*
****************************************************************************
*
* MODULE:       d.set.viewproj
* AUTHOR(S):    Sharif Razzaque, LMMS, June 1995
*               Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      To define the viewing map projection.
* COPYRIGHT:    (C) 1995 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
*************************************************************************
d.set.viewproj

This executable is used to define the viewing-map-projection for future maps,
just as g.region is used define the region of the earth to plot on future maps.

This code inputs projection information from the user and stores it in the 
state file for use by the drawing commands.  The paramters to be passed to 
PROJ to get the desired map-projection.
 
Parameters are entered almost exactly like they are passed to PROJ.

For example, the following PROJ command line:
	proj +proj=merc +lon_0=90w (see PROJ manual)
becomes:
	d.set.viewproj proj=merc lon_0=90w
while
	proj +proj=ups +south
becomes:
	d.set.viewproj proj=ups south=y

PROJ flags that don't take any parameters (like +south in the above example)
as inputted as the name of the flag followed by =y.
(This is the best I could do using the GRASS parcer)

note: there is no interactive version, just this command line version.
      it will prompt the user, however, if they forget the projection type.

Sharif Razzaque June 1995
*************************************************************************
*/
#include <unistd.h>	/* For access */
#include <sys/types.h>	/* For mkdir */
#include <sys/stat.h>	/* For mkdir */

#include "config.h"	/* For Grass 5.0 Bev Wallace */
#include "gis.h"
#include "projects.h"


int main (int argc, char **argv)
{

    struct Option *proj;

    /* these are the optional parameters which proj accepts. */
    /* if you need a parameter that isn't listed here, please add it here */

    char *opt_parms_names[] = 
    {
		"ellps",
		"lon_0", "lon_1", "lon_2", "lon_3", "lonc", "lon_a", "lon_b",
		"lat_0", "lat_1", "lat_2", "lat_3", "lat_ts", "lat_b", "lat_a",
		"k" ,"zone", "alpha", "a", "h", "W"
    };
    int num_of_possible_opt_parms;
    struct Option *opt_parms[100];

    char *opt_flags_names[]=
    {
		"no_cut","ns","no_rot","south","over","geoc","no_defs"
    };
    int num_of_possible_opt_flags;
    struct Option *opt_flags[100];

    char save_proj_info[50][50];
   
    int i,j;


/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

/* set up command line for proj parameters */

        /* this is the only parameter required by proj */
    proj             = G_define_option();
    proj->key         = "proj";
    proj->type        = TYPE_STRING;
    proj->required    = YES;
    proj->description = "projection to display maps in (see PROJ manual)";

        /* setup optional parameters */

    num_of_possible_opt_parms= sizeof(opt_parms_names)/sizeof(char *);
    for (i=0; i<num_of_possible_opt_parms; i++)
	{
                opt_parms[i]		=	G_define_option();
	        opt_parms[i]->key	=	opt_parms_names[i];
                opt_parms[i]->type	=       TYPE_STRING;
		opt_parms[i]->required  =	NO;
		opt_parms[i]->description=  "optional proj parameter, use varies by projection (see PROJ man)";
	}

      /* setup optional flags */
      /*   note: these are the flags for proj, but I define them to the parsers 
           as options because the parser supports them better */
   
    num_of_possible_opt_flags= sizeof(opt_flags_names)/sizeof(char *);
    for (i=0; i<num_of_possible_opt_flags; i++)
	{
                opt_flags[i]		=	G_define_option();
		opt_flags[i]->key	=	opt_flags_names[i];
		opt_flags[i]->description=" optional proj flag, enter y or n";
		opt_flags[i]->type	=	TYPE_STRING;
		opt_flags[i]->answer    =  "n";
		opt_flags[i]->required  =	NO;
		opt_flags[i]->options   =	"y,n";
	}

    /* get proj info from user */
    if (G_parser(argc, argv))
	exit(1);


    /* now pack the infomation in a formation in a format that is saveable */
	sprintf(save_proj_info[0],"proj=%s",proj->answer);
        j=1;
	for (i=0; i<num_of_possible_opt_parms; i++)
	{
		if (opt_parms[i]->answer!=NULL)
		
       		{

			sprintf(save_proj_info[j], "%s=%s", opt_parms_names[i], 
				opt_parms[i]->answer);
			j++;
		}
	}
	for (i=0; i<num_of_possible_opt_flags; i++)
	{
		if (*(opt_flags[i]->answer)=='y')
		{
			sprintf(save_proj_info[j], "%s", opt_flags_names[i]);
			j++;
		}
	}

   
   /* see if proj will accept it */
  	{
		PJ *ref;
		static char *parms[50];

		for (i=0; i<j; i++)
		{
			parms[i]=save_proj_info[i];
		}

		ref = pj_init(j,parms);
		if (ref)
		{
			fprintf (stdout, "projection was accepted by PROJ\n");
			fflush (stdout);
			pj_free(ref);
		}
		else
		{
			/* Exit due to PROJ error */
			fprintf(stderr, "pj_init error %d:  %s\n", 
				pj_errno, pj_strerrno(pj_errno));
			G_fatal_error ("PROJ doesn't accept these parameters!");
		}
	}

     /* save it the general projection information file */
 	{
		FILE *file_Ptr;

		file_Ptr = G_fopen_new ("viewproj_states" , "current");
                if (file_Ptr == NULL)
                    G_fatal_error ("Unable to open file viewproj_states/current");

		for (i=0;i<j;i++)
		{
			fprintf(file_Ptr,"%s\n",save_proj_info[i]);
		}
		fclose(file_Ptr);
	}
			

    exit(0);
}

