#include "gis.h"
#include "site.h"

#define QUAD_SIZE 0

/***********************************************************************/
/* sites2cell:  Converts a GRASS site file to a GRASS cell file. The   */
/*              program uses a subset of the funtions in s.menu.       */
/*              It is non-interactive.                                 */
/*                                                                     */
/* NOTE:        The quad size (ie number of cells used to represent    */
/*              one site) is fixed to 0 (ie one cell/site).            */
/*              If the output file already exists, the program exits   */
/*              (gracefully).                                          */
/*                                                                     */
/* Calls:       Library routines (as included above)                   */
/*              get_site      get_sites.c                              */
/*              write_cell    write_cell.c                             */
/*                                                                     */
/* Author:      Katarina Johnsson, CCRS                                */
/*                                                                     */
/* Date:        March 6, 1992                                          */
/*                                                                     */
/* Modified by: Tom Brauer, USACERL                                    */
/* Date:        November 19, 1992                                      */
/* Description:	Removed all *.c files which were compiled in from the */
/*		    s.menu code, but were not required.                */
/*              Placed all remaining code in same directory as main.   */
/*		Renamed sites2cell as s.to.rast                        */
/*		Changed "sites"	to "input" & "cell" to "output" in     */
/*		    the parser routines                                */
/*              Added a flag for verbose                               */
/*		Enabled a parser run interactive version               */
/***********************************************************************/
main (argc, argv) 	
char *argv[];
{
    SITE_LIST site_list;
    struct Cell_head window;
    struct Categories cats;
    char layer[40];
    char *mapset;
    int cellfd;
    int north;
    int east;
    char *desc;
    int n;
    char title[200];
    char *name;
    char buf[200];
    int zero_one;
    int some_ok;
    long xcat;
    int Quiet;

    struct Option *input;
    struct Option *output;
    struct Flag   *verbose;

/* INIT 
*/

    G_gisinit(argv[0]);

    input = G_define_option();
    input->key		="input";
    input->description	="Name of input site list";
    input->type	=TYPE_STRING;
    input->required	=YES;
    input->gisprompt = "old,site_lists,site list";
    input->description = "Name of site file";

    output = G_define_option();
    output->key		="output";
    output->description	="Name of output cell file";
    output->type	=TYPE_STRING;
    output->required	=YES;
    output->gisprompt   ="new,cell,raster";
    output->description ="Name of new raster file";

    verbose = G_define_flag() ;
    verbose->key         = 'v' ;
    verbose->description = "Verbose";

    /* G_disable_interactive();
    */
    if(G_parser(argc, argv))
	exit(-1);

    Quiet = !verbose->answer;

    G_get_window (&window);


/* Check ouput file name


     if (G_legal_filename(output->answer) == (-1) )
	{
		printf("\nFile name %s is not legal", output->answer);
		exit(-1);
	};


    if (G_find_cell2(output->answer, G_mapset()) != NULL)
	{
 		printf("\nCell file %s already exists\n", output->answer);
		exit(-1);
	};
*/
 

/* Read site list 
*/
    initialize_site_list (&site_list);   /* -> empty site list */

    strcpy((&site_list)->name, input->answer); 

    if(get_site(&site_list,Quiet) != 0)
	exit(-1);



/* Create output layer 
*/ 
    strcpy(layer, output->answer);
    strcpy(title,"Output from site2cell");
 
    if (!Quiet) printf ("creating empty raster file ...\n");
    if ((cellfd = G_open_cell_new_random (layer)) < 0)
    {
	printf("can't create raster file <%s>\n", layer);
	exit(-1);
    }



/* DO IT!!
   if the site descriptions are all of the form: #n <label>
   then assign the site to the #n category with label <label>
   otherwise create a 0/1 cell file
   Copied from s.menu/sites_to_cell/main.c
*/

    G_init_cats ((CELL)0, title, &cats);

    zero_one = 0;
    some_ok = 0;
    rewind_site_list (&site_list);
    for (n = 1; next_site (&site_list, &north, &east, &desc); n++)
    {
	*buf = 0;
	if (sscanf (desc,"#%ld%[^\n]", &xcat, buf) < 1)
	{
		zero_one = 1;
		if (some_ok) break;
		continue;
	}
	some_ok = 1;
	G_strip (buf);
	if (*buf)
	    G_set_cat ((CELL)xcat, buf, &cats);
    }
    if (zero_one && some_ok)
    {
	printf ("\n** some site descriptions were not in the #cat format.");
	printf (" creating a 0/1 raster file\n\n");
    }
    if (zero_one)
    {
	G_init_cats ((CELL)1,title,&cats);
	G_set_cat ((CELL)1, "site data", &cats);
    }
    G_set_cat ((CELL)0, "no data", &cats);

    if (!Quiet) printf ("transferring sites to raster file ...\n");

    rewind_site_list (&site_list);
    for (n = 1; next_site (&site_list, &north, &east, &desc); n++)
    {
	if (zero_one)
	    write_cell (cellfd, &window, north, east, QUAD_SIZE, (CELL)1);
	else
	{
	    xcat = 0;
	    sscanf (desc,"#%ld", &xcat);
	    write_cell (cellfd, &window, north, east, QUAD_SIZE, (CELL)xcat);
	}
    }
    if (!Quiet) printf ("creating support files ...\n");
    G_close_cell (cellfd);
    G_write_cats (layer, &cats);

    if (!Quiet) printf ("compressing raster file ...\n");

    sprintf (buf, "r.compress %s > /dev/null", layer);
    system (buf);

    if (!Quiet) printf("\n<%s> raster file complete\n", layer);


/* Clean up 
*/  
    rewind_site_list(site_list);
    free_site_list(site_list);
    return(0);
}
