/*====================================================================*/
/* program to propogate the link label into the hillslope areas;      */
/* processes CELL files only and works on window derived from link    */
/* label map                                                          */
/*====================================================================*/
#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "local_proto.h"

#define NOMASK 1

static int nrows, ncols;

int main (int argc, char *argv[]) 
{
    int partfd;
    char msg[100];
    char drain_name[30], *drain_mapset;
    char ridge_name[30], *ridge_mapset;
    char part_name[30], *part_mapset;
    CELL *drain, *ridge;
    struct Cell_head window;
    int row, col, npass, tpass;
    char mg[100]; 
	struct GModule *module;
    struct Option *opt1, *opt2, *opt3, *opt4 ;

	module = G_define_module();
	module->description =
		"Generates a raster map layer showing "
		"watershed subbasins.";

    opt1 = G_define_option() ;
    opt1->key        = "number" ;
    opt1->type       = TYPE_INTEGER ;
    opt1->required   = YES ;
    opt1->description= "Number of passes through the dataset" ;
	opt1->gisprompt  = "old,cell,raster" ;

    opt2 = G_define_option() ;
    opt2->key        = "c_map" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = YES ;
    opt2->description= "Coded stream network file name";
	opt2->gisprompt  = "old,cell,raster" ;

    opt3 = G_define_option() ;
    opt3->key        = "t_map" ;
    opt3->type       = TYPE_STRING ;
    opt3->required   = YES ;
    opt3->description= "Thinned ridge network file name" ;
	opt3->gisprompt  = "old,cell,raster" ;

    opt4 = G_define_option() ;
    opt4->key        = "result" ;
    opt4->type       = TYPE_STRING ;
    opt4->required   = YES ;
    opt4->description= "Name for the resultant watershed partition file" ;
	opt4->gisprompt  = "new,cell,raster" ;

    G_gisinit (argv[0]);

    if (G_parser(argc, argv) < 0)
        exit(-1);

    sscanf(opt1->answer, "%d", &tpass) ;
 
    strcpy(drain_name, opt2->answer); 
    drain_mapset = G_find_cell2(drain_name, "");
    if (drain_mapset == NULL)
    {
        sprintf(mg, "%s: <%s> raster file not found\n", G_program_name(),
                opt2->answer);
        G_fatal_error(msg);
	exit(1);
    }

/* this isn't a nice thing to do. G_align_window() should be used first */
    G_get_cellhd (drain_name, drain_mapset, &window);
    G_set_window (&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

   
    strcpy (ridge_name, opt3->answer);
    ridge_mapset = G_find_cell2(ridge_name, "");
    if (ridge_mapset == NULL)
    {
        sprintf(mg, "%s: <%s> raster file not found\n", G_program_name(),
                opt3->answer);
        G_fatal_error(msg);
        exit(1);
    }


    strcpy (part_name, opt4->answer);
    part_mapset = G_find_cell2(part_name,"");
    if (part_mapset != NULL)
    {
        sprintf(mg, "%s: <%s> raster file exists already\n", G_program_name(),
                opt4->answer);
        G_fatal_error(msg); 
	exit(1);
    }

    drain = read_map (drain_name, drain_mapset, NOMASK, nrows, ncols);
    ridge = read_map (ridge_name, ridge_mapset, NOMASK, nrows, ncols);

    partfd = G_open_cell_new (part_name);
    if (partfd < 0)
    {
	sprintf (msg, "unable to create %s", part_name);
	G_fatal_error (msg);
    }

/* run through file and set streams to zero at locations where ridges exist*/ 
    for (row = 0; row < nrows; row++)
    {
	for (col = 0; col < ncols; col++)
	    if (ridge[row*ncols + col] != 0) drain[row*ncols + col] = 0;
     }

    for (npass = 1; npass <= tpass; npass++)
    {
	for (row = 1; row < nrows-1; row++)
	{
	 for (col = 1; col < ncols-1; col++)
	  {
	   if(drain[row*ncols+col] == 0 && ridge[row*ncols+col] == 0)  
	    {
	    if(drain[(row-1)*ncols+col] != 0 && ridge[(row-1)*ncols+col] == 0) 
	     drain[row*ncols+col] = drain[(row-1)*ncols+col];  
	    if(drain[row*ncols+(col-1)] !=0 && ridge[row*ncols+(col-1)] == 0 ) 
	     drain[row*ncols+col] = drain[row*ncols+(col-1)]; 
	    }
	  }
	}
        fprintf (stdout,"forward sweep complete\n");

	for (row = nrows-3; row > 1; --row)
	{
	 for (col = ncols-3; col > 1; --col)
	  {
	   if(drain[row*ncols+col] == 0 && ridge[row*ncols+col] == 0)  
	    {
	    if(drain[(row+1)*ncols+col] != 0 && ridge[(row+1)*ncols+col] == 0) 
	     drain[row*ncols+col] = drain[(row+1)*ncols+col]; 
	    if(drain[row*ncols+(col+1)] != 0 && ridge[row*ncols+(col+1)] == 0) 
	     drain[row*ncols+col] = drain[row*ncols+(col+1)]; 
	    }
	   }
	}
        fprintf (stdout,"reverse sweep complete\n");    
     }

/* write out partitioned watershed map */
for (row = 0; row<nrows; row++)
 G_put_map_row (partfd, drain+(row*ncols));
      
    fprintf (stdout,"creating support files for %s\n", part_name);
    G_close_cell (partfd);

    exit(0);

}
