/*
   d.rast.edit
 
   Usage:  d.rast.edit

   Chris Rewerts, Agricultural Engineering, Purdue University
   April 1991

   d.rast.edit is an graphically interactive cell editor which allows the
   user to display a cell layer on the graphics monitor and use the
   mouse cursor to indicate which cell values to edit by pointing to
   cells displayed. 

   d.rast.edit determines the name and mapset of the cell layer displayed on
   the monitor, then makes a copy of the layer's values in a temporary
   file. Note: current window settings and masks are ignored when
   making the copy, so that the view on the screen can be various
   "zoom" views to allow the user to find individual cells. After
   editing, a new cell layer is created.
 
 */

#define GLOBAL
#include "edit.h"

main(argc, argv)
int argc ;
char **argv ;
{
    char temp[128], line[128] ;
    int stat;
    int i;
    char *m;
    int fd;
    CELL *cell;
	struct Option *opt1 ;

  /* Initialize the GIS calls */
    G_gisinit (argv[0]) ;

	opt1    = G_define_option() ;
	opt1->key = "grid_color" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = NO ;
    opt1->answer     = "black" ;
    opt1->options = "white,red,orange,yellow,green,blue,indigo,violet,magenta,brown,gray,black";
    opt1->description= "Color for showing grid boundaries" ;

    /* Check command line */
    if (G_parser(argc, argv))
        exit(-1);

	grid_color = D_translate_color("opt1->answer") ;

    R_open_driver();
    /* if the monitor is divided into more than one window, 
       find which is the choosen, current one being used */

    if (D_get_cur_wind(temp))
        error(1,"No current graphics window") ;

    if (D_set_cur_wind(temp))
        error(1,"Current graphics window not available") ;

    /* now that we have established what monitor window we
       are to work with, find out the name of the cell layer
       currently displayed there */

    D_get_cell_name (temp);
    if (*temp == 0 )
        error (1,"no data layer drawn in current window");

    m = G_find_cell2(temp, "");
    if(m == NULL)
    {
        sprintf(line, " %s - cell file not found\n", temp);
        error(1, line);
    }
    strcpy(orig_mapset, m);
    sscanf (temp, "%s", orig_name);

    fprintf (stderr, "\n\nName of original cell layer: [%s] in [%s]\n", orig_name, orig_mapset);

  /* get the name for the new cell file to be created (the one we will edit) */
    m = G_ask_cell_new("Enter the name for the new layer to be created\n", 
    new_name);
    if (m == NULL) {
        ext();
    }
  /* get names straight  */
    strcpy(user_mapset, m);
    strcpy(current_name, orig_name);
    strcpy(current_mapset, orig_mapset);

   /* we are not in the business of resampling the old file to
      a new cell file with a different window setting, so when
      we make a copy of the cell file to be edited, we will
      ignor the current window and find out what the cell_head
      window is (and call that "real_window")(the current_window
      could be anything, since the user will probably have to use
      zoom to get a detailed view of what they are editing). */

    G_get_cellhd(orig_name, orig_mapset, &real_window);

    real_nrows = real_window.rows;
    real_ncols = real_window.cols;

    main_menu();

    ext();
}

/*---------------------------------------------------------------*/
do_edit(c_row, c_col, new_value)

  /* 
     called by the edit() routine. given a row and col location 
     a value for the cell at that location, find that location
     in the temporary file and write in the new value
  */

    long new_value;	
    int c_row, c_col;
{

   int fd;
   long offset;
   CELL c;

    fd = open(tempfile, 2);
    if (c_row > real_nrows - 1)
        error(0,"bad row number\n");
    if (c_col > real_ncols - 1)
        error(0,"bad col number\n");

  /* how many bytes into the temporary file will this value be? */
    offset = ((c_row * real_ncols) + c_col) * cellsize; 
 
    lseek(fd, offset, 0);
    read(fd, &c, cellsize);

    lseek(fd, offset, 0);
    c = (CELL)new_value;
    if( write(fd, &c, cellsize) != cellsize )
        error(0,"was not able to write new value");

    close(fd);
}
/*------------------------------------------------------------------*/
error(code, message)

  /* 
     some sort of function to deal with errors.
     code 0: print warning and continue
     code 1: print error message, close things down, and die
  */

int code;
char message[128];
{

int cellfd;

    if(code == 0)
    fprintf(stderr, "\n\7WARNING: %s\n", message);
    else if(code == 1)
    fprintf(stderr, "\n\7ERROR: %s\n", message);
    if(code == 1){
        G_unopen_cell(cellfd);
        unlink(tempfile);
        R_close_driver();
    fprintf(stderr, "\n     +-------------------------------------------+\n");
    fprintf(stderr, "     |                d.rast.edit aborts         |\n");
    fprintf(stderr, "     +-------------------------------------------+\n\n");
        exit(-1);
    }

}

/*-------------------------------------------------------------*/

ext()
{
    R_close_driver();
    fprintf(stderr, "\n     +-------------------------------------------+\n");
    fprintf(stderr, "     |                 d.rast.edit exits         |\n");
    fprintf(stderr, "     +-------------------------------------------+\n\n");
    exit(0);
}
