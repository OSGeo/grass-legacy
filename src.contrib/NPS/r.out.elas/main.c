/* Function: "main.c" for GRASS Program "r.out.elas"             
   "r.out.elas" transfers GRASS cell files to ELAS raster files.
   
   Exit status of 0 indicates program was successful.
   Exit status of 1 indicates program was not successful.
   
   Author: Bruce Powell, National Park Service
*/
       

#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"

int 
main (int argc, char *argv[])
 {
  int fd_in;
  int fd_out;
  int fd_mask;
  CELL *cell;
  struct Cell_head region;
  struct Cell_head orig_region;
  struct Cell_head ref;
  struct Cell_head cellhd;
  char name[1024]; 
  char *mapset;
  char r_name[1024];
  char r_mapset[1024];
  char output[1024];
  long int row, col;
  long int nrows, ncols;
  int shift_left;
  double total_bytes;
  double required_bytes;
  CELL min, max;
  struct Colors colors;
  CELL cat;
  int red, green, blue;
  struct elas_header
   {
    long int nbih;
    long int nbpr;
    long int il;
    long int ll;
    long int ie;
    long int le;
    long int nc;
    long int hri4321;
    char y_grid_desc[4];
    long int y_offset;
    char x_grid_desc[4];
    long int x_offset;
    float y_spot_size;
    float x_spot_size;
    float matrix[4];
    long int ih19;
    long int ih20;
    char not_used_21[4];
    char not_used_22[4];
    long int labl;
    char head[4];
    char comments[96][4];
    unsigned short int color_table[256];
    char not_used_249_256[32];
   } elas;
  struct GModule *module;
  int elas_color_num;
  int i;
  int align;
  int colors_read;
  static char empty_byte = {'\0'};
  int tot_cols, extra_cols;
  unsigned short int temp_short;
  unsigned long int temp_long;
  double temp_doub;
  float temp_float;
  char indicator;
  struct Flag *flag;
  struct Option *opt_input;
  struct Option *opt_output;
  /* "nbpr" (number of bytes per record) will be a multiple of */
  /* the value in "ELAS_multiple".                             */
  static long int ELAS_multiple = {256};


  G_gisinit (argv[0]);
  
  /* Set description */
  module              = G_define_module();
  module->description = ""\
  "Export a GRASS raster map layer to an ELAS raster file";

  /* Request a pointer to memory for each flag. */
  flag = G_define_flag();
  flag->key = 'f';
  flag->description = "Floating-point ELAS output file will be created.";
  flag->answer = 0x00;
  /* Request a pointer to memory for each option. */
  opt_input = G_define_option();
  opt_input->key = "input";
  opt_input->type = TYPE_STRING;
  opt_input->required = YES;
  opt_input->gisprompt = "old,cell,raster";
  opt_input->description = "GRASS input cell file name.";
  opt_input->answer = "";
  /* Request a pointer to memory for each option. */
  opt_output = G_define_option();
  opt_output->key = "output";
  opt_output->type = TYPE_STRING;
  opt_output->required = YES;
  opt_output->description = "ELAS output raster file name.";
  opt_output->answer = "";
  /* Using GRASS parsing to obtain arguments... */
  if (G_parser(argc, argv) != 0)
   {
    exit(1);
   }
  /* input file name */
  strcpy(name,opt_input->answer);
  /* output file name */
  strcpy(output,opt_output->answer);

  /* Check sizeof "CELL" and make sure it has the value of 4. */
  if ( sizeof(CELL) != 4 )
   {
    char msg[256];	
    sprintf(msg,"%s: GRASS typedef of \"CELL\" must have the value of 4.\n\
The size of \"CELL\" is %ld, which is incorrect.\n",
G_program_name(),sizeof(CELL) );
    G_fatal_error (msg);
    exit(1);
   }

  /* Determine whether to create an integer or floating-point ELAS file. */
  if ( (unsigned)(char)flag->answer == (unsigned)(char)0x00)
   {
    indicator = 'i';
   }
  else
   {
    indicator = 'f';
    fprintf(stderr,"INFORMATION:\n%s: Floating-point ELAS output file will be \
created...\n",
    G_program_name() );
    sleep(5);
   }

  /* Get current window "region". */
  if (G_get_window(&region) < 0)
   {
    char msg[256];	
    sprintf(msg,"%s: Unable to obtain current region.\n",G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Check current region for validity... */
  if ((region.north < region.south) || 
      (region.east  < region.west)  || 
      (region.ns_res <= 0.0      )  || 
      (region.ew_res <= 0.0      )  || 
      (region.rows   <= 0.0      )  || 
      (region.cols   <= 0.0      )    )
   {
    char msg[256];	
    sprintf(msg,"%s: Current region is invalid.\n",G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Determine if current region is an Unreferenced XY or a UTM projection. */
  if ((region.proj != 0)&&(region.proj != 1))
   {
    char msg[256];	
    sprintf(msg,"%s: Current region is neither an unreferened XY or a UTM\
 projection.\n",
    G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Save some of the original values in "region" in "orig_region". */
  orig_region.north = region.north;
  orig_region.south = region.south;
  orig_region.east = region.east;
  orig_region.west = region.west;
  orig_region.ns_res = region.ns_res;
  orig_region.ew_res = region.ew_res;

  /* Determine if current region's values are integers. */
  /* ELAS requires that its "window" be integers. */
  align = 0;
  /* If "align" remains equal to 0 then no realignment */
  /* will be necessary for current region.             */
  /* If "align" is set to 1, then realignment will be  */
  /* necessary for the current region in order to transfer */
  /* file to ELAS.                                         */
  if (region.ew_res != (double)(long)(int)region.ew_res) 
    align = 1;
  if (region.ns_res != (double)(long)(int)region.ns_res) 
    align = 1;
  if (region.north != (double)(long)(int)region.north) 
    align = 1;
  if (region.south != (double)(long)(int)region.south) 
    align = 1;
  if (region.east != (double)(long)(int)region.east) 
    align = 1;
  if (region.west != (double)(long)(int)region.west) 
    align = 1;
  /* If "align" equals 1 then current region needs to be */
  /* realigned with integer values as its values.        */
  /* ELAS requires its "window" to be integer values.    */
  if (align == 1)
   {
    ref.compressed = region.compressed;
    ref.proj = region.proj;
    ref.zone = region.zone;
    /* Recalculate "window" values as integers. */
    ref.ns_res = (double)(long)(int)(region.ns_res+0.499);
    ref.ew_res = (double)(long)(int)(region.ew_res+0.499);
    ref.north = (double) ( ( (long)(int)
                ((region.north/ref.ns_res)+0.499) )
                * ref.ns_res ); 
    ref.south = (double) ( ( (long)(int)
                ((region.south/ref.ns_res)+0.499) )
                * ref.ns_res ); 
    ref.east = (double) ( ( (long)(int)
               ((region.east/ref.ew_res)+0.499) )
               * ref.ew_res ); 
    ref.west = (double) ( ( (long)(int)
               ((region.west/ref.ew_res)+0.499) )
               * ref.ew_res ); 
    ref.rows = (ref.north - ref.south) / ref.ns_res;
    ref.cols = (ref.east  - ref.west ) / ref.ew_res;
    region.north = ref.north;
    region.south = ref.south;
    region.east = ref.east;
    region.west = ref.west;
    region.ns_res = ref.ns_res;
    region.ew_res = ref.ew_res;
    region.rows = ref.rows;
    region.cols = ref.cols;
    /* Set current window from "ref". */
    if (G_set_window(&ref) < 0)
     {
      char msg[256];	
      sprintf(msg,"%s: Unable to set current region.\n",G_program_name() );
      G_fatal_error (msg);
      exit(1);
     }
    /* Get current window and place those value is "region". */
    if (G_get_set_window(&region) < 0)
     {
      char msg[256];	
      sprintf(msg,"%s: Unable to get current region.\n",G_program_name() );
      G_fatal_error (msg);
      exit(1);
     }
   }

  /* Obtain the number of rows (nrows) and  the number of */
  /* columns (ncols) for the existing region (window).    */
  nrows = G_window_rows();
  ncols = G_window_cols();
  /* Check nrows */
  if ( nrows <= 0 )
   {
    char msg[256];	
    sprintf (msg, "%s: number of rows as %ld for cell file <%s> is incorrect.\n",
    G_program_name(), nrows, name);
    G_fatal_error (msg);
    exit(1);
   }
  /* Check ncols */
  if ( ncols <= 0 )
   {
    char msg[256];	
    sprintf (msg, "%s: number of columns as %ld for cell file <%s> is incorrect.\n",
    G_program_name(), ncols, name);
    G_fatal_error (msg);
    exit(1);
   }

  /* Get "mapset" with cell "name". */
  mapset = G_find_cell2 (name, "");
  if (mapset == NULL)
   {
    char msg[256];	
    sprintf (msg, "%s: <%s> cell file not found.\n\
Execute \"g.mapsets\" if necessary.\n",
    G_program_name(), name );
    G_fatal_error (msg);
    exit(1);
   }

  /* Determine if cell "name" is a reclass file or not. */
  if (G_is_reclass(name, mapset, r_name, r_mapset)==1)
   {
    char msg[256];	
    sprintf(msg,"%s: file: <%s> in mapset: %s is a reclass file.\n\
Please use \"r.resample\" to create a new cell file,\n\
then re-execute \"%s\".\n",
    G_program_name(),name,mapset,G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }
  else
   {
    if (G_is_reclass(name, mapset, r_name, r_mapset)!=0)
     {
      char msg[256];	
      sprintf (msg, "%s: Unable to determine if <%s> is a reclass file or \
not.\n",
      G_program_name(), name);
      G_fatal_error (msg);
      exit(1);
     }
   }

  /* Open existing cell filename stored in variable "name". */
  fd_in = G_open_cell_old (name, mapset);
  if (fd_in < 0)
   {
    char msg[256];	
    sprintf (msg, "%s: Unable to open cell file: <%s>.\n",
    G_program_name(), name);
    G_fatal_error (msg);
    exit(1);
   }

  /* Create ELAS output file that has the file name in variable "output". */
  if ((fd_out = creat(output, 0644 )) < 0)
   {
    char msg[256];	
    G_close_cell(fd_in);
    sprintf (msg, "%s: Unable to create output file: <%s>.\n",
    G_program_name(), output );
    G_fatal_error (msg);
    exit(1);
   }

  /* Allocate memory for the cell buffer "cell". */
  cell = G_allocate_cell_buf();

  /* Make sure that you are using unreferenced XY or UTM projection. */
  if ((G_projection() != 0)&&(G_projection() != 1))
   {
    char msg[256];	
    G_close_cell(fd_in);
    sprintf (msg, "%s: Projection is neither unreferenced XY or UTM.\n",
    G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Read cellhd information. */
  if (G_get_cellhd(name,mapset,&cellhd) != 0)
   {
    char msg[256];	
    G_close_cell(fd_in);
    sprintf (msg, "%s: Unable to read cellhd file <%s>.\n",
    G_program_name(),name );
    G_fatal_error (msg);
    exit(1);
   }

  /* Check cellhd values for validity... */
  if ((cellhd.north < cellhd.south) || 
      (cellhd.east  < cellhd.west)  || 
      (cellhd.ns_res <= 0.0      )  || 
      (cellhd.ew_res <= 0.0      )    )
   {
    char msg[256];	
    sprintf(msg,"%s: Values for cellhd file are invalid.\n",
    G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Make sure cellhd.format is correct. */
  if ( (cellhd.format > 3) || (cellhd.format < 0) )
   {
    char msg[256];	
    G_close_cell(fd_in);
    sprintf(msg,"%s: cellhd format: %d is incorrect for file <%s>.\n",
    G_program_name(),cellhd.format,name);
    G_fatal_error (msg);
    exit(1);
   }

  /* Determine if cellhd.proj is unreferenced XY or UTM. */
  if ((cellhd.proj != 0)&&(cellhd.proj != 1))
   {
    char msg[256];	
    G_close_cell(fd_in);
    sprintf(msg,"%s: cellhd proj: %d is not unreferenced XY or UTM \
projection.\n",
    G_program_name(),cellhd.proj);
    G_fatal_error (msg);
    exit(1);
   }

  /* Compare region.proj with cellhd.proj. */
  if (region.proj != cellhd.proj)
   {
    fprintf(stderr,"WARNING:\n%s: Current region proj: %d is NOT equal to \
cellhd proj: %d\n",
    G_program_name(),region.proj,cellhd.proj);
    sleep(5);
   }
   
  /* If region.proj = 0 (for unreferenced XY), then give warning. */
  if (region.proj == 0)
   {
    fprintf(stderr,"INFORMATION:\n%s: GRASS unreferenced XY will be \
transferred to an ELAS UTM projection.\n",
    G_program_name() );
    sleep(5);
   }

  /* Determine if "mask" is on or not. */
  fd_mask = G_maskfd();
  if (fd_mask != -1)
   {
    fprintf (stderr, "INFORMATION:\n%s: \"MASK\" file exists, thus the GRASS \
cell file <%s>\nwill be \"masked\" during the transfer to ELAS from GRASS.\n",
    G_program_name(), name);
    close(fd_mask);
    sleep(5);
   }

  /* Compare values in "cellhd" to values in "orig_region". */
  if ( (orig_region.north != cellhd.north)     ||
       (orig_region.south != cellhd.south)     ||
       (orig_region.east  != cellhd.east)      ||
       (orig_region.west  != cellhd.west)      ||
       (orig_region.ns_res  != cellhd.ns_res)  ||
       (orig_region.ew_res  != cellhd.ew_res)    )
   {
    fprintf(stderr,"WARNING:\n\
%s: The \"region's\" values are not equal to the \"cellhd's\" values for\n\
cell file <%s>.  The area of the GRASS cell file to be transferred to ELAS\n\
is determined by the region (WIND file).  All of the cell file might not\n\
have been transferred to ELAS due the region determining the area of the\n\
cell file to be transferred (which may be the desired result).  To ensure\n\
that the entire area of the cell file be transferred to ELAS, then prior to\n\
executing this program, run \"g.region\" and select: \"4 Set from a raster\n\
map\" and provide <%s> as the raster file name.\n", 
    G_program_name(), name, name);
    sleep(5);
   }

  /* Value "i" indicates integer output file to be created. */
  /* Value "f" indicates floating point output file to be created. */
  if (indicator == 'i')
   {
    /* integer-byte raster file to be created */
    /* element may be from 1 to 4 bytes */
    tot_cols = ((ncols*(cellhd.format+1)) / ELAS_multiple) * ELAS_multiple;
    temp_doub = (double) (((double)(ncols*(cellhd.format+1))
                / (double)ELAS_multiple) * (double) ELAS_multiple);
    if ( (double) tot_cols == temp_doub ) 
     {
      tot_cols = ((ncols*(cellhd.format+1)) / ELAS_multiple) * ELAS_multiple;
     }
    else
     {
      tot_cols = (((ncols*(cellhd.format+1)) / ELAS_multiple)+1)
                 * ELAS_multiple;
     }
    extra_cols = tot_cols - (ncols * (cellhd.format+1));      
   }
  else
   {
    /* floating-point-byte raster file to be created */
    /* element will be 4 bytes */
    tot_cols = ((ncols * 4) / ELAS_multiple) * ELAS_multiple;
    temp_doub = (double) ((((double)ncols*(double)4.0)
                / (double)ELAS_multiple) * (double) ELAS_multiple);
    if ( (double) tot_cols == temp_doub ) 
     {
      tot_cols = ((ncols*4) / ELAS_multiple) * ELAS_multiple;
     }
    else
     {
      tot_cols = (((ncols*4) / ELAS_multiple)+1) * ELAS_multiple;
     }
    extra_cols = tot_cols - (ncols * 4);      
   }

  /* Write out information for user concerning newly */
  /* aligned region for transfer to ELAS.            */
  if (align == 1)
   {
    fprintf(stderr,"INFORMATION:\n%s:\nGRASS region realigned to create ELAS \
\"window\" with integer values.\n\
GRASS region values used for transfer to ELAS are:\n\
North:          %f\n\
South:          %f\n\
East:           %f\n\
West:           %f\n\
NS Resolution:  %f\n\
EW Resolution:  %f\n\
Rows:           %d\n\
Columns:        %d\n\
ELAS values are:\n\
X-Offset=%ld Y-Offset=%ld\n\
il (initial line)=1  ll (last line)=%ld\n\
ie (initial element)=1  le(last element)=%ld\n",
    G_program_name(),region.north,region.south,region.east,region.west,
    region.ns_res,region.ew_res,region.rows,region.cols,
    (long) (int) (region.west  - region.ew_res), 
    (long) (int) (region.north + region.ns_res),nrows,ncols);
    sleep(5);
   }

  /* Create ELAS header of 1024 bytes...*/
  /* Create a new ELAS raster file whose name is stored in  */
  /* variable "output".                                     */
  elas.nbih = 1024;
  elas.nbpr = tot_cols;
  elas.il = 1;
  elas.ll = nrows;
  elas.ie = 1;
  elas.le = ncols;
  elas.nc = 1;
  elas.hri4321 = 4321;
  strcpy(elas.y_grid_desc,"NOR");   /* Descriptor for "UTM". */
  elas.y_offset = (long) (int) (region.north + region.ns_res); 
  strcpy(elas.x_grid_desc,"EAS");   /* Descriptor for "UTM". */
  elas.x_offset = (long) (int) (region.west  - region.ew_res); 
  elas.y_spot_size = (float) region.ns_res;
  elas.x_spot_size = (float) region.ew_res;
  /* UTM matrix */
  elas.matrix[0]=1.0; elas.matrix[1]=0.0;
  elas.matrix[2]=0.0; elas.matrix[3]=-1.0;
  if (indicator == 'i')
   {
    if ( cellhd.format == 0)
     {
      /* integer:  1 byte element */
      elas.ih19 = 0x04d20401;
     }
    else
     {
      if ( cellhd.format == 1)
       {
        /* integer:  2 byte element */
        elas.ih19 = 0x04d20402;
       }
      else
       {
        if ( cellhd.format == 2)
         {
          /* integer:  3 byte element */
          elas.ih19 = 0x04d20403;
         }
        else
         {
          if ( cellhd.format == 3)
           {
            /* integer:  4 byte element */
            elas.ih19 = 0x04d20404;
           }
         }
       }
     }
   }
  else
   {
    /* float:  4 byte element */
    elas.ih19 = 0x04d28004; 
   }
  elas.ih20 = 0;
  for (i=0; i < 4; i++)
   {
    *(elas.not_used_21 + i) = '\0';
    *(elas.not_used_22 + i) = '\0';
    *(elas.head + i) = '\0';
   }
  elas.labl=0;
  for (i=0; i < 32; i++)
   {
    *(elas.not_used_249_256 + i) = '\0';
   }

  /* For GRASS 1-byte cell files, transfer colors  */
  /* to ELAS header.                               */
  /* ELAS header can hold a maximum of 256 colors. */
  if ( cellhd.format == 0 )
   {
   /* Get GRASS colors... */
    colors_read = 1;
    if (G_read_colors(name,mapset,&colors)!=1)
     {
      fprintf(stderr,"INFORMATION:\n%s: Unable to read GRASS colors, so no \
GRASS colors\nwere transferred to the ELAS file header.\n",
      G_program_name());
      colors_read = 0;
      sleep(5);
     }
    /* Check colors.cmin and colors.cmax. */
    if (colors_read == 1)
     {
      if ( (colors.cmin < 0) ||
           (colors.cmax < 0) ||
           (colors.cmin > colors.cmax) )
       {
        fprintf(stderr,"INFORMATION:\n%s: Color values retrieved are incorrect.\
\nNo GRASS colors were transferred to the ELAS file header.\n",
        G_program_name() );
        colors_read = 0;
        sleep(5);
       }
     }
    /* If colors_read equals 1 then colors.cmin and colors.cmax */
    /* were properly retrieved.                                 */
    if (colors_read == 1)
     {
      elas_color_num=0;
      for (i = 0;  i < 256; i++)
       {
        cat = i;
        if (G_get_color(cat,&red,&green,&blue,&colors)==1)
         {
          elas.color_table[elas_color_num] = (unsigned)(short)(int)blue;
          blue = (long)(int) ( (float)(((double)blue / (double)255.)
                 * (double)15.)+0.499);
          blue = blue << 8;
          blue = blue &  0x00000f00;
          red = (long)(int) ( (float)(((double)red / (double)255.)
                * (double)15.)+0.499);
          red = red << 4;
          red = red & 0x000000f0;
          green = (long)(int) ( (float)(((double)green / (double)255.)
                  * (double)15.)+0.499);
          green = green &  0x0000000f;
          temp_short = (unsigned)(short)(int) ( blue | red);
          temp_short = (unsigned)(short)(int) ( temp_short | green);
          elas.color_table[elas_color_num] =  temp_short;
          elas_color_num += 1;
         }
        else
         {
          elas.color_table[elas_color_num] = (unsigned)(short)(int) 0;
          elas_color_num += 1;
         }
       }
     }
    else
     {
      /* color_read = 0, so no colors read from GRASS... */
      for (i = 0;  i < 256; i++)
       {
        elas.color_table[i] = (unsigned)(short)(int) 0;
       }
     }
   }
  else
   {
    /* cellhd.format is not equal to 0 */
    fprintf(stderr,"INFORMATION:\n%s: GRASS cell file: <%s> is a multi-byte \
cell,\nso no GRASS colors were transferred to the ELAS file header.\n",
    G_program_name(),name );
    sleep(5);
    for (i = 0;  i < 256; i++)
       elas.color_table[i]=(unsigned)(short)(int)0;
   }

  /* Write structure "elas" to the output file "fd_out". */
  if (write(fd_out,&elas,sizeof(elas)) != sizeof(elas))
   {
    perror("write error to output file.");
    G_close_cell(fd_in);
    close(fd_out);
    exit(1);;
   }
  /* Count total bytes written to file "output". */
  total_bytes = (double)(long)(int)sizeof(elas);

  /* If "indicator" equals "i" then create ELAS file as a 1 to 4 */
  /* byte integer per element raster file.                       */
  if (indicator == 'i')
   {
    /* Integer with 1 to 4 byte element */
    /* Calculate how many bits to shift the 4 byte field: */
    /* "temp_long" to the left by.                        */
    shift_left = (int)((4-(cellhd.format+1))*8);
    /* Loop by how many rows there are in current region. */
    for (row = 0; row < nrows; row++)
     {
      /* Retrieve the row... */
      if (G_get_map_row (fd_in, cell, row) < 0)
        exit(1);
      /* Loop by how many columns there are in current region. */
      /* Each column is four bytes. */
      /* 1 to 4 byte unsigned integer element */
      for (col = 0; col < ncols; col++)
       {
        temp_long = (unsigned) (long) *(cell+col);
        /* Write out "cellhd.format+1" bytes to file "fd_out" */
        /* Shift left value in temp_long by "shift_left" bits. */
        if ( shift_left > 0 )
          temp_long = temp_long << shift_left;
        /* Write out "cellhd.format+1" bytes to file "fd_out" */
        /* from value at address &temp_long).                 */
        if (write(fd_out,&temp_long,cellhd.format+1) != cellhd.format+1)
         {
          perror("write error to output file.");
          G_close_cell(fd_in);
          close(fd_out);
          exit(1);
         }
        total_bytes += (cellhd.format+1);
       }
      /* Write ELAS row as a multiple of "ELAS_multiple" bytes...       */
      /* If ncols is a multiple of "ELAS_multiple" then row will not be */
      /* extended by the following "for" loop.                          */
      for (i=(ncols*(cellhd.format+1)); i < tot_cols; i++)
       {
        if (write(fd_out,&empty_byte,1) != 1)
         {
          perror("write error to output file.");
          G_close_cell(fd_in);
          close(fd_out);
          exit(1);
         }
        total_bytes += 1;
       }
      /* Write out the percentage of rows processed... */
      G_percent((row+1),nrows,1);
     }
   }  
  else
   {
    /* If "indicator" equals "f" then create ELAS file as a 4 */
    /* byte floating point value per element raster file.     */
    /* Floating point with 4 byte element */
    /* Loop by how many rows there are in current region. */
    for (row = 0; row < nrows; row++)
     {
      /* Retrieve the row... */
      if (G_get_map_row (fd_in, cell, row) < 0)
        exit(1);
      /* Loop by how many columns there are in current region. */
      /* Each column is four bytes. */
      /* 4 byte floating point element */
      for (col = 0; col < ncols; col++)
       {
        temp_float = (float) (long) *(cell+col);
        /* Write out 4 bytes to file "fd_out"  */
        /* from value at address &temp_float . */
        if (write(fd_out,&temp_float,4) != 4)
         {
          perror("write error to output file.");
          G_close_cell(fd_in);
          close(fd_out);
          exit(1);
         }
        total_bytes += 4;
       }
      /* Write ELAS row as a multiple of "ELAS_multiple" bytes...       */
      /* If ncols is a multiple of "ELAS_multiple" then row will not be */
      /* extended by the following "for" loop.                          */
      for (i=(ncols*4); i < tot_cols; i++)
       {
        if (write(fd_out,&empty_byte,1) != 1)
         {
          perror("write error to output file.");
          G_close_cell(fd_in);
          close(fd_out);
          exit(1);
         }
        total_bytes += 1;
       }
      /* Write out the percentage of rows processed... */
      G_percent((row+1),nrows,1);
     }
   }  
  /* Calculate required_bytes. */
  if (indicator == 'i')
   {
    required_bytes = (double) nrows * ((double) ncols *
                     (double)(cellhd.format+1));
    required_bytes = required_bytes + ((double) nrows * (double) extra_cols);
    required_bytes = required_bytes + 1024;
   }
  else
   {
    required_bytes = (double) nrows * (double) (ncols*4);
    required_bytes = required_bytes + ((double) nrows * (double) extra_cols);
    required_bytes = required_bytes + 1024;
   }
  /* Compare required_bytes with total_bytes. */
  if (required_bytes != total_bytes)
   {  
    fprintf(stderr,"ERROR:\n%s: Bytes written to output file \"%s\" \
was: %.0f\nThe total bytes should be: %.0f\n\
Output file: \"%s\" is not correct...\n",
G_program_name(),output,total_bytes,required_bytes,output);
    G_close_cell(fd_in);
    close(fd_out);
    exit(1);
   }
  /* Close input and output files... */
  G_close_cell(fd_in);
  close(fd_out);
  /* An exit of 0 means program was successful...     */
  /* An exit of 1 means program was not successful... */
  exit(0);
 }
