/* Function: "main.c" for GRASS Program "r.in.elas"
   "r.in.elas" transfers ELAS raster files to GRASS cell files.

   Exit status of 0 indicates program was successful.
   Exit status of 1 indicates program was not successful.
   
   AUTHOR: Bruce Powell, National Park Service (NPS)
   http://www.nps.gov/
   
   "r.in.elas" and "r.out.elas" will transfer multi-byte raster files.  If you 
    would like information concerning these programs, please contact:
    Bruce Powell, NPS, 303 969 2590.
*/

#include <math.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"

int main( int argc, char *argv[])
 {
  int fd_out;
  int fd_in;
  int fd_mask;
  struct Cell_head region;
  struct Histogram histogram;
  struct Range range;
  CELL *cell;
  char output[1024];
  char *mapset;
  char input[256];
  long int row, col;
  struct Colors colors;
  CELL cat;
  long int red, green, blue;
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
  int i;
  int byte;
  int read_colors;
  struct Flag *flag;
  struct Option *opt_channel;
  struct Option *opt_zone;
  struct Option *opt_proj;
  struct Option *opt_nbpe;
  struct Option *opt_x_spot_size;
  struct Option *opt_y_spot_size;
  struct Option *opt_input;
  struct Option *opt_output;
  double elas_N, elas_S, elas_W, elas_E;
  unsigned char *buffer;
  long int chosen_channel, chan;
  long int utm_zone;
  int scan_int;
  unsigned long int temp_long;
  int nrows, ncols;
  long int cell_cnt;
  int bytes_read;
  int chosen_proj;
  FILE *fptr_pipe; 
  char command[512];
  long int file_bytes;
  long int calc_bytes;
  long int old_ll, old_le;
  long int nbpe;
  long int ls_args;
  char arg4[15], arg5[15];
  int exit_status;
  CELL min, max;
  CELL low, high;
  short int spot_error;
  unsigned long int mask4bytes;


  G_gisinit (argv[0]);
  
  /* Set description */
  module              = G_define_module();
  module->description = ""\
  "Import an ELAS raster file into a GRASS raster map.";
  
  /* Request a pointer to memory for each flag. */
  flag = G_define_flag();
  flag->key = 'h';
  flag->description = "Create GRASS histogram file.";
  flag->answer = 0x00;
  /* Request a pointer to memory for each option. */
  opt_channel = G_define_option();
  opt_channel->key = "channel";
  opt_channel->type = TYPE_INTEGER;
  opt_channel->required = NO;
  opt_channel->description = "ELAS channel number.";
  opt_channel->answer = "1";
  /* Request a pointer to memory for each option. */
  opt_zone = G_define_option();
  opt_zone->key = "zone";
  opt_zone->type = TYPE_INTEGER;
  opt_zone->required = NO;
  opt_zone->description = "UTM zone number. (1-60)";
  opt_zone->answer = "0";
  /* Request a pointer to memory for each option. */
  opt_proj = G_define_option();
  opt_proj->key = "proj";
  opt_proj->type = TYPE_INTEGER;
  opt_proj->required = NO;
  opt_proj->description = "GRASS \"proj\" value. (0 for XY and 1 for UTM)";
  opt_proj->answer = "1"; /* Default projection is UTM. */
  /* Request a pointer to memory for each option. */
  opt_nbpe = G_define_option();
  opt_nbpe->key = "nbpe";
  opt_nbpe->type = TYPE_INTEGER;
  opt_nbpe->required = NO;
  opt_nbpe->description = "(number of bytes per element) for ELAS file.\n\
\t\t\"nbpe\" is an OPTIONAL value, only to be inputted to override\n\
\t\tthe ELAS header value.";
  opt_nbpe->answer = "0";
  /* Request a pointer to memory for each option. */
  opt_x_spot_size = G_define_option();
  opt_x_spot_size->key = "x-spot-size";
  opt_x_spot_size->type = TYPE_INTEGER;
  opt_x_spot_size->required = NO;
  opt_x_spot_size->description = "ELAS X Spot size number.\n\
\t\t\"x-spot-size\" is an OPTIONAL value, only to be inputted to\n\
\t\toverride the ELAS header value."; 
  opt_x_spot_size->answer = "0";
  /* Request a pointer to memory for each option. */
  opt_y_spot_size = G_define_option();
  opt_y_spot_size->key = "y-spot-size";
  opt_y_spot_size->type = TYPE_INTEGER;
  opt_y_spot_size->required = NO;
  opt_y_spot_size->description = "ELAS Y Spot size number.\n\
\t\t\"y-spot-size\" is an OPTIONAL value, only to be inputted to\n\
\t\toverride the ELAS header value."; 
  opt_y_spot_size->answer = "0";
  /* Request a pointer to memory for each option. */
  opt_input = G_define_option();
  opt_input->key = "input";
  opt_input->type = TYPE_STRING;
  opt_input->required = YES;
  opt_input->description = "ELAS input raster file name.";
  opt_input->answer = "";
  /* Request a pointer to memory for each option. */
  opt_output = G_define_option();
  opt_output->key = "output";
  opt_output->type = TYPE_STRING;
  opt_output->required = YES;
  opt_output->gisprompt = "new,cell,raster";
  opt_output->description = "GRASS output cell file name.";
  opt_output->answer = "";
  /* Using GRASS parsing to obtain arguments... */
  if (G_parser(argc, argv) != 0)
   {
    exit(1);
   }

  /* Check sizeof "CELL" and make sure it has the value of 4. */
  if ( sizeof(CELL) != 4)
   {
    G_fatal_error ("%s: GRASS typedef of \"CELL\" must have the value of 4.\n\
The size of \"CELL\" is %ld, which is incorrect.\n",
G_program_name(),sizeof(CELL) );
    exit(1);
   }

  /* Assign value from "opt_channel->answer" to "chosen_channel". */
  scan_int=sscanf(opt_channel->answer,"%ld",&chosen_channel);
  if (scan_int <= 0)
   {
    char msg[256];	
    sprintf(msg,"%s: \"%s\" is an incorrect value for an ELAS channel \
number.\n", 
            G_program_name(),opt_channel->answer );
    G_fatal_error (msg);
    exit(1);
   }

  /* Assign value from "opt_zone->answer" to "utm_zone". */
  scan_int=sscanf(opt_zone->answer,"%ld",&utm_zone);
  if (scan_int <= 0)
   {
    char msg[256];	
    sprintf(msg,"%s: \"%s\" is an incorrect value for an ELAS zone number.\n", 
            G_program_name(),opt_zone->answer );
    G_fatal_error (msg);
    exit(1);
   }

  /* Assign value from "opt_proj->answer" to "chosen_proj". */
  scan_int=sscanf(opt_proj->answer,"%d",&chosen_proj);
  if (scan_int <= 0)
   {
    char msg[256];	
    sprintf(msg,"%s: \"%s\" is an incorrect value for a GRASS \"proj\" value.\n\
Use 0 for unreferenced XY and 1 for UTM.\n",
            G_program_name(),opt_proj->answer );
    G_fatal_error (msg);
    exit(1);
   }

  /* Make sure "chosen_proj" is equal to 0 or 1. */
  if ((chosen_proj!=0)&&(chosen_proj!=1))
   {
    char msg[256];	
    sprintf(msg,"%s: \"%s\" is an incorrect value for a GRASS \"proj\" value.\n\
Use 0 for unreferenced XY and 1 for UTM.\n",
            G_program_name(),opt_proj->answer );
    G_fatal_error (msg);
    exit(1);
   }

  /* Assign value from "opt_nbpe->answer" to "nbpe". */
  scan_int=sscanf(opt_nbpe->answer,"%ld",&nbpe);
  if (scan_int <= 0)
   {
    char msg[256];	
    sprintf(msg,"%s: \"%s\" is an incorrect value for \"nbpe\" .\n\
(number of bytes per element) for ELAS file.\n",    
    G_program_name(),opt_nbpe->answer );
    G_fatal_error (msg);
    exit(1);
   }

  /* Determine if "nbpe" has a valid value. */ 
  if (nbpe != 0)
   {
    /* "nbpe" must be from 1 to 4. */
    if ((nbpe < 0) || (nbpe > 4)) 
     {
      char msg[640];	
      sprintf(msg,"%s: \"%s\" is an incorrect value for \"nbpe\"\n\
When inputting \"nbpe\", \"nbpe\" must be a value from 1 to 4.\n\
You must execute \"%s\" non-interactively to provide\n\
a value for \"nbpe\".\n",
      G_program_name(),opt_nbpe->answer,G_program_name() );
      G_fatal_error (msg);
      exit(1);
     }
   }

  /* Assign value from "opt_x_spot_size->answer" to "x_spot_size". */
  scan_int=sscanf(opt_x_spot_size->answer,"%f",&(elas.x_spot_size));
  if (scan_int <= 0)
   {
    char msg[256];	
    sprintf(msg,"%s: \"%s\" is an incorrect value for \"x-spot-size\" value.\n",
            G_program_name(),opt_x_spot_size->answer );
    G_fatal_error (msg);
    exit(1);
   }

  /* Assign value from "opt_y_spot_size->answer" to "y_spot_size". */
  scan_int=sscanf(opt_y_spot_size->answer,"%f",&(elas.y_spot_size));
  if (scan_int <= 0)
   {
    char msg[256];	
    sprintf(msg,"%s: \"%s\" is an incorrect value for \"y-spot-size\" value.\n",
            G_program_name(),opt_y_spot_size->answer );
    G_fatal_error (msg);
    exit(1);
   }

  /* input file name */
  strcpy(input,opt_input->answer);

  /* output file name */
  strcpy(output,opt_output->answer);

  /* Provide warning message about UTM zone, since ELAS does not */
  /* store a UTM zone value.                                     */
  if (chosen_proj == 1)
   {
    if (utm_zone == 0)
     {
      fprintf (stderr,"WARNING:\n%s: ELAS header does not store a value for UTM \
  zone, 0 zone selected.\nAfter this execution, determine correct UTM zone and \
  run \"r.support\" and\nchoose: \"Edit the header for [%s]?\".\n",
      G_program_name(), output );
      sleep(5);
     }
    else
     {
      /* Make sure that the provided UTM zone value is between 1 and 60. */
      if ((utm_zone<1)||(utm_zone>60))
       {
        G_fatal_error ("%s: \"%ld\" is an incorrect UTM zone;\
 zone must be between 1 and 60.\n", G_program_name(),utm_zone );
        exit(1);
       }
     }
   }

  /* Make sure that "chosen_channel" is not less than or equal to 0. */
  if ( chosen_channel <= 0)
   {
    G_fatal_error ("%s: \"%ld\" is an incorrect value for an ELAS channel \
number.\n", 
            G_program_name(),chosen_channel );
    exit(1);
   }

  /* Determine if "name" is a legal GRASS file name or not. */
  if ( G_legal_filename(output) != 1)
   {
    G_fatal_error ("%s: <%s> is not a legal GRASS file name.\n",
            G_program_name(), output );
    exit(1);
   }

  /* Get "mapset". */
  mapset = G_mapset();

  /* Make sure that cell file "output" does not already exist in "mapset". */
  if (G_find_cell(output,mapset) != NULL )
   {
    G_fatal_error ("%s: <%s> cell file already exists.\n\
Please choose a different cell file name.\n",
    G_program_name(), output );
    exit(1);
   }

  /* Open ELAS input file that has the file name in variable "input". */
  if ((fd_in = open(input, 0644 )) < 0)
   {
    G_fatal_error ("%s: Unable to open ELAS input file:\n<%s>.\n",
    G_program_name(), input );
    exit(1);
   }

  /* Read 1024 byte ELAS header. */
  if(read(fd_in,&elas,sizeof(elas)) != sizeof(elas))
   {
    char msg[256];	
    close(fd_in);
    sprintf (msg, "%s: Unable to read ELAS file: <%s>.\n", 
             G_program_name(),input );
    G_fatal_error (msg);
    exit(1);
   }

  /* Make sure that you have 1 or more channels for the ELAS file. */
  if ( elas.nc < 1 )
   {
    close(fd_in);
    G_fatal_error ("%s: ELAS file <%s> has %ld channels.\n", 
             G_program_name(),input,elas.nc );
    exit(1);
   }

  /* Make sure that "chosen_channel" is not greater than "elas.nc". */
  if (chosen_channel > elas.nc)
   {
    close(fd_in);
    G_fatal_error ("%s: Selected ELAS channel of %ld is greater than \
total channels of %ld.\n",
             G_program_name(),chosen_channel,elas.nc );
    exit(1);
   }

  /* Provide message to user if chosen_channel is greater than one. */
  if (chosen_channel > 1)
   {
    fprintf(stderr,"INFORMATION:\n%s: You have selected ELAS channel %ld.\n",
    G_program_name(),chosen_channel);
    sleep(5);
   }

  /* Check number of bytes per record (nbpr) for ELAS file. */
  if ( elas.nbpr <= 0 )
   {
    close(fd_in);
    G_fatal_error ("%s: ELAS file <%s> has the number of bytes per \
record as %ld .\n", 
             G_program_name(),input,elas.nbpr );
    exit(1);
   }

  /* Make sure "nbpr" (number of bytes per record) is divisable by 256. */
  if ( (elas.nbpr % 256) != 0) 
   {
    G_fatal_error("%s: ELAS variable \"nbpr\" value is %ld and it is not a \
multiple of 256.\n",
    G_program_name(),elas.nbpr);
    exit(1);
   }

  /* If "nbpe" does not equal 0 then an "nbpe" value was provided by the user.*/
  if (nbpe != 0)
   {
    if ( ( elas.ih19 == 0x04d20401 ) ||
         ( elas.ih19 == 0x04d20402 ) ||
         ( elas.ih19 == 0x04d20403 ) ||
         ( elas.ih19 == 0x04d20404 )    )
     {
      fprintf(stderr,"WARNING:\n%s: ELAS variable \"ih19\" has the value of \
%08lx (hex).\nThis is a valid value and it is advised that inputted value\n\
\"nbpe\" should not override this \"ih19\" value in the ELAS header.\n",
      G_program_name(),elas.ih19);
      sleep(5);
     }
    /* Assign new values to "ih19" variable based on "nbpe" value. */
    if (nbpe == 1)
     {
      elas.ih19 = 0x04d20401;
     }
    else
     {
      if (nbpe == 2)
       {
         elas.ih19 = 0x04d20402;
       }
      else
       {
        if (nbpe == 3)
         {
           elas.ih19 = 0x04d20403;
         }
        else
         {
          if (nbpe == 4)
           {
            elas.ih19 = 0x04d20404;
           }
         }
       }
     }
    fprintf(stderr,"WARNING:\n%s: ELAS variable \"ih19\" changed so it now \
reflects\nthat the ELAS file is a %ld byte per element file.\n",
    G_program_name(),nbpe);
    sleep(5);
   }
  else
   {
    /* "nbpe" equals 0, so user did not supply a value for "nbpe". */
    /* Determine if ELAS file is integer with 1, 2, 3, or 4 bytes. */
    if ( ( elas.ih19 != 0x04d20401 ) &&
         ( elas.ih19 != 0x04d20402 ) &&
         ( elas.ih19 != 0x04d20403 ) &&
         ( elas.ih19 != 0x04d20404 )    )
     {
      char msg[512];
      close(fd_in);
      fprintf(stderr,"ERROR:\n%s: ELAS file <%s>\n\
has the value in variable \"ih19\" as: %08lx (hex) in the ELAS header.\n\
This \"ih19\" value does not indicate an ELAS 1-4 byte integer file.\n",
      G_program_name(),input,elas.ih19);
      sprintf(msg,"%s: \
Re-execute \"%s\" and provide a value from 1 to 4 for the variable \"nbpe\". \
\"nbpe\" will override the \"ih19\" value in the \nELAS header.  You must \
execute \"%s\" non-interactively to provide\na value for \"nbpe\".\n",
      G_program_name(), G_program_name(),G_program_name() );
      G_fatal_error(msg);
      exit(1);
     }
   }

  /* Obtain the number of bytes for the ELAS input file. */
  sprintf(command,"/bin/ls -l %s | /usr/bin/awk '{print NF,$4,$5}'",input);
  fptr_pipe = popen(command,"r");
  /* Obtain number of args (ls_args) from "ls" command. */
  scan_int = fscanf(fptr_pipe,"%ld %s %s",&ls_args,arg4,arg5);
  if (scan_int != 3)
   {
    G_fatal_error("%s: Unable to obtain number of bytes for ELAS file: <%s>.\n",
    G_program_name(),input);
    exit(1);
   }
  if (ls_args == 8)
   {
    /* Berkeley "ls" with 8 arguments produced with 4th arg being bytes. */
      scan_int = sscanf(arg4,"%ld",&file_bytes);
   }
  else
   {
    if (ls_args == 9)
     {
      /* AT&T "ls" with 9 arguments produced with 5th arg being bytes. */
      scan_int = sscanf(arg5,"%ld",&file_bytes);
     }
    else
     {
      scan_int = 0;
     }
    if (scan_int != 1)
     {
      G_fatal_error("%s: Unable to obtain number of bytes for ELAS file: <%s>.\n",
      G_program_name(),input);
      exit(1);
     }
   }
  pclose(fptr_pipe);

  /* Determine if number of bytes for the ELAS input file is evenly */
  /* divisable by "nbpr".                                           */
  if ( (((file_bytes - 1024)/elas.nc) % elas.nbpr) != 0)
   {
    char msg[512];	
    fprintf(stderr,"%s: Total byte size (without header) of (%ld-1024)/%ld(nc)\n\
for ELAS file: <%s> is not evenly divisible by %ld(nbpr).\n",
    G_program_name(),file_bytes,elas.nc,input,elas.nbpr);
    G_fatal_error(msg);
    exit(1);
   }

  /* Check if "nbih" (number of bytes in header) equals 1024. */
  if (elas.nbih != 1024)
   {
    char msg[512];	
    sprintf(msg,"%s: ELAS variable \"nbih\" value is %ld and does not equal\
 1024.\n",
    G_program_name(),elas.nbih);
    G_fatal_error(msg);
    exit(1);
   }

  /* Calculate the total bytes of the ELAS input file based on  the values */
  /* "ll", "nc", and "nbpr".                                               */
  calc_bytes = 1024 + ((elas.ll * elas.nc)*elas.nbpr);

  /* Determine if the calculated bytes for the ELAS input file (calc_bytes) */
  /* is equal to the actual bytes for the ELAS input file (file_bytes).     */
  if (file_bytes != calc_bytes)  
   {
    /* "file_bytes" does not equal "calc_bytes" so recalculate the "ll" value.*/
    old_ll = elas.ll;
    elas.ll = (long)(int) ((((double)file_bytes - 1024.0)
                  / (double)elas.nbpr)
                  / (double)elas.nc);
    fprintf(stderr,"WARNING:\n%s: ELAS variable \"ll\" value changed from \
%ld to %ld.\n",
    G_program_name(),old_ll,elas.ll);
    sleep(5);
   }

  /* Determine if "le" X "nbpe" is greater than "nbpr".  If it is then */   
  /* recalculate the value for "le".                                   */
  if ( elas.nbpr < (elas.le * nbpe))
   {
    old_le = elas.le;
    elas.le = old_le / nbpe;
    if ( (old_le % nbpe) != 0)
      elas.le += 1;
    /* Check if the new calculated "le" value is valid. */
    if ( elas.nbpr < (elas.le * nbpe))
     {
      char msg[512];	
      sprintf(msg,"%s: ELAS variables: %ld(le) X %ld(nbpe)= %d and\
 is greater than %ld(nbpr).\n",
      G_program_name(),old_le,nbpe,(int)(old_le * nbpe),elas.nbpr);
      G_fatal_error(msg);
      exit(1);
     }
    else
     {
      fprintf(stderr,"WARNING:\n%s: ELAS variable \"le\" value changed from \
%ld to %ld.\n",
      G_program_name(),old_le,elas.le);
      sleep(5);
     }
   }

  /* Check Y and X grid descriptions to determine if ELAS file is UTM. */
  if ( ( ( (*(elas.y_grid_desc+0)!='N')&&(*(elas.y_grid_desc+0)!='n') ) ||
         ( (*(elas.y_grid_desc+1)!='O')&&(*(elas.y_grid_desc+1)!='o') ) ||
         ( (*(elas.y_grid_desc+2)!='R')&&(*(elas.y_grid_desc+2)!='r') )   ) ||
       ( ( (*(elas.x_grid_desc+0)!='E')&&(*(elas.x_grid_desc+0)!='e') ) ||
         ( (*(elas.x_grid_desc+1)!='A')&&(*(elas.x_grid_desc+1)!='a') ) ||
         ( (*(elas.x_grid_desc+2)!='S')&&(*(elas.x_grid_desc+2)!='s') )   )   )
   {
    /* Only provide warning if "chosen_proj" is 1 (UTM). */
    if (chosen_proj == 1)
     {
      *(elas.y_grid_desc+3) = '\0'; 
      *(elas.x_grid_desc+3) = '\0'; 
      fprintf (stderr,"WARNING:\n%s: ELAS file <%s>\n\
  has \"%s\" as Y grid description, and \"%s\" as X grid description.\n",
      G_program_name(),input,elas.y_grid_desc,elas.x_grid_desc );
      fprintf (stderr,"\
  Y grid description should be \"NOR\" and X grid description should be \"EAS\"\n\
  to be a UTM projection.\n");
      sleep(5);
     }
   }

  /* Check if "y_offset" is 0. */
  if (elas.y_offset == 0)
   {
    fprintf(stderr,"WARNING:\n%s: ELAS \"Y-Offset\" value equals zero.\n",
    G_program_name() );
    sleep(5);
   }
   
  /* Check if "x_offset" is 0. */
  if (elas.x_offset == 0)
   {
    fprintf(stderr,"WARNING:\n%s: ELAS \"X-Offset\" value equals zero.\n",
    G_program_name() );
    sleep(5);
   }
  
  spot_error = 0; 
  /* Check X spot size for the ELAS file. */
  if (strcmp(opt_x_spot_size->answer,"0") != 0)
   {
    /* Override the ELAS header "x_spot_size" with user inputted */
    /* "x_spot_size".                                            */
    scan_int=sscanf(opt_x_spot_size->answer,"%f",&(elas.x_spot_size));
   }
  /* Make sure that the "x_spot_size" value is greater than zero. */
  if (elas.x_spot_size <= 0)
   {
    fprintf (stderr,"ERROR:\n%s: ELAS file <%s>\n\
has X spot size as %f.\n\
Re-execute \"%s\" and provide value for \"x-spot-size\".\n\
You must execute \"%s\" non-interactively to provide\n\
a value for \"x-spot-size\".\n",
    G_program_name(),input,elas.x_spot_size,G_program_name(),G_program_name() );
    spot_error = 1;
   }
   
  /* Check Y spot size for the ELAS file. */
  if (strcmp(opt_y_spot_size->answer,"0") != 0)
   {
    /* Override the ELAS header "y_spot_size" with user inputted */
    /* "y_spot_size".                                            */
    scan_int=sscanf(opt_y_spot_size->answer,"%f",&(elas.y_spot_size));
   }
  /* Make sure that the "y_spot_size" value is greater than zero. */
  if (elas.y_spot_size <= 0)
   {
    fprintf (stderr,"ERROR:\n%s: ELAS file <%s>\n\
has Y spot size as %f.\n\
Re-execute \"%s\" and provide value for \"y-spot-size\".\n\
You must execute \"%s\" non-interactively to provide\n\
a value for \"y-spot-size\".\n",
    G_program_name(),input,elas.y_spot_size,G_program_name(),G_program_name() );
    spot_error = 1;
   }

  /* If "x_spot_size" or "y_spot_size" are incorrect then exit. */
  if (spot_error)
   {
    exit(1);
   }

  /* "ll" can not be less than "il" and         */
  /* neither "il" and "ll" can be less than or  */
  /* equal to zero.                             */
  if ( (elas.ll < elas.il ) ||
       (elas.il <= 0)       || 
       (elas.ll <= 0)          )
   {
    char msg[256];	
    close(fd_in);
    sprintf (msg, "%s: ELAS file <%s> has \"il\" equal to %ld,\
\nand \"ll\" equal to %ld .\n", 
             G_program_name(),input,elas.il,elas.ll );
    G_fatal_error (msg);
    exit(1);
   }

  /* "le" can not be less than "ie" and         */
  /* neither "ie" and "le" can be less than or  */
  /* equal to zero.                             */
  if ( (elas.le < elas.ie ) ||
       (elas.ie <= 0)       || 
       (elas.le <= 0)          )
   {
    char msg[256];	
    close(fd_in);
    sprintf (msg, "%s: ELAS file <%s> has \"ie\" equal to %ld,\
\nand \"le\" equal to %ld .\n", 
             G_program_name(),input,elas.ie,elas.le );
    G_fatal_error (msg);
    exit(1);
   }

  /* Calculate N, S, E, and W values for ELAS file. */
  /* These new values will be used for the new GRASS region. */
  elas_N = ((double)elas.y_offset - (double)elas.y_spot_size)
           - ((double)(elas.il-1) * (double)elas.y_spot_size);
  elas_S = ((double)elas.y_offset - (double)elas.y_spot_size)
           - ((double)elas.ll * (double)elas.y_spot_size);
  elas_E = (double)elas.x_offset 
           + ((double)(elas.le+1) * (double)elas.x_spot_size);
  elas_W = (double)elas.x_offset 
           + ((double)elas.ie * (double)elas.x_spot_size);

  /* Adjust "region" values when using XY Projection. */
  if (chosen_proj == 0)
   {
    if ( (elas.y_spot_size != 1.0) || (elas.x_spot_size != 1.0) )
     {
      if (elas.x_spot_size != 1.0)
       {
        fprintf(stderr,"WARNING:\n%s: ELAS X Spot size is %f and this value \n\
should be 1 when using XY Projection.\n",
        G_program_name(), elas.x_spot_size );
       }
      if (elas.y_spot_size != 1.0)
       {
        fprintf(stderr,"WARNING:\n%s: ELAS Y Spot size is %f and this value \n\
should be 1 when using XY Projection.\n",
        G_program_name(), elas.y_spot_size );
       }
      fprintf(stderr,"%s: The values for the GRASS region may be incorrect as a \
result!\n",G_program_name());
      sleep(7);
     }
   else
     {
      /* These values are only adjusted when using XY Projection and 
         y_spot_size and x_spot_size are equal to 1.                 */
      elas_N -= ((double)elas.y_spot_size / (double)2.);
      elas_S -= ((double)elas.y_spot_size / (double)2.);
      elas_E += ((double)elas.x_spot_size / (double)2.);
      elas_W += ((double)elas.x_spot_size / (double)2.);
     }
   }
  /* Assign ELAS values to GRASS "region". */
  region.north = elas_N;
  region.south = elas_S;
  region.east  = elas_E;
  region.west  = elas_W;
  region.ns_res= (double) elas.y_spot_size;
  region.ew_res= (double) elas.x_spot_size;
  region.proj= chosen_proj;
  region.rows = (double)elas.ll - (double)(elas.il-1);
  region.cols = (double)elas.le - (double)(elas.ie-1);

  /* Assign values to "region.format" based on value in variable "ih19". */
  if ( elas.ih19 == 0x04d20401 )
   {
    region.format=0;
   }
  else
   {
    if ( elas.ih19 == 0x04d20402 )
     {
      region.format=1;
     }
    else
     {
      if ( elas.ih19 == 0x04d20403 )
       {
        region.format=2;
       }
      else
       {
        if ( elas.ih19 == 0x04d20404 )
         {
          region.format=3;
         }
       }
     } 
   }

  /* Assign chosen "utm_zone" to "region.zone". */
  region.zone = utm_zone;

  /* Set current window from "region". */
  if (G_set_window(&region) < 0)
   {
    char msg[256];	
    close(fd_in);
    sprintf(msg,"%s: Unable to set current region.\n",G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Open new cell file name stored in variable "output". */
  fd_out = G_open_cell_new(output);
  if (fd_out < 0)
   {
    char msg[256];	
    close(fd_in);
    sprintf (msg, "%s: Unable to open GRASS cell file: <%s>.\n",
    G_program_name(), output);
    G_fatal_error (msg);
    exit(1);
   }

  /* Allocate memory for "buffer".*/
  /* "buffer" will have "nbpr" bytes. */
  buffer = (unsigned char *) malloc (elas.nbpr); 
  if (buffer == NULL)
   {
    char msg[256];	
    close(fd_in);
    close(fd_out);
    sprintf(msg,"%s: Unable to allocate memory for variable \"buffer\".\n",
            G_program_name() );
    G_fatal_error (msg);
    exit(1);
   }

  /* Obtain the number of rows (nrows) and  the number of */
  /* columns (ncols) for the existing region (window).    */
  nrows = G_window_rows();
  ncols = G_window_cols();

  /* Check nrows */
  if ( nrows <= 0 )
   {
    char msg[256];	
    close(fd_in);
    close(fd_out);
    sprintf (msg, "%s: Number of rows as %d for GRASS cell file <%s> \
is incorrect.\n",
    G_program_name(), nrows, output);
    G_fatal_error (msg);
    exit(1);
   }

  /* Check ncols */
  if ( ncols <= 0 )
   {
    char msg[256];	
    close(fd_in);
    close(fd_out);
    sprintf (msg, "%s: Number of columns as %d for GRASS cell file <%s> is\
 incorrect.\n",
    G_program_name(), ncols, output );
    G_fatal_error (msg);
    exit(1);
   }

  /* Make sure that the calculated ELAS "rows" equals "nrows". */
  if ( (int)nrows != (int)(elas.ll-(elas.il-1)) )
   {
    char msg[256];	
    close(fd_in);
    close(fd_out);
    sprintf (msg, "%s: Calculated ELAS rows of %d is not equal to\
 GRASS region's rows of %d.\n", 
    G_program_name(), (int)(elas.ll-(elas.il-1)),nrows);
    G_fatal_error (msg);
    exit(1);
   }

  /* Make sure that the calculated ELAS "columns" equals "ncols". */
  if ( (int)ncols != (int)(elas.le-(elas.ie-1)) )
   {
    char msg[256];	
    close(fd_in);
    close(fd_out);
    sprintf (msg, "%s: Calculated ELAS columns of %d is not equal to\
 GRASS region's columns of %d.\n", 
    G_program_name(), (int)(elas.le-(elas.ie-1)),ncols);
    G_fatal_error (msg);
    exit(1);
   }
  
  /* Provide message to user if histogram file will be created. */
  if ( (unsigned)(char)flag->answer == (unsigned)(char)0x01)
   {
    fprintf(stderr,"INFORMATION:\n%s: A GRASS histogram file will be created.\n", 
    G_program_name() );
    sleep(5);
   }

  /* Determine if "mask" is on or not. */
  fd_mask = G_maskfd();
  if (fd_mask != -1)
   {
    fprintf (stderr, "INFORMATION:\n%s: \"MASK\" file exists, thus the GRASS \
cell file <%s>\nwill be \"masked\" during the transfer from ELAS to GRASS.\n",
    G_program_name(), output);
    close(fd_mask);
    sleep(5);
   }

  /* Allocate memory for the cell buffer "cell". */
  cell = G_allocate_cell_buf();

  /* Write warning message if only part of the ELAS file is being */
  /* transferred to GRASS.                                        */
  if ( (elas.ie != 1) || (elas.il != 1) )
   {
    fprintf (stderr,"WARNING:\n\
%s: Only part of the ELAS file has been transferred to GRASS (which may\n\
be the desired result).  ELAS variable \"ie\" (initial element)= %ld and ELAS\n\
variable \"il\" (initial line)= %ld.  Change both values to 1 prior to\n\
executing this program, if you want the entire area of the ELAS file to be\n\
transferred to GRASS.\n",
    G_program_name(),elas.ie,elas.il );
    sleep(5);
   }

  /* Read from ELAS file and write to GRASS file. */
  if ( (elas.ih19 == 0x04d20401 ) ||
       (elas.ih19 == 0x04d20402 ) ||
       (elas.ih19 == 0x04d20403 ) ||
       (elas.ih19 == 0x04d20404 )   )
   {
    /* Determine if creation of histogram file is turned on. */
    if ( (unsigned)(char)flag->answer == (unsigned)(char)0x01)
     {
      exit_status=G_init_histogram(&histogram);
     }
    mask4bytes = 0xffffffff;
    if (region.format < 3)
     {
      if (region.format == 0)
       {
        mask4bytes =  0x000000ff;
       }
      else
       {
        if (region.format == 1)
         {
          mask4bytes = 0x0000ffff;
         }
        else
         {
          if (region.format == 2)
           {
            mask4bytes = 0x00ffffff;
           }
         }
       }
     }
    /* ELAS integer 1, 2, 3, or 4 byte element. */
    for (row=(elas.il-1); row < elas.ll; row++)
     {
      for (chan=1; chan <= elas.nc; chan++)
       {
        /* If current channel "chan" is the "chosen_channel" then            */
        /* transfer values from "buffer" to "temp_long" and then to "cell". */
        if (chan == chosen_channel)
         {
          /* Read from ELAS input file for a total of "nbpr" bytes. */
          if ((bytes_read=read(fd_in,buffer,(long)(int)elas.nbpr)) < 0 )
           {
            char msg[256];	
            close(fd_in);
            close(fd_out);
            sprintf(msg,"%s: Reading of ELAS input file <%s> was \
unsuccessful.\n",
                    G_program_name(), input );
            G_fatal_error (msg);
            exit(1);
           }
          /* Transfer values from "buffer" to "temp_long". */
          cell_cnt=0;
          for (col=((elas.ie-1)*(region.format+1));
               col < (elas.le*(region.format+1)); col += (region.format+1))
           {
            temp_long=0;
            /* Transfer the "bytes" in "buffer" to "temp_long". */
            for (byte=col; byte < (col+(region.format+1)); byte++)
             {
              temp_long = temp_long << 8;
              temp_long = temp_long | (long)(int) *(buffer+byte);
             }
            /* "mask" value in "temp_long" variable. */
            temp_long = temp_long & mask4bytes;
            /* Transfer value from "temp_long" to "cell[cell_cnt]". */
            cell[cell_cnt] = (CELL) temp_long;
            /* Determine if creation of histogram file is turned on. */
            if ( (unsigned)(char)flag->answer == (unsigned)(char)0x01)
              exit_status=G_add_histogram((CELL)temp_long,(int)1,&histogram);
            /* Count number of "columns" in row "cell". */
            cell_cnt += 1;
           }
          /* Write "cell" (one row) to GRASS output file. */
          G_put_raster_row(fd_out,cell, CELL_TYPE);
         }
        else
         {
          /* Skip the channels not being read using "lseek". */
          lseek(fd_in,(long)elas.nbpr,(int)1);
         }
       }
      G_percent((row+1),elas.ll,1);
     }
   }

  /* Close input file. */
  close(fd_in);

  /* Close output file. */
  /* "G_close_cell"  will automatically execute "G_put_cellhd". */
  G_close_cell(fd_out);

  /* Determine if creation of histogram file is turned on. */
  if ( (unsigned)(char)flag->answer == (unsigned)(char)0x01)
   {
    /* Sort the values in the histogram. */
    exit_status=G_sort_histogram(&histogram);
    /* Write out the values in the histogram to the file name in "output". */
    exit_status=G_write_histogram(output,&histogram);
    /* Free memory used for the the histogram. */
    exit_status=G_free_histogram(&histogram);
   }

  read_colors = 0;
  /* Determine if there are some color values in ELAS header. */
  for (i=0; i < 256; i++)
   {
    if ( elas.color_table[i] != (unsigned) (short) (int) 0 )
     {
      /* If "read_colors" equals 1, then there are some color  */
      /* values in the ELAS header.                            */
      read_colors = 1;
      break;
     }
   }

  /* Write warning message if there are no colors in ELAS header. */
  if (read_colors == 0)
   {
    fprintf (stderr,"INFORMATION:\n\
No color values in ELAS header, no colors transferred to GRASS.\n\
A GRASS color file was created for you though (using a RGB color ramp).\n\
After this execution, to change your color file, run \
\"r.support\" and select: \n\"Create/Update the color table for [%s]?\".\n",
    G_program_name() );
    sleep(5);
   }

  /* Obtain the minimum and maximum values of categories. */
  G_read_range(output,mapset,&range);
    min = range.min;
    max = range.max;

  /* Determine the highest and lowest possible value for categories. */ 
  if (read_colors == 1)
   {
    low=0; high=255;
   }
  else
   {
    low=0; high = (CELL) (pow(2.,(double) (8 * (region.format+1) )) - 1); 
   }

  /* Make sure that min and max values do not go beyond possible range. */
  if(min>high)min=high; if(min<low)min=low;
  if(max>high)max=high; if(max<low)max=low;
  if(min>max)max=min;   if(max<min)min=max;

  /* Transfer ELAS color values to GRASS.          */
  /* ELAS header can hold a maximum of 256 colors. */
  if ( read_colors == 1 )
   {
    for (cat = min;  cat <= max; cat++)
     {
      blue = elas.color_table[cat]; 
      blue = blue & 0x00000f00;
      blue = blue >> 8;
      blue = blue & 0x0000000f;
      blue = (long)(int) ( (float)(((double)blue / (double)15.)
              * (double)255.));
      red = elas.color_table[cat]; 
      red = red & 0x000000f0;
      red = red >> 4;
      red = red & 0x0000000f;
      red = (long)(int) ( (float)(((double)red / (double)15.)
             * (double)255.));
      green = elas.color_table[cat]; 
      green = green & 0x0000000f;
      green = (long)(int) ( (float)(((double)green / (double)15.)
               * (double)255.));
      G_add_color_rule((CELL)cat,red,green,blue,
                       (CELL)cat,red,green,blue,&colors);
     }
   }
  else
   {
    /* Create "color ramp table". */
    exit_status=G_make_ramp_colors(&colors,min,max);
   }

  /* Create color file "output". */
  if (G_write_colors(output,mapset,&colors) == -1)
   {
    fprintf (stderr,"WARNING:\n%s: Unable to create color file: <%s>.\n\
After this execution, to create color file, run \"r.support\" and select:\n\
 \"Create/Update the color table for [%s]?\".\n",
    G_program_name(), output, output );
    sleep(5);
   }
  /* Free up memory used by "colors". */
  exit_status=G_free_colors(&colors);

  /* Create a new "WIND" file and provide message to user. */
  exit_status=G_put_window(&region);
  if (exit_status==1)
   {
    fprintf(stderr,"WARNING:\n\
%s: Your \"WIND\" file (region) has been changed to match the\n\
\"cellhd\" file for GRASS file <%s>.\n",
    G_program_name(), output);
    sleep(5);
   }

  /* If user provided a value for "nbpe" then the ELAS values may have been */ 
  /* recalculated.  So provide the user with information concerning the     */
  /* recalculated ELAS values that were used for the transfer.              */
  /* If "nbpe" is not equal to 0, then user supplied "nbpe" value.          */
  if (nbpe != 0)
   {
    fprintf(stderr,"\nINFORMATION:\n");
    fprintf(stderr,"ELAS values used in creating new GRASS cell file \
<%s>:\n",output);
    fprintf(stderr,"nbpe(number of bytes per element)=%ld\n",nbpe);
    fprintf(stderr,"nbih(number of bytes in header)=%ld\n",elas.nbih);
    fprintf(stderr,"nbpr(number of bytes per record)=%ld\n",elas.nbpr); 
    fprintf(stderr,"nc(total number of channels)=%ld\n",elas.nc); 
    fprintf(stderr,"il(initial line)=%ld\n",elas.il);
    fprintf(stderr,"ll(last line)=%ld\n",elas.ll);
    fprintf(stderr,"ie(initial element)=%ld\n",elas.ie);
    fprintf(stderr,"le(last element)=%ld\n",elas.le);
    fprintf(stderr,"Y-Offset=%ld\n",elas.y_offset);
    fprintf(stderr,"X-Offset=%ld\n",elas.x_offset);
    fprintf(stderr,"Y-Spot-Size=%f\n",elas.y_spot_size);
    fprintf(stderr,"X-Spot-Size=%f\n",elas.x_spot_size);
    fprintf(stderr,"ELAS channel used was %ld.\n",chosen_channel); 
    if ( chosen_proj == 0)
      fprintf(stderr,"Projection used was unreferenced XY.\n");
    if ( chosen_proj == 1)
     {
      fprintf(stderr,"Projection used was UTM.\n");
      fprintf(stderr,"UTM zone used was %ld.\n",utm_zone);
     }
   }

  /* An exit of 0 means program was successful...     */
  /* An exit of 1 means program was not successful... */
  exit(0);
 }
