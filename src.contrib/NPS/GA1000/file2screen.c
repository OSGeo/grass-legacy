/*****************************************************************************/
/* file2screen.c;   source for "file2screen"                                 */
/*                                                                           */
/* This program will display a graphics screen image on the Masscomp GA1000  */
/* from a file previously created by "screen2file".                          */
/* The default file name is "screenfile".                                    */
/* It is recommended that you use the option "-r" to save your registers of  */
/* your current graphics image on the screen.  And also it is recommended    */
/* that you use the "-r" option when you are displaying a file on the screen.*/
/* You do not have to use the "-r" option when you create an image on your   */
/* graphics screen that was created by such GRASS commands as:  Dcell or     */
/* Dvect.  The grass colors are the default color registers used when you    */
/* do not save the registers using the "-r" option.                          */
/* This program "file2screen" will return an exit status of 0 if successful. */
/* And it will return an exit status of 1 if NOT successful.                 */
/* You must be in universe att to compile this program, but you may be in    */
/* either att or ucb to execute the program.                                 */
/* You may have to modify "/usr/scripts/grass.colors" for variable "gc"      */
/* according to where your file "grass.colors" is located at.                */
/* You may have to modify CURSOR_OFF_0 and CURSOR_OFF_1 according to which   */
/* tty is the display terminal for each of your graphics processors.         */
/* It is recommended that you use the script "Demo" with these two programs: */
/* "screen2file" and "file2screen" to run demostrations on your GA1000.      */
/* To compile this program and create the executable "file2screen" issue the */
/* following commands:                                                       */
/* universe att                                                              */
/* /bin/cc file2screen.c -lgp -o file2screen                                 */
/* universe ucb                                                              */
/* To see the usage for the command "file2screen" simply type:               */
/*                file2screen                                                */
/*****************************************************************************/
#include <stdio.h>
#include <fcntl.h>
/* #include "/usr/.attinclude/libgpdefs.h" */
#include <libgpdefs.h>
/* Global Variables */
#define CURSOR_OFF_0 system("/bin/echo \033Gac > /dev/tty4");
#define CURSOR_OFF_1 system("/bin/echo \033Gac > /dev/tty5");
#define BUF 512
char reg_file[BUF];
FILE *fptr;
static char gc[BUF] = {"/usr/scripts/grass.colors"};
char filename[BUF];

main(argc,argv)
 int argc;
 char *argv[];
{
  char device[BUF];
  int x_left,x_right,y_bottom,y_top,placed;
  int fd, rcount, size;
  MGBB_DESC hostdesc;
  MGBB_DESC scrndesc;
  int len, i, slash, entire_len;

  if (argc != 2 && argc != 3 && argc != 4)
   {
    fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
    fprintf(stderr,"example:  %s        0\n",argv[0]);   
    fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
    fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
    fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
    fprintf(stderr,"note:     gp# must be 0 or 1\n");   
    fprintf(stderr,"note:     -r signifies \"use color registers from \"___R\" register file\"\n");
    fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
    exit(1);
   }
  if (argc == 2)
   {
    /* check argument 1 as to whether it equals 0 or 1 */
    if ( (strcmp(argv[1],"0") == 0) || (strcmp(argv[1],"1") == 0) )
     {
      sprintf(device,"/dev/gp%c",*(*(argv+1)+0) );
      /* default file name "screenfile" */
      strcpy(filename,"screenfile");
      /* open file "screenfile" as file to contain the 10 planes */
      if ((fd = open(filename, 0)) == -1)
       {
        fprintf(stderr,"Unable to open file:  %s\n",filename);
        exit(1);
       }
      strcpy(reg_file,gc);
      if ( (fptr = fopen(reg_file,"r")) == NULL)
       {
        fprintf(stderr,"Unable to open file:  %s\n",reg_file);
        close(fd);
        exit(1);
       }
     }
    else
     {
      fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
      fprintf(stderr,"example:  %s        0\n",argv[0]);   
      fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
      fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
      fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
      fprintf(stderr,"note:     gp# must be 0 or 1\n");   
      fprintf(stderr,"note:     -r signifies \"use color registers from \"___R\" register file\"\n");
      fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
      exit(1);
     }
   }
  else
    if (argc == 3)
     {
      if ( (strcmp(argv[1],"0") == 0) || (strcmp(argv[1],"1") == 0) || (strcmp(argv[1],"-r") == 0) )
       {
        if ( (strcmp(argv[1],"0") == 0) || (strcmp(argv[1],"1") == 0) )
         {
          if (strcmp(argv[2],"-r") == 0)
           {
            fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
            fprintf(stderr,"example:  %s        0\n",argv[0]);   
            fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
            fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
            fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
            fprintf(stderr,"note:     gp# must be 0 or 1\n");   
            fprintf(stderr,"note:     -r signifies \"use color registers from \"___R\" register file\"\n");
            fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
            exit(1);
           }
          sprintf(device,"/dev/gp%c",*(*(argv+1)+0) );
          strcpy(filename,argv[2]);
          if ((fd = open(filename, 0)) == -1)
           {
            fprintf(stderr,"Unable to open file:  %s\n",filename);
            exit(1);
           }
          strcpy(reg_file,gc);
          if ( (fptr = fopen(reg_file,"r")) == NULL)
           {
            fprintf(stderr,"Unable to open file:  %s\n",reg_file);
            close(fd);
            exit(1);
           }
         }
        else
          if ( (strcmp(argv[1],"-r") == 0) && ((strcmp(argv[2],"0")==0) || (strcmp(argv[2],"1")==0)) )
           {
            sprintf(device,"/dev/gp%c",*(*(argv+2)+0) );
            strcpy(filename,"screenfile");
            if ((fd = open(filename, 0)) == -1)
             {
              fprintf(stderr,"Unable to open file:  %s\n",filename);
              exit(1);
             }
            strcpy(reg_file,"screenfileR");
            if ( (fptr = fopen(reg_file,"r")) == NULL)
             {
              strcpy(reg_file,gc);
              if ( (fptr = fopen(reg_file,"r")) == NULL)
               {
                fprintf(stderr,"Unable to open file:  %s\n",reg_file);
                close(fd);
                exit(1);
               }
             }
           }
          else
           {
            fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
            fprintf(stderr,"example:  %s        0\n",argv[0]);   
            fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
            fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
            fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
            fprintf(stderr,"note:     gp# must be 0 or 1\n");   
            fprintf(stderr,"note:     -r signifies \"use color registers from \"___R\" register file\"\n");
            fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
            exit(1);
           }
       }
      else
       {
        fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
        fprintf(stderr,"example:  %s        0\n",argv[0]);   
        fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
        fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
        fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
        fprintf(stderr,"note:     gp# must be 0 or 1\n");   
        fprintf(stderr,"note:     -r signifies \"use color registers from \"___R\" register file\"\n");
        fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
        exit(1);
       }
     }
    else
      if (argc == 4)
       {
        if ( (strcmp(argv[1],"-r") == 0) && ((strcmp(argv[2],"0")==0)||(strcmp(argv[2],"1")==0)) ) 
         {
          if (strcmp(argv[3],"-r") == 0)
           {
            fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
            fprintf(stderr,"example:  %s        0\n",argv[0]);   
            fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
            fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
            fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
            fprintf(stderr,"note:     gp# must be 0 or 1\n");   
            fprintf(stderr,"note:     -r signifies \"use color registers from \"___R\" register file\"\n");
            fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
            exit(1);
           }
          strcpy(filename,argv[3]);
          if ( reg_file_name()==1)
            exit(1);
          sprintf(device,"/dev/gp%c",*(*(argv+2)+0) );
          if ((fd = open(filename, 0)) == -1)
           {
            fprintf(stderr,"Unable to open file:  %s\n",filename);
            exit(1);
           }
          if ( (fptr = fopen(reg_file,"r")) == NULL)
           {
            strcpy(reg_file,gc);
            if ( (fptr = fopen(reg_file,"r")) == NULL)
             {
              fprintf(stderr,"Unable to open file:  %s\n",reg_file);
              close(fd);
              exit(1);
             }
           }
         }
        else
         {
          fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
          fprintf(stderr,"example:  %s        0\n",argv[0]);   
          fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
          fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
          fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
          fprintf(stderr,"note:     gp# must be 0 or 1\n");   
          fprintf(stderr,"note:     -r signifies \"use color registers from \"___R\" register file\"\n");
          fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
          exit(1);
         }
       }

/* Uncomment this line if your system has only one GA1000 graphics processor */
/* sprintf(device,"/dev/gp0");                                               */

/* assign graphics processor */
  mgiasngp(device,0);
/* enable all planes */
  mgipln(-1);
/* replace */
  mgimodfunc(3,0,3);
/* get coordinate values of graphics screen */
  mgigetvcoor(2,&x_left,&y_bottom,&x_right,&y_top,&placed);
  if (placed != 0)
    {
     close(fd);
     close(fptr);
     mgideagp();
     exit(1);
    }
  /* check if gp is GA1000 */
  if (x_left!=0 || y_bottom!=0 || x_right!=1151 || y_top!=909)
   {
    fprintf(stderr,"graphics processor is not a GA1000.\n");
    close(fd);
    close(fptr);
    mgideagp();
    exit(1);
   }
/* define windows */
  mgidefw(3);
  mgipw( 3, 2, x_left, y_bottom, x_right, y_top );
  mgiv(3);
/* check the sizes of both files */
  if (check_sizes(filename) == 1)
   {
    close(fd);
    close(fptr);
    mgideagp();
    exit(1);
   }
/* clear all graphics planes */
/* mgiclearpln(2,-1,0);      */
/* show frame 1 and modify frame 1 */
  mgifb(1,1);
  if (set_reg() == 1)
   {
    close(fd);
    mgideagp();
    exit(1);
   }
/*Set up the screen descriptor*/
/* mgibbdescfbv(desc,mono,fbnum,nplanes,pmask) */
  mgibbdescfbv(&scrndesc, 0, 1, 1023);

/*Set up the host descriptor in which to save the image in memory*/
/* mgibbdes1(hostdesc,x_left,y_bottom,x_right,y_top,bitoff,mono,nplanes,pmask*/
  mgibbdesc1(&hostdesc,x_left,y_bottom,
             (int)(x_right+1),(int)(y_top+1),0,0,10,1023);
/*Allocate memory*/
  mgibballoc(&hostdesc);

/*Get the size of the image area for future use*/
  size = mgibbsize(&hostdesc);

/*Copy the array out to the named file*/
  rcount = read(fd, hostdesc.addr, size);
  close(fd);
/* check if file "filename" was read properly */
  if (rcount != 1310400)
   {
    fprintf(stderr,"File:  \"%s\" read incorrect number of bytes.\n");
    mgideagp();
    exit(1);
   }

/*Copy the entire screen to the descriptor named scrndesc*/
  mgibblt2(&hostdesc,0,0,&scrndesc,x_left,y_bottom,
           (int)(x_right+1),(int)(y_top+1),0,1023);
/*Sync GP with CPU _DO NOT FORGET TO DO THIS*/
  mgisyncrb(1);
  mgibbfree(&hostdesc);

/* turn off blinking cursor */  
  if (strcmp(device,"/dev/gp0")==0)
   {
    CURSOR_OFF_0
   }
  else
   if (strcmp(device,"/dev/gp1")==0)
    {
     CURSOR_OFF_1
    }

/* deassign graphics processor */
  mgideagp();
}



int
set_reg()
 {
  int i;
  long int colors[1024];

/* read color array from file:  reg_file */
  if ( (fread(colors, sizeof(colors), 1, fptr)) != 1)
   {
    fprintf(stderr,"Incorrectly read file:  %s.\n",reg_file);
    close(fptr);
    return(1);
   }
/* set colors and place in color registers 0 to 1023 */
  mgicms(0,1023,colors);
/* close file:  reg_file */
  close(fptr);
  return(0);
 }



#include <sys/types.h>
#include <sys/stat.h>
int
check_sizes()
 {
  struct stat stbuf;
  int error;

  error = 0;
  if ( stat(filename,&stbuf) == -1 )
   {
    if ( lstat(filename,&stbuf) == -1 )
     {
      if ( fstat(filename,&stbuf) == -1 )
       {
        fprintf(stderr,"Unable to obtain status for file: %s\n",filename);
        error = 1;
       }
     }
   }
  if (stbuf.st_size != 1310400)
   {
    fprintf(stderr,"File:  %s is not the correct size.\n",filename);
    error = 1;
   }
  if ( stat(reg_file,&stbuf) == -1 )
   {
    if ( lstat(reg_file,&stbuf) == -1 )
     {
      if ( fstat(reg_file,&stbuf) == -1 )
       {
        fprintf(stderr,"Unable to obtain status for file: %s\n",reg_file);
        error = 1;
       }
     }
   }
  if (stbuf.st_size != 4096)
   {
    fprintf(stderr,"File:  %s is not the correct size.\n",reg_file);
    error = 1;
   }
  if (error)
    return(1);
  else
    return(0);
 }



int
reg_file_name()
 {
  int len, i, slash, entire_len;
 
  entire_len = strlen(filename);
  slash = 0;
  for (i=entire_len; i >= 0; i--)
   {
    if ( *(filename+i) == '/')
     {
      slash = i + 1;
      break;
     }
   }
  len = entire_len - slash;
  if (len == 14)
   {
    if ( *(filename+(entire_len-1)) == 'R')
     {
      fprintf(stderr,"File name:  \"%s\"\ncan not have 14th letter as capital 'R' when using '-r' option.\n",filename);
      return(1);
     }
    strcpy(reg_file,filename);
    len = entire_len - 1;
    *(reg_file+len) = 'R';
   }
  else
   {
    strcpy(reg_file,filename);
    len = entire_len;
    *(reg_file+len) = 'R';
   }
  return(0);
 }
