/*****************************************************************************/
/* screen2file.c;   source for "screen2file"                                 */
/*                                                                           */
/* This program will save the graphics screen image from the Masscomp GA1000 */
/* into a file.  The default file name is "screenfile".                      */
/* It is recommended that you use the option "-r" to save your registers of  */
/* your current graphics image on the screen.  And also it is recommended    */
/* that you use the "-r" option when you are displaying a file on the screen.*/
/* You do not have to use the "-r" option when you create an image on your   */
/* graphics screen that was created by such GRASS commands as:  Dcell or     */
/* Dvect.  The grass colors are the default color registers used when you    */
/* do not save the registers using the "-r" option.                          */
/* This program "screen2file" will return an exit status of 0 if successful. */
/* And it will return an exit status of 1 if NOT successful.                 */
/* You must be in universe att to compile this program, but you may be in    */
/* either att or ucb to execute the program.                                 */
/* You may have to modify "/usr/scripts/grass.colors" for variable "gc"      */
/* according to where your file "grass.colors" is located at.                */
/* It is recommended that you use the script "Demo" with these two programs: */
/* "screen2file" and "file2screen" to run demostrations on your GA1000.      */
/* To compile this program and create the executable "screen2file" issue the */
/* following commands:                                                       */
/* universe att                                                              */
/* /bin/cc screen2file.c -lgp -o screen2file                                 */
/* universe ucb                                                              */
/* To see the usage for the command "screen2file" simply type:               */
/*                screen2file                                                */
/*****************************************************************************/
#include <stdio.h>
#include <fcntl.h>
/* #include "/usr/.attinclude/libgpdefs.h" */
#include <libgpdefs.h>
/* Global Variables */
#define BUF 512
char reg_file[BUF];
FILE *fptr;
long int colors[1024];
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
  int make_reg;

  if (argc != 2 && argc != 3 && argc != 4)
   {
    fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
    fprintf(stderr,"example:  %s        0\n",argv[0]);   
    fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
    fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
    fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
    fprintf(stderr,"note:     gp# must be 0 or 1\n");   
    fprintf(stderr,"note:     -r signifies \"save current color registers in \"___R\" register file\"\n");
    fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
    exit(1);
   }
  make_reg = 0;
  if (argc == 2)
   {
    /* check argument 1 as to whether it equals 0 or 1 */
    if ( (strcmp(argv[1],"0") == 0) || (strcmp(argv[1],"1") == 0) )
     {
      sprintf(device,"/dev/gp%c",*(*(argv+1)+0) );
      /* default file name "screenfile" */
      strcpy(filename,"screenfile");
      /* open file "screenfile" as file to contain the 10 planes */
      if ((fd = open(filename, O_WRONLY|O_CREAT|O_TRUNC, 0666)) == -1)
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
      fprintf(stderr,"note:     -r signifies \"save current color registers in \"___R\" register file\"\n");
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
            fprintf(stderr,"note:     -r signifies \"save current color registers in \"___R\" register file\"\n");
            fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
            exit(1);
           }
          sprintf(device,"/dev/gp%c",*(*(argv+1)+0) );
          strcpy(filename,argv[2]);
          if ((fd = open(filename, O_WRONLY|O_CREAT|O_TRUNC, 0666)) == -1)
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
            if ((fd = open(filename, O_WRONLY|O_CREAT|O_TRUNC, 0666)) == -1)
             {
              fprintf(stderr,"Unable to open file:  %s\n",filename);
              exit(1);
             }
            strcpy(reg_file,"screenfileR");
            if ( (fptr = fopen(reg_file,"w+")) == NULL)
             {
              fprintf(stderr,"Unable to open file:  %s\n",reg_file);
              close(fd);
              exit(1);
             }
            make_reg = 1;
           }
          else
           {
            fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
            fprintf(stderr,"example:  %s        0\n",argv[0]);   
            fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
            fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
            fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
            fprintf(stderr,"note:     gp# must be 0 or 1\n");   
            fprintf(stderr,"note:     -r signifies \"save current color registers in \"___R\" register file\"\n");
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
        fprintf(stderr,"note:     -r signifies \"save current color registers in \"___R\" register file\"\n");
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
            fprintf(stderr,"note:     -r signifies \"save current color registers in \"___R\" register file\"\n");
            fprintf(stderr,"note:     default file name is:  \"screenfile\"\n");
            exit(1);
           }
          strcpy(filename,argv[3]);
          if ( reg_file_name()==1)
            exit(1);
          sprintf(device,"/dev/gp%c",*(*(argv+2)+0) );
          if ((fd = open(filename, O_WRONLY|O_CREAT|O_TRUNC, 0666)) == -1)
           {
            fprintf(stderr,"Unable to open file:  %s\n",filename);
            exit(1);
           }
          if ( (fptr = fopen(reg_file,"w+")) == NULL)
           {
            fprintf(stderr,"Unable to open file:  %s\n",reg_file);
            close(fd);
            exit(1);
           }
          make_reg = 1;
         }
        else
         {
          fprintf(stderr,"usage:    %s [-r] gp# [filename]\n",argv[0]);   
          fprintf(stderr,"example:  %s        0\n",argv[0]);   
          fprintf(stderr,"example:  %s  -r    1\n",argv[0]);   
          fprintf(stderr,"example:  %s        0   myfile\n",argv[0]);   
          fprintf(stderr,"example:  %s  -r    1   myfile\n",argv[0]);   
          fprintf(stderr,"note:     gp# must be 0 or 1\n");   
          fprintf(stderr,"note:     -r signifies \"save current color registers in \"___R\" register file\"\n");
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
/* show frame 1 and modify frame 1 */
  mgifb(1,1);
  if (make_reg)
   {
    if (get_reg() == 1)
     {
      close(fd);
      mgideagp();
      exit(1);
     }
   }
  if (set_reg() == 1)
   {
    close(fd);
    mgideagp();
    exit(1);
   }
/*Set up the screen descriptor*/
/* mgibbdescfbv(desc,mono,fbnum,nplanes,pmask) */
  mgibbdescfbv(&scrndesc, 0, 1, 1023);

/*Set up the host descriptor in which to save the screen*/
/* mgibbdes1(hostdesc,x_left,y_bottom,x_right+1,y_top+1,bitoff,mono,nplanes,pmask*/
  mgibbdesc1(&hostdesc,x_left,y_bottom,
           (int)(x_right+1),(int)(y_top+1),0,0,10,1023);
/*Allocate memory*/
  mgibballoc(&hostdesc);

/*Get the size of the image area for future use*/
  size = mgibbsize(&hostdesc);

/*Copy the entire screen to the descriptor named hostdesc*/
  mgibblt2(&scrndesc,0,0,&hostdesc,x_left,y_bottom,
           (int)(x_right+1),(int)(y_top+1),0,1023);

/*Sync GP with CPU _DO NOT FORGET TO DO THIS*/
  mgisyncrb(1);

/*Copy the array out to the named file*/
  rcount = write(fd, hostdesc.addr, size);
  close(fd);
  mgibbfree(&hostdesc);
  mgideagp();
 }



int
 get_reg()
 {
  int i;

/* get colors and place in colors array */
  mgigetcms(0,1023,colors);
/* write color array to file: reg_file */
  fwrite(colors, sizeof(colors), 1, fptr);
/* Rewind file "reg_file" to beginning */
  fseek(fptr,0,0);
  return(0);
 }



int
set_reg()
 {
  int i;

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
