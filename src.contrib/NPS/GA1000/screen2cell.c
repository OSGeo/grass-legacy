/*****************************************************************************/
/* screen2cell.c;   source for "screen2cell"                                 */
/*                                                                           */
/* This program will save the graphics screen image from the Masscomp GA1000 */
/* and create a GRASS cell file in "cell" directory and also its supporting  */
/* files in directories: "cellhd", "colr", and "cats".  The file "WIND" will */
/* also be created.  This program will prompt for the GRASS variables        */
/* "GISDBASE", "LOCATION", and "MAPSET".  It will get the default variables  */
/* for "GISDBASE" and "LOCATION" from the ".grassrc" in your home directory. */
/* If file ".grassrc" does not exist then the default values will come from  */ 
/* the variables "DEFAULT_GISDB" and "DEFAULT_LOC" in this source code.      */
/* You may have to modify variables "DEFAULT_GISDB" and "DEFAULT_LOC" for    */
/* your particular system.                                                   */
/* It should be noted that this program, "screen2cell" is capable of saving  */
/* all 1024 colors from the Masscomp GA1000 and converting that image to     */
/* GRASS files.  The GRASS cell file may be either a single-byte or multi-   */
/* byte (2 byte) file.                                                       */
/* Also the GRASS files created will be in the "xy" projection to ensure you */
/* do not treat the image as being georeferenced.                            */
/* The default file name is "screencell" if no file name is passed with the  */
/* "screen2cell" command.                                                    */
/* This program "screen2cell" will return an exit status of 0 if successful. */
/* And it will return an exit status of 1 if NOT successful.                 */
/* You must be in universe att to compile this program, but you may be in    */
/* either att or ucb to execute the program.                                 */
/* To compile this program and create the executable "screen2cell" issue the */
/* following commands:                                                       */
/* universe att                                                              */
/* /bin/cc screen2cell.c -lgp -o screen2cell                                 */
/* universe ucb                                                              */
/* To see the usage for the command "screen2cell" simply type:               */
/*                screen2cell                                                */
/*****************************************************************************/

#define DEFAULT_GISDB "/data/grass"
#define DEFAULT_LOC   "spearfish"

/* #include "/usr/.attinclude/libgpdefs.h" */
#include <libgpdefs.h>
#include <signal.h>
#include <stdio.h>
#include <fcntl.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>
#define BUF128 128
#define BUF256 256
#define BUF512 512

/* GLOBAL VARIABLES */
MGBB_DESC hostdesc;
MGBB_DESC scrndesc;
static char prog_name  [BUF128];
static char cell_file  [BUF256];
static char cell2_file [BUF256];
static char cellhd_file[BUF256];
static char colr_file  [BUF256];
static char cats_file  [BUF256];
static char wind_file  [BUF256];
static char filename   [BUF128];
static long int colors [1024];
static unsigned short int ptr_1024[1024];
static unsigned short int ptr_256 [256];
static int ptr_count1024;
static int ptr_count256;
static int fd2, fd, size;
static int format;
static FILE *fptr;
static FILE *fptr2;
static FILE *fptr3;
static FILE *fptr4;
static FILE *fptr5;
static FILE *mail_ptr;

main(argc,argv)
 int argc;
 char *argv[];
 {
  extern int capture_image();
  int quit();
  int interrupt();
  int hangup();
  int termination();
  int return_value;
  int len;
  char location[BUF256];
  char device[BUF128];
  char command  [BUF512];
  int errorM;
  int errorW;

  if (argc != 2 && argc != 3)
   {
    fprintf(stderr,"usage:    %s gp# [filename]\n",argv[0]);   
    fprintf(stderr,"example:  %s  0\n",argv[0]);   
    fprintf(stderr,"example:  %s  1\n",argv[0]);   
    fprintf(stderr,"example:  %s  0   myfile\n",argv[0]);   
    fprintf(stderr,"example:  %s  1   myfile\n",argv[0]);   
    fprintf(stderr,"note:     gp# must be 0 or 1\n");   
    fprintf(stderr,"note:     default file name is:  \"screencell\"\n");
    exit(1);
   }
  if (argc == 2)
   {
    /* check argument 1 as to whether it equals 0 or 1 */
    if ( (strcmp(argv[1],"0") == 0) || (strcmp(argv[1],"1") == 0) )
     {
      sprintf(device,"/dev/gp%c",*(*(argv+1)+0) );
      return_value = grass_loc(location);
      if (return_value == 1)
        exit(1);
      /* default file name "screencell" */
      strcpy(filename,"screencell");
      if (mk_tmp(location) == 1)
        exit(1);
      fprintf(stderr,"note:     default file name is:  \"screencell\"\n");
      sprintf(cell2_file,"%s/SC%d",cell2_file, getpid() );
      sprintf(cell_file,"%s/%s",cell_file,filename);
      sprintf(cellhd_file,"%s/%s",cellhd_file,filename);
      sprintf(colr_file,"%s/%s",colr_file,filename);
      sprintf(cats_file,"%s/%s",cats_file,filename);
      sprintf(wind_file,"%s/WIND",location);
      strcpy(prog_name,argv[0]);
     }
    else
     {
      fprintf(stderr,"usage:    %s gp# [filename]\n",argv[0]);   
      fprintf(stderr,"example:  %s  0\n",argv[0]);   
      fprintf(stderr,"example:  %s  1\n",argv[0]);   
      fprintf(stderr,"example:  %s  0   myfile\n",argv[0]);   
      fprintf(stderr,"example:  %s  1   myfile\n",argv[0]);   
      fprintf(stderr,"note:     gp# must be 0 or 1\n");   
      fprintf(stderr,"note:     default file name is:  \"screencell\"\n");
      exit(2);
     }
   }
  else
    if (argc == 3)
     {
      if ( (strcmp(argv[1],"0") == 0) || (strcmp(argv[1],"1") == 0) ) 
       {
        sprintf(device,"/dev/gp%c",*(*(argv+1)+0) );
        return_value = grass_loc(location);
        if (return_value == 1)
          exit(1);
        strcpy(filename,argv[2]);
      if (mk_tmp(location) == 1)
        exit(1);
        sprintf(cell2_file,"%s/SC%d",cell2_file, getpid() );
        sprintf(cell_file,"%s/%s",cell_file,filename);
        sprintf(cellhd_file,"%s/%s",cellhd_file,filename);
        sprintf(colr_file,"%s/%s",colr_file,filename);
        sprintf(cats_file,"%s/%s",cats_file,filename);
        sprintf(wind_file,"%s/WIND",location);
        strcpy(prog_name,argv[0]);
       }
      else
       {
        fprintf(stderr,"usage:    %s gp# [filename]\n",argv[0]);   
        fprintf(stderr,"example:  %s  0\n",argv[0]);   
        fprintf(stderr,"example:  %s  1\n",argv[0]);   
        fprintf(stderr,"example:  %s  0   myfile\n",argv[0]);   
        fprintf(stderr,"example:  %s  1   myfile\n",argv[0]);   
        fprintf(stderr,"note:     gp# must be 0 or 1\n");   
        fprintf(stderr,"note:     default file name is:  \"screencell\"\n");
        exit(1);
       }
     }
    else
     {
      fprintf(stderr,"usage:    %s gp# [filename]\n",argv[0]);   
      fprintf(stderr,"example:  %s  0\n",argv[0]);   
      fprintf(stderr,"example:  %s  1\n",argv[0]);   
      fprintf(stderr,"example:  %s  0   myfile\n",argv[0]);   
      fprintf(stderr,"example:  %s  1   myfile\n",argv[0]);   
      fprintf(stderr,"note:     gp# must be 0 or 1\n");   
      fprintf(stderr,"note:     default file name is:  \"screencell\"\n");
      exit(1);
     }


  signal (SIGINT,interrupt);   
  signal (SIGQUIT,quit);   
  signal (SIGHUP,hangup);   
  signal (SIGTERM,termination);   

  fprintf(stderr,"\nStep 1: Capture image from screen...\n\n"); 
  return_value = capture_image(device);

  if (return_value == 1)
    exit(1);

/* Notify user that mail will be sent when processing is done. */
  fprintf(stderr,"\nStep 2: Convert captured image to GRASS files.\n"); 
  fprintf(stderr,"\n        You will be notified by mail when processing is complete.\n\n");

  signal (SIGINT, SIG_IGN);   
  signal (SIGQUIT,SIG_IGN);   
  signal (SIGHUP, SIG_IGN);   
  signal (SIGTERM,SIG_IGN);   

/* fork to background */
   if (fork() > 0) exit(1);

  errorM = 0;

/* Call function "build_CELL2" to build 2 byte cell file
   called "cell2_file" */
  if (build_CELL2() == 1)
   {
    close(fd2);
    unlink(cell2_file);
    errorM = 1;
   }
  if (errorM == 0)
   {
    if (format == 0)
     {
      if (build_CELL1() == 1)
       {
        close(fd2);
        unlink(cell2_file);
        unlink(cell_file);
        errorM = 1;
       }
     }
   }
/* close fd2 "cell2_file" */
   close(fd2);

  if (errorM == 0)
   {
    if (build_COLR() == 1)
     {
      unlink(cell2_file);
      errorM = 1;
     }
   }

  if (errorM == 0)
   {
    if (build_CELLHD() == 1)
     {
      if (format == 0)
        unlink(cell_file);
      unlink(cell2_file);
      unlink(cellhd_file);
      errorM = 1;
     }
   }

  if (errorM == 0)
   {
    if (build_CATS() == 1)
     {
      if (format == 0)
        unlink(cell_file);
      unlink(cell2_file);
      unlink(cellhd_file);
      unlink(cats_file);
      errorM = 1;
     }
   }
 
  if (errorM == 0)
   {
    if (build_WIND() == 1)
     {
      fprintf(stderr,"Unsuccessful in producing \"WIND\" file in directory \"%s\"\n",location);
      errorW = 1;
     }
    else
      errorW = 0;
   }

  if (errorM == 0)
   {
    if (format==0)
     {
      unlink(cell2_file);
     }
    else
     {
      if (format==1)
       {
        sprintf(command,"/bin/mv %s %s &",cell2_file,cell_file);
        system(command);
       }
     }
   }
  mail_ptr = popen("mail `whoami`","w");
  if (errorM == 0)
   {
fprintf(mail_ptr," %s: Screen conversion to GRASS files SUCCESSFUL!\n",prog_name);
fprintf(mail_ptr," %s: GRASS files: \"%s\" located in directory:\n \"%s\"\n",
          prog_name,filename,location);
    if (errorW == 1)
      fprintf(stderr," %s: Unsuccessful in producing \"WIND\" file in directory \"%s\"\n",prog_name,location);
   }
  else
   {
fprintf(mail_ptr," %s: Screen conversion to GRASS files UNSUCCESSFUL!\n",prog_name);
fprintf(mail_ptr," %s: Unsuccessful attempt made in directory:\n %s\n",prog_name,location);
   }
  pclose(mail_ptr);
 }


int
capture_image(device)
  char device[];
 {
  int x_left,x_right,y_bottom,y_top,placed;

/* open file "cell2_file" as file to contain the 10 planes */
  if ((fd2 = open(cell2_file, O_RDWR|O_CREAT|O_TRUNC, 0666)) == -1)
   {
    fprintf(stderr,"Unable to open file:  %s\n",cell2_file);
    return(1);
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
     close(fd2);
     mgideagp();
     return(1);
    }
  /* check if gp is GA1000 */
  if (x_left!=0 || y_bottom!=0 || x_right!=1151 || y_top!=909)
   {
    fprintf(stderr,"graphics processor is not a GA1000.\n");
    close(fd2);
    mgideagp();
    return(1);
   }
/* define windows */
  mgidefw(3);
  mgipw( 3, 2, x_left, y_bottom, x_right, y_top );
  mgiv(3);
/* show frame 1 and modify frame 1 */
  mgifb(1,1);
  if (get_reg() == 1)
   {
    close(fd2);
    mgideagp();
    return(1);
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

/* Sync GP with CPU... DO NOT FORGET TO DO THIS!! */
  mgisyncrb(1);

#ifdef DEBUG
fprintf(stderr,"hostdesc.mtyp.host= %x\n",hostdesc.mtyp.host);
fprintf(stderr,"hostdesc.mtyp.view= %x\n",hostdesc.mtyp.view);
fprintf(stderr,"hostdesc.mtyp.mono= %x\n",hostdesc.mtyp.mono);
fprintf(stderr,"hostdesc.mtyp.reserve= %x\n",hostdesc.mtyp.reserve);
fprintf(stderr,"hostdesc.mtyp.alloc= %x\n",hostdesc.mtyp.alloc);
fprintf(stderr,"hostdesc.mtyp= %x\n",hostdesc.mtyp);
fprintf(stderr,"hostdesc.fbnum= %d\n",hostdesc.fbnum);
fprintf(stderr,"hostdesc.nplanes= %d\n",hostdesc.nplanes);
fprintf(stderr,"hostdesc.mask= %x\n",hostdesc.mask);
fprintf(stderr,"hostdesc.addr= %x\n",hostdesc.addr);
fprintf(stderr,"hostdesc.image.bl.x=%d\n",hostdesc.image.bl.x);
fprintf(stderr,"hostdesc.image.bl.y=%d\n",hostdesc.image.bl.y);
fprintf(stderr,"hostdesc.image.width=%d\n",hostdesc.image.width);
fprintf(stderr,"hostdesc.image.height=%d\n",hostdesc.image.height);
fprintf(stderr,"hostdesc.bitoff= %d\n",hostdesc.bitoff);
fprintf(stderr,"hostdesc.step= %d\n",hostdesc.step);
fprintf(stderr,"hostdesc.pln_bytes= %d\n",hostdesc.pln_bytes);
#endif DEBUG
/* print out MGBB_DESC values */
  mgideagp();
  return(0);
 }



int
 get_reg()
 {
  int i;

/* get colors and place in colors array */
  mgigetcms(0,1023,colors);
  return(0);
 }


int
build_CELL2()
 {
  int i, j, k;
  unsigned char *plane[10];
  unsigned char *b[10];
  unsigned short int bytes[8];
  unsigned short int temp;
  static unsigned short int mask[8] = { 0x0001,0x0002,0x0004,0x0008,0x0010,0x0020,0x0040,0x0080 };
  int m;
  unsigned char ptr_char[1024];

/* Set all values in "ptr_char" array to 0x00 */
  for (i=0; i < 1024; i++)
   {
    ptr_char[i] = (unsigned)(char) 0x00;
   }
/* Set all values in "ptr_256" array to -1 */
  for (i=0; i < 256; i++)
   {
    ptr_256[i] = -1;
   }
/* assign addresses of each of the 10 planes to the array "plane" */
  for (i=(hostdesc.nplanes-1); i >=0 ; i--)
   {
    plane[9-i]=(unsigned char *)(hostdesc.addr+(i*hostdesc.pln_bytes));
   }

/* loop thru each of the bytes of each of the 10 planes;  131040 bytes/plane */
/* There are 1,048,320 bits per plane with each of
   those bits being 10 bits deep */

  for (j=0; j < hostdesc.pln_bytes; j++)
   { 
/* place byte from position "j" from each of the 10 planes into the array "b" */
/* variable "i" corresponds to each of the 10 planes (0-9) */
/* variable "j" corresponds to each byte of each of the planes */
/* 910 bits * 1152 bits = 1,048,320 bits = 131,040 bytes is j */
/* Array b holds 10 bytes; with each byte being 8 bits for a total of 80 bits */
    for (i=0; i < hostdesc.nplanes; i++)
      b[i] = plane[i]+j;
       
/* rearrange the 8 bits from 10 bytes into 8 bytes with 10 bits */
/* transfer 10 bit value to 2 byte variable "bytes" */
/* The original 10 bytes of 8 bits each becomes 8 2-byte fields */
    for (i=0; i < 8; i++)
     {
      bytes[7-i] = 0;
      for (k=9; k >= 0; k--)
       {
        temp = 0;
        temp = (unsigned) (short) (int) *b[9-k]; 
        temp = temp & mask[i];
        if (i < k)
          temp = temp << (k-i);
        else
          if (i == k)
            temp = temp;
          else
            if (i > k)
              temp = temp >> (i-k); 
        bytes[7-i] = bytes[7-i] + temp;
       }
     }
    for (m=0; m < 8; m++)
      ptr_char[ bytes[m] ] = (unsigned)(char)0x01;
    if (write(fd2,bytes,sizeof(bytes)) != sizeof(bytes))
     {
      perror("write error");
      return(1);
     }
   }

  ptr_count1024=0;
/* Place the location where the 0-1023 number is in the 
   array "ptr_256" so that this can be used later to convert
   the 2-byte(0-1023) number to a 1-byte(0-255) number and
   and then written to the one-byte cell file */ 
  for (i=0; i < 1024; i++)
   {
    if (ptr_char[i] == (unsigned)(char) 0x01)
     {
      ptr_1024[i] = ptr_count1024;
      ptr_count1024+=1;
     }
    else
     ptr_1024[i] = -1;
   }
  if (ptr_count1024 <= 256)
   {
    format=0;
    ptr_count256=0;
/* count 0x01 values in "ptr_char" array and place the the
   total number into variable "ptr_count256" */
/* place the location of the 0-1023 value in the "ptr_256" array
   so this can be used later to locate the 0-1023 location in
   array "colors" */
    for (i=0; i < 1024; i++)
     {
      if (ptr_char[i] == (unsigned)(char) 0x01)
       {
        ptr_256[ptr_count256] = i;
        ptr_count256 += 1; 
       }
     }
   }
  else
    format=1;
  return(0);
 }



int
build_CATS()
 {
  int ctot_less1;

  if ( (fptr5 = fopen(cats_file,"w")) == NULL)
   {
    printf("Unable to open file \"%s\"\n",cats_file);
    return(1);
   }
  if (ptr_count1024 > 0)
   ctot_less1 = ptr_count1024 - 1;
  else
   ctot_less1 = 0;
  fprintf(fptr5,"# %d categories\n",ctot_less1);
  fprintf(fptr5,"%s\n",filename);
  fprintf(fptr5,"\n");
  fprintf(fptr5,"0.00 0.00 0.00 0.00\n");
  fprintf(fptr5,"0:no data\n");
  close(fptr5);
  return(0);
 }


int
build_COLR()
 {
  int i;
  unsigned long int red;
  unsigned long int grn;
  unsigned long int blu;
  static unsigned short int neg_one = { -1 };

  if ( (fptr2 = fopen(colr_file,"w")) == NULL)
   {
    printf("Unable to open file '%s'\n",colr_file);
    return(1);
   }

  fprintf(fptr2,"#1 first color\n");
  if (format == 1)
   {
/* write RGB values to file "colr_file" */
    for (i=0; i < 1024; i++)
     {
      red = colors[i];
      red = red & 0x00ff0000; 
      red = red >> 16;
      grn = colors[i];
      grn = grn & 0x0000ff00;
      grn = grn >>  8;
      blu = colors[i];
      blu = blu & 0x000000ff;
      fprintf(fptr2,"%d %d %d\n",red,grn,blu);
     }
   }
  else
    if (format == 0)
     {
      for (i=0; i < ptr_count1024; i++)
       {
        if (ptr_256[i] != neg_one )
         {
          red = colors[ ptr_256[i] ];
          red = red & 0x00ff0000; 
          red = red >> 16;
          grn = colors[ ptr_256[i] ];
          grn = grn & 0x0000ff00;
          grn = grn >>  8;
          blu = colors[ ptr_256[i] ];
          blu = blu & 0x000000ff;
          fprintf(fptr2,"%d %d %d\n",red,grn,blu);
         }
       }
     }
  close(fptr2);
  return(0);
 }


int
build_CELLHD()
 {

  if ( (fptr3 = fopen(cellhd_file,"w")) == NULL)
   {
    printf("Unable to open file '%s'\n",cellhd_file);
    return(1);
   }
  fprintf(fptr3,"proj:       0\n");
  fprintf(fptr3,"zone:       1\n");
  fprintf(fptr3,"north:      0000910.00\n");
  fprintf(fptr3,"south:      0000000.00\n");
  fprintf(fptr3,"west:       000000.00\n");
  fprintf(fptr3,"east:       001152.00\n");
  fprintf(fptr3,"n-s resol:  01.00\n");
  fprintf(fptr3,"e-w resol:  01.00\n");
  fprintf(fptr3,"format:     %d\n",format);
  fprintf(fptr3,"compressed: 0\n");
  close(fptr3);
  return(0);
 }


int
build_WIND()
 {

  if ( (fptr4 = fopen(wind_file,"w")) == NULL)
   {
    printf("Unable to open file '%s'\n",wind_file);
    return(1);
   }
  fprintf(fptr4,"proj:       0\n");
  fprintf(fptr4,"zone:       1\n");
  fprintf(fptr4,"north:      0000910.00\n");
  fprintf(fptr4,"south:      0000000.00\n");
  fprintf(fptr4,"east:       001152.00\n");
  fprintf(fptr4,"west:       000000.00\n");
  fprintf(fptr4,"e-w resol:  01.00\n");
  fprintf(fptr4,"n-s resol:  01.00\n");
  close(fptr4);
  return(0);
 }

int
build_CELL1()
 {
  unsigned short int short_int;
  unsigned char one_char;
  int i;
  int bytes_read;

  if (format == 0)
   {
    /* open file "CELL" as file to contain the 10 planes */
    if ((fd = open(cell_file, O_WRONLY|O_CREAT|O_TRUNC, 0666)) == -1)
     {
      fprintf(stderr,"Unable to open file:  %s\n",cell_file);
      return(1); 
     }
    lseek(fd2,0,0);
    for (i=0; i < (2096640/2); i++)
     {
       bytes_read = read(fd2,&short_int,2);
       if (bytes_read != 2)
       { 
        fprintf(stderr,"read error\n");
        close(fd);
        return(1);
       }
      one_char = (unsigned)(char)(ptr_1024[short_int]&0x00ff);
      if (write(fd,&one_char,1) != 1)
       { 
        perror("write error");
        close(fd);
        return(1);
       }
     }
    close(fd);
   }
  return(0);
 }


int
grass_loc(location)
  char location[];
 {
  int return_value;
  int error, error2;
  struct stat stbuf;
  int mkdir(path,mode);
  char *path;
  static int mode = { 0777 };
  static int mask = { 0022 };
  int umask(mask);
  char gisdbase[BUF128];
  char location_name[BUF128];
  char mapset[BUF128];

  return_value=get_grass_loc(location);
  if (return_value == 0)
   {
    sprintf(cell_file,"%s/cell",location);
    sprintf(cellhd_file,"%s/cellhd",location);
    sprintf(colr_file,"%s/colr",location);
    sprintf(cats_file,"%s/cats",location);
   }
  else
   {
    return(1);
   }

  error  = 0;
  error2 = 0;
  umask(mask);

/* Determine if directory "cell_file" exists */
  if ( stat(cell_file,&stbuf) == -1 )
   {
    path = cell_file;
    return_value = mkdir(path,mode);
    if (return_value != 0)
     {
  fprintf(stderr,"Unable to write in directory \"%s\"\n",
          location);
      error2 = 1;
      error  = 1;
     }
    else
     {
      fprintf(stderr,"Created directory \"cell\" in directory \"%s\"\n",
              location);
     }
   }
  else
   {
    if ( (stbuf.st_mode & S_IFMT) != S_IFDIR)
     {
      fprintf(stderr,"\"%s\" is NOT a directory.\n",cell_file);
      error = 1;
     }
   }

/* Determine if directory "cellhd_file" exists */
  if ( stat(cellhd_file,&stbuf) == -1 )
   {
    if (error2 == 0)
     {
      path = cellhd_file;
      return_value = mkdir(path,mode);
      if (return_value != 0)
       {
   fprintf(stderr,"Directory: \"%s\" does not exist and unable to create it.\n",
           cellhd_file);
        error2 = 1;
        error  = 1;
       }
      else
       {
        fprintf(stderr,"Created directory \"cellhd\" in directory \"%s\"\n",
                location);
       }
     }
   }
  else
   {
    if ( (stbuf.st_mode & S_IFMT) != S_IFDIR)
     {
      fprintf(stderr,"\"%s\" is NOT a directory.\n",cellhd_file);
      error = 1;
     }
   }

/* Determine if directory "colr_file" exists */
  if ( stat(colr_file,&stbuf) == -1 )
   {
    if (error2 == 0)
     {
      path = colr_file;
      return_value = mkdir(path,mode);
      if (return_value != 0)
       {
   fprintf(stderr,"Directory: \"%s\" does not exist and unable to create it.\n",
           colr_file);
        error2 = 1;
        error  = 1;
       }
      else
       {
        fprintf(stderr,"Created directory \"colr\" in directory \"%s\"\n",
                location);
       }
     }
   }
  else
   {
    if ( (stbuf.st_mode & S_IFMT) != S_IFDIR)
     {
      fprintf(stderr,"\"%s\" is NOT a directory.\n",colr_file);
      error = 1;
     }
   }

/* Determine if directory "cats_file" exists */
  if ( stat(cats_file,&stbuf) == -1 )
   {
    if (error2 == 0)
     {
      path = cats_file;
      return_value = mkdir(path,mode);
      if (return_value != 0)
       {
   fprintf(stderr,"Directory: \"%s\" does not exist and unable to create it.\n",
           cats_file);
        error2 = 1;
        error  = 1;
       }
      else
       {
        fprintf(stderr,"Created directory \"cats\" in directory \"%s\"\n",
                location);
       }
     }
   }
  else
   {
    if ( (stbuf.st_mode & S_IFMT) != S_IFDIR)
     {
      fprintf(stderr,"\"%s\" is NOT a directory.\n",cats_file);
      error = 1;
     }
   }
  return(error);
 }


int
get_grass_loc(location)
  char location[];
 {
  char *getenv();
  char Home[256];
  char *home;
  char dummy[15];
  char grassrc_file[BUF256];
  short int len;
  struct stat stbuf;
  int return_value;
  char input[BUF128];
  char gisdbase[BUF128];
  char location_name[BUF128];
  char mapset[BUF128];
  int count;
  int mkdir(path,mode);
  char *path;
  static int mode = { 0777 } ;
  char temp[BUF256];
  
  home = Home;
  home = getenv("HOME");
  sprintf(grassrc_file,"%s/.grassrc",home);
  if ( stat(grassrc_file,&stbuf) == -1 )
   {
    return_value = prompt4var(gisdbase,location_name,mapset);
    if (return_value == 1)
     {
      return(return_value);
     }
   }
  else
   {
    if ( (fptr = fopen(grassrc_file,"r")) == NULL)
     {
      return_value = prompt4var(gisdbase,location_name,mapset);
      if (return_value == 1)
       {
        return(return_value);
       }
     }
    else
     {
      fscanf(fptr,"%s %s",dummy,gisdbase);
      fscanf(fptr,"%s %s",dummy,location_name);
      fscanf(fptr,"%s %s",dummy,mapset);
      fclose(fptr);
     }
   }
  count = 0;
  do
   {
    fprintf(stderr,"\nDATABASE:\t%s\n",gisdbase);
    fprintf(stderr,"LOCATION:\t%s\n",location_name);
    fprintf(stderr,"MAPSET:  \t%s\n",mapset);
    fprintf(stderr,"Are these values correct for where you wish to place\n"); 
    fprintf(stderr,"the files created from the screen?  Enter (y or n):\n");
    gets(input);
    count += 1;
    if ( *(input+0) != 'y' && *(input+0) != 'Y' )
     {
      if (count == 3)
       {
        fprintf(stderr,"You have failed to select values for DATABASE, LOCATION, and MAPSET\n"); 
        return(1);
       }
      else
       {
        return_value = prompt4var(gisdbase,location_name,mapset);
        if (return_value == 1)
         {
          return(return_value);
         }
       }
     }
    else
      count = 4;
   }
  while (count < 4);
  if ( *(input+0) != 'y' && *(input+0) != 'Y' )
   {
    return(1);
   }
  if ( strcmp(gisdbase,"/") == 0 )
   {
    sprintf(location,"%s%s/%s",gisdbase,location_name,mapset);
    sprintf(temp,"%s%s",gisdbase,location_name);
   }
  else
   {
    sprintf(location,"%s/%s/%s",gisdbase,location_name,mapset);
    sprintf(temp,"%s/%s",gisdbase,location_name);
   }
  if ( stat(location,&stbuf) == -1 )
   {
    if ((stat(gisdbase,&stbuf)!= -1) && (stat(temp,&stbuf)!= -1))
     {
      path = location;
      return_value = mkdir(path,mode);
      if (return_value != 0)
       {
        fprintf(stderr,"Unable to create directory \"%s\"\n",
                mapset);
        return(1);
       }
      else
       {
        if ( stat(location,&stbuf) == -1 )
         {
          fprintf(stderr,"Directory: \"%s\" does NOT exist.\n",location);
          return(1);
         }
        else
         {
          fprintf(stderr,"Directory: \"%s\" created.\n",mapset);
         }
       }
     }
    else
     {
      fprintf(stderr,"Directory: \"%s\" does NOT exist.\n",location);
      return(1);
     }
   }
  if ( (stbuf.st_mode & S_IFMT) != S_IFDIR)
   {
    fprintf(stderr,"\"%s\" is NOT a directory.\n",location);
    return(1);
   }
  return(0);
 }


int
prompt4var(gisdbase,location_name,mapset)
  char gisdbase[];
  char location_name[];
  char mapset[];
 {
  char *getenv();
  char input[BUF128];
  short int count;
  short int len;
  char *store();
  char *whoami();
  char Name[9];
  char *name;

  strcpy(gisdbase,"");
  strcpy(location_name,"");
  strcpy(mapset,"");
  name = Name;
  name = whoami();
  if ( strcmp(name,"?")==0 )
   {
    return(1);
   }
  count = 0;
  while (count < 1)
   {
    fprintf(stderr,"Enter DATABASE:  [Press Return for default:  \"%s\"]\t",DEFAULT_GISDB);
    gets(input);
    count += 1;
    len = strlen(input);
    if ( len > 0 )
     {
      sscanf(input,"%s",gisdbase);
      if (*(gisdbase+0)=='/')
        count = 3;
      else
       {
        fprintf(stderr,"First character must be a \"/\".\n");
        strcpy(gisdbase,"");
       }
     }
   }
  if ( len==0 || *(gisdbase+0) != '/' )
   {
    fprintf(stderr,"Default name: \"%s\" selected for you...\n",DEFAULT_GISDB);
    strcpy(gisdbase,DEFAULT_GISDB);
   }
  count = 0;
  while (count < 1)
   {
    fprintf(stderr,"Enter LOCATION:  [Press Return for default:  \"%s\"]\t",DEFAULT_LOC);
    gets(input);
    count += 1;
    len = strlen(input);
    if ( len > 0 )
     {
      sscanf(input,"%s",location_name);
      if (*(location_name+0) != '/')
        count = 3;
      else
       {
        fprintf(stderr,"First character must NOT be a \"/\".\n");
        strcpy(location_name,"");
       }
     }
   }
  if (len == 0 || (*(location_name+0) == '/') )
   {
    fprintf(stderr,"Default name: \"%s\" selected for you...\n",DEFAULT_LOC);
    strcpy(location_name,DEFAULT_LOC);
   }
  count = 0;
  while (count < 1)
   {
    fprintf(stderr,"Enter MAPSET:  [Press Return for default:  \"%s\"]\t",name);
    gets(input);
    count += 1;
    len = strlen(input);
    if ( len > 0 )
     {
      sscanf(input,"%s",mapset);
      if (*(mapset+0) != '/')
        count = 3;
      else
       {
        fprintf(stderr,"First character must NOT be a \"/\".\n");
        strcpy(mapset,"");
       }
     }
   }
  if (len == 0 || (*(mapset+0) == '/') )
   {
    fprintf(stderr,"Default name: \"%s\" selected for you...\n",name);
    strcpy(mapset,name);
   }
  return(0);
 }


int mk_tmp(location)
  char location[];
 {
  int error; 
  struct stat stbuf;
  int mkdir(path,mode);
  char *path;
  static int mode = { 0777 } ;
  int return_value;

  error = 0;
/* Determine if ".tmp" directory exists in directory "location" */
  sprintf(cell2_file,"%s/.tmp",location);
  if ( stat(cell2_file,&stbuf) == -1 )
   {
    path = cell2_file;
    return_value = mkdir(path,mode);
    if (return_value != 0)
     {
       fprintf(stderr,"Unable to write in directory \"%s\"\n",
               location);
      error  = 1;
     }
    else
     {
      fprintf(stderr,"Created directory \".tmp\" in directory \"%s\"\n",
              location);
     }
   }
  else
   {
    if ( (stbuf.st_mode & S_IFMT) != S_IFDIR)
     {
      fprintf(stderr,"\".tmp\" is NOT a directory in \"%s\".\n",location);
      fprintf(stderr,"Please remove \".tmp\" or move it to another\nlocation prior to executing \"%s\".\n",prog_name);
      error = 1;
     }
   }
  return(error); 
 }


char *whoami()
 {
  static char *name= 0;
  char *store();

  if (!name)
   {
    struct passwd *getpwuid();
    struct passwd *p;
    if (p = getpwuid (getuid()))
      name = store (p->pw_name);
   }
  if (!name)
   {
    name = store ("?");
   }
  return(name);
 }


char *store(s)
char *s;
 {
  char *buf;
  char *malloc(size);
  unsigned size;

  buf = malloc (strlen(s) + 1);
  strcpy (buf, s);
  return(buf);
 }

int
quit(n)
 int n;
 {
  fprintf(stderr,"%s: quit\n",prog_name);
  return(n);
 }
int
interrupt(n)
 int n;
 {
  fprintf(stderr,"%s: interrupt\n",prog_name);
  return(n);
 }
int
hangup(n)
 int n;
 {
  fprintf(stderr,"%s: hangup\n",prog_name);
  return(n);
 }
int
termination(n)
 int n;
 {
  fprintf(stderr,"%s: termination\n",prog_name);
  return(n);
 }
