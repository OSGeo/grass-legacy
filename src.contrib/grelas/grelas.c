/********************************************************
 *                                                      *
 *   GRELAS Version 1.14 Beta Test Relase     11-11-87  *
 *                                                      *
 *   James A. Farley, W. Fredrick Limp, Bruce Powell    *
 *   and Ian Johnson                                    *
 *                                                      *
 *   This software has been developed by the Arkansas   *
 *   Archeological Survey with support and assistance   *
 *   ITD/SRSC, CERL and NPS-Denver. Any bugs or         *
 *   suggested improvements should be brought to the    *
 *   attention of the GRASS Support Center at ITD/SRSC, *
 *   Suite 308, Building 1100, NSTL MS 39529            *
 *                                                      *
 ********************************************************/
#include "/usr/grass3/src/libes/gis.h"
#include <termio.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <sys/signal.h>

#define version "GRELAS version 1.14(BETA) 11-10-87"
#define GDIR "/usr/grass3/data"
#define EDIR "/usr/elas/data"
#define elist "ls /usr/elas/data"
#define glist "ls /usr/grass3/data"
#define LIST "list"
#define GO_ELAS "/usr/scripts/elas"
#define menu_gis "/usr/local/bin/gis"
#define cmdline_gis "/usr/local/bin/GIS"
#define import_pgm "/usr/grass3/bin/Mimportcell ETRANS "
#define GDEFAULT_FILE "TRANS_ELAS"
#define target_file "ETRANS"
#define DEFAULT_MAPSET "PERMANENT"

/*********************** DEFINE GLOBAL VARIABLES *********************/

char grass_layer[40], elas_location[40],grass_location[40],
     dataset_to_trans[40],change_elas_wrkdir[80],
     elas_fullpath[100],grass_fullpath[100],
     location_name[41],mapset[41],
     tmp[]="                                                           ",
     import_cmd[100],elas_file[40];
long lu,fun,irn,nbtm,l,ia[256],hd_rcd_id,ecoltab[480];
     initial_line,last_line,initial_element,last_element,number_channels,
     bytes_in_head,bytes_per_record,x_offset,y_offset,blocksize,lastline,
     start_line,end_line,start_element,end_element;
float *ys,*xs,floatarray[256],xspot,yspot,north,south,east,west,chk_colr[768];
/*
This was changed to handle 256 catagories - bytearray had to be changed
to an unsigned character.
char *filename,bytearray[1024],ch,ans[20];
*/
char *filename,ch,ans[20];
unsigned char bytearray[1024];
int i,recno,c,row,col,channel,
    count,chosen_channel,mark,
    irecln,rows_to_process,
    nrows,ncols,fdout,fdin;
int write_size;

struct Cell_head cellhd;
struct Colors colr;
CELL *cell,*pc;

struct elas_head
{
   long          window[8];
   char          Nor[4];
   long          Y_offset;
   char          Eas[4];
   long          X_offset;
   float         XY_spot[2];
   float         Trans_matrix[4];
   long          Not_used[5];
   char          Comments[388];
   unsigned char Color_tab[512];
   long          Not_used2[8];
};

struct elas_head elas_hd;


waitspace()
   { char s[2];
     printf("\n\nPress <return> to continue: ");
     gets(s);
   }

/*********************** SYSERR FUNCTION *********************/
int syserr(msg)
char *msg;
{
     extern int errno,sys_nerr;
     extern char *sys_errlist[];

     fprintf(stdout,"ERROR: %s (%d",msg,errno);
     if(errno >0 && errno <sys_nerr)
        fprintf(stdout, "; %s)\n",sys_errlist[errno]);
     else
        fprintf(stdout,")\n");
     waitspace();
} /* end syserr */


/*********************** GET_ELOCI FUNCTION *********************/
get_eloci()
{
  int fchk,test;
  char s[130], *object;
   do
    {
      test=0;
      clearend(4);
      printf("\n\nAvailable Elas locations in %s are:\n\n",EDIR);
      system(elist);
      printf("\nEnter ELAS Location: ");
      gets(elas_location);
      sprintf(s,"%s",elas_location);
      if(strlen(s)>0) {test=1;
                       sprintf(s,"%s/%s",EDIR,elas_location);object=s;}
     }while ((test<=0) || (fchk=(open(object,0))== -1));;
    clearend(4);
    printf("Elas location is: %s",elas_location);
} /* end get_eloci */

/*********************** GET_DATASET FUNCTION *********************/
ask_elas_file(dirname)
char dirname[30];
{
 char edlist[80],s[130],*object;
 int fchk,test;

 sprintf(edlist,"ls %s/%s",EDIR,dirname);
    do
     {
       test=0;
       clearend(5);
       printf("\n\nElas data file in %s/%s are:\n\n",
              EDIR,elas_location);
       system(edlist);
       printf("\nEnter new ELAS file name: ");
       gets(elas_file);
       sprintf(s,"%s",elas_file);
       if(strlen(s)>0) 
       {
         test=1;
         sprintf(elas_fullpath,"%s/%s/%s",
                        EDIR,elas_location,elas_file);
       }
     } while((test<=0) || (fchk=(open(elas_fullpath,0))>= 0));
   clearend(5);
   printf("ELAS file to create is: %s\n",elas_fullpath);
   printf("after ask_elas_file\n");
} /* end ask_elas_file */

get_dataset(dirname)
char dirname[30];
{
 char edlist[80],s[130],*object;
 int fchk,test;
 sprintf(edlist,"ls %s/%s",EDIR,dirname);
    do
     {
       test=0;
       clearend(5);
       printf("\n\nAvailable Elas data sets in %s/%s are:\n\n",
              EDIR,elas_location);
       system(edlist);
       printf("\nEnter name of dataset to transfer: ");
       gets(dataset_to_trans);
       sprintf(s,"%s",dataset_to_trans);
       if(strlen(s)>0) {test=1;sprintf(elas_fullpath,"%s/%s/%s",
                        EDIR,elas_location,dataset_to_trans);}
     } while((test<=0) || (fchk=(open(elas_fullpath,0))== -1));
   clearend(5);
   printf("ELAS dataset to transfer is: %s\n",elas_fullpath);
} /* end get_dataset */

/*********************** GET_GLOCI FUNCTION *********************/
char *get_gloci()
 { 
    char *LOCATION_NAME,*MAPSET,s[130],*object;
    int fchk;
  do
     {
       clearend(6);
       printf("\nAvailable GRASS locations are:\n");
       system(glist);
       printf("\n\nCurrent default LOCATION is: %s\n",G_location());
       printf("Enter name of Grass target LOCATION or <return> for default: ");
    gets(location_name);
    sprintf(s,"%s/%s",GDIR,location_name);
    object=s;
    } while (fchk=(open(object,0)== -1));
if(location_name[0]==NULL)sprintf(location_name,"%s",G_location());
else G__setenv("LOCATION_NAME",location_name);
G__write_env();
sprintf(grass_location,"%s",s);
clearend(6);
printf("GRASS LOCATION is: < %s >\n",location_name);
} /* end get_gloci */

/************************ GET_GMAPSET ******************************/
char get_mapset(dirname)
char dirname[40];
{
  char *MAPSET,s[130],*object;
  int fchk;
  do
   {
/*
       G__setenv("MAPSET",DEFAULT_MAPSET);
*/
       clearend(7);
       printf("\nAvailable GRASS mapsets are:\n");
       sprintf(s,"ls %s/%s",GDIR,location_name);
       system(s);
       printf("\n\nCurrent default MAPSET is: %s\n",G_mapset());
       printf("\nEnter name of Grass target MAPSET or <return> for default: ");
       gets(mapset);
       sprintf(s,"%s/%s/%s",GDIR,location_name,mapset);
       object=s;
    } while (fchk=(open(object,0)== -1));
if(mapset[0]==NULL) sprintf(mapset,"%s",G_mapset());
else G__setenv("MAPSET",mapset);
G__write_env();
clearend(7);
printf("GRASS MAPSET is: < %s >\n",mapset);
} /* end get_gmapset */

char get_gmapset(dirname)
char dirname[40];
{
  char *MAPSET,s[130],*object;
  int fchk;
  do
   {
/*
       G__setenv("MAPSET",DEFAULT_MAPSET);
*/
       clearend(7);
       printf("\nAvailable GRASS mapsets are:\n");
       sprintf(s,"ls %s/%s",GDIR,location_name);
       system(s);
       printf("\n\nCurrent default MAPSET is: %s\n",G_mapset());
       printf("\nEnter name of Grass target MAPSET or <return> for default: ");
       gets(mapset);
       sprintf(s,"%s/%s/%s",GDIR,location_name,mapset);
       object=s;
    } while (fchk=(open(object,0)== -1));
if(mapset[0]==NULL) sprintf(mapset,"%s",G_mapset());
else G__setenv("MAPSET",mapset);
G__write_env();
clearend(7);
printf("GRASS MAPSET is: < %s >\n",mapset);
printf("\nEnter the name of the new map layer to be created: ");
gets(grass_layer);
clearend(1);
sprintf(grass_fullpath,"%s/%s/%s/%s",GDIR,location_name,mapset,grass_layer);
printf("\nMap layer will be created in <%s> <%s> as %s\n",
       location_name,mapset,grass_layer);
} /* end get_gmapset */

/*********************** GR_READHD   FUNCTION *********************/
gr_readhd(object_data)
char object_data[70];
{
float test;
char *answer;
i=recno=c=row=col=channel=count=chosen_channel=last_line=irecln=0;
mark='\056';
printf("Object_data is: %s\n", object_data);

fun=5; /*Open file*/
irn=0;lu=1;nbtm=1024;l=0;

/* assign input file name and check open */
sprintf(tmp,"%s",object_data);
filename=tmp;
eltran_(&lu,&fun,&irn,&nbtm,filename,&l);
if(l<0) {printf("File open error code=%d\n\n",l);exit(2);} 
printf("\nFile %s opened: \n",filename);

fun=1; /* Read header in floating point to get XSPOT,YSPOT */
irn=0;lu=1;nbtm=1024;l=0;
eltran_(&lu,&fun,&irn,&nbtm,floatarray,&l);
if(l<0) {printf("Read error on header,  code=%d\n\n",l);exit(3);}
yspot=floatarray[12];
xspot=floatarray[13];

/*validate xspot/yspot */
if(yspot != xspot) 
   {printf("Error...xspot not equal to yspot. Program terminated\n");
    waitspace(); exit(1);}

fun=10; /* Rewind data file for read of integers */
irn=0;lu=1;nbtm=1024;l=0;
eltran_(&lu,&fun,&irn,&nbtm,filename,&l);

fun=1; /*Read header in integer for other numeric data*/
irn=0;lu=1;nbtm=1024;l=0;
eltran_(&lu,&fun,&irn,&nbtm,ia,&l);
if(l<0) {printf("Read error on header,  code=%d\n\n",l);exit(3);}
bytes_in_head=ia[0];
bytes_per_record=ia[1];
initial_line=ia[2];
last_line=ia[3];
initial_element=ia[4];
last_element=ia[5];
number_channels=ia[6];
y_offset=ia[9];
x_offset=ia[11];

/* Check ELAS header file for valid x/y offsets
   If they are equal to 0 then reset to 9999999.0 (yoff), and
   100000.0 (xoff), to insure positive dummy UTM coordinates.
   If this occurs then user must supply cell dimension values
*/
if(y_offset<=0)
  { y_offset=9999999.0; 
    printf("Y_offset in ELAS header was equal to 0. Reset to 9999999.0\n");}
if(x_offset<=0)
  { x_offset=100000.0; 
    printf("X_offset in ELAS header was equal to 0. Reset to 100000.0\n");}
if(yspot<=0)
  { printf("Spot values (ie. cell dimensions), are 0 in ELAS header\n");
    while (yspot <= 0) 
     {
       printf("\nEnter cell dimension in meters: ");
       scanf("%f",&yspot);
     } /*end while yspot */
   xspot=yspot; 
   } /* end if yspot <= 0 */
printf("\n\n");
printf("Information from ELAS header for data file %s\n\n",object_data);
printf("Bytes in header: %5d       Bytes per Record: %5d\n",ia[0],ia[1]);
printf("Initial Line:    %5d       Last Line:        %5d\n",ia[2],ia[3]);
printf("Initial Element: %5d       Last Element:     %5d\n",ia[4],ia[5]);
printf("X spot size:     %.2f       Y spot size:      %.2f\n",xspot,yspot);
printf("Xoffset:         %5d       Yoffset:          %5d\n",x_offset,y_offset);
printf("Channels:        %5d\n",ia[6]);

/* Establish record length offset below ELAS file header */

i=2048; /* 2048 used to accomadate initial division in loop
           (ie. assumes an ELAS header RECL of 1024 */
  do { i=i/2;
       test=(bytes_per_record % i);
     } while (test!=0);
irecln=i;
} /* END GR_READHD */

/*********************** MAKE_IMPORT FUNCTION *********************/
make_import(object_data)
char object_data[70];
{
FILE *outfile;
char *outfilename[100];
long first_data_rcd,recs_in_scan;
int col_printed,num_to_print,col_count,elements_to_process;
int start_line,end_line,start_element,end_element,rows_to_process;

/* Initializing numeric values to 0 */

i=recno=c=row=col=channel=count=chosen_channel=col_count=elements_to_process=0;
rows_to_process=col_printed=num_to_print=start_line=end_line=0;
first_data_rcd=recs_in_scan=start_element=end_element=0;
mark='\056';


fun=5; /*Open file*/
irn=0;lu=1;nbtm=1024;l=0;

/* assign input file name and check open */

sprintf(tmp,"%s",object_data);
filename=tmp;
eltran_(&lu,&fun,&irn,&nbtm,filename,&l);
if(l<0) {printf("File open error code=%d\n\n",l);exit(2);} 

/* Get User parameters to define portion of ELAS file to
   transfer. User enters channel #, start_line, end_line,
   start_element, end_element */

while ((chosen_channel>number_channels) || (chosen_channel<=0)) 
   {
     printf("\n\nWhich channel do you want output (1 - %d) ? ",number_channels);
     scanf("%d",&chosen_channel);
   }/* end chosen_channel */
while((start_line<initial_line) || (start_line>=last_line))
   { printf("\nInitial line in file is: %4d",initial_line);
     printf("  Enter starting line number to transfer: ");
     scanf("%d",&start_line);
   } /* end while start_line */
while((end_line<=start_line) || (end_line>last_line))
   { printf("\nLast line in file is:    %4d",last_line);
     printf("  Enter last line to transfer: ");
     scanf("%d",&end_line);
   } /* end while end_line */
rows_to_process=(end_line-start_line)+1; /* designates # scan lines to trans */

while((start_element<initial_element) || (start_element>=last_element))
   { printf("\nFirst element in scan line is: %4d",initial_element);
     printf("  Enter first element to transfer: ");
     scanf("%d",&start_element);
   } /* end while start_line */
while((end_element<=start_element) || (end_element>last_element))
   { printf("\nLast element in scan line is:    %4d",last_element);
     printf("  Enter last element to transfer: ");
     scanf("%d",&end_element);
   } /* end while end_line */
elements_to_process=(end_element-start_element)+1; /* Assigns # cols to trans
                                                       for each scan line */

clearend(1);
printf("\nMatrix coordinates for data transfer are:\n\n");
printf("  Elements: %4d - %4d     Scan Lines: %4d - %4d\n",
        start_element,end_element,start_line,end_line);
printf("Total elements:    %4d     Total lines:       %4d\n",
        elements_to_process,rows_to_process);

sprintf(outfilename,"%s",target_file);
outfile=fopen(outfilename,"w");
if (outfile==NULL) {printf("\n\nUnable to open output file\n\n");exit(4);}
printf("\n\nReading ELAS data file:\n ");

/* Setting cardinal corrdinate values for print as file header
   for use by Mimportcell. These values required by GRASS to 
   construct a valid cell file / cellhd file.
*/
/****************************************************
 FIRST VERSION OF UTM CALCS...RETAIN THRU BETA TEST
 north=y_offset+((last_line-start_line)*yspot);
 south=y_offset+((last_line-end_line)*yspot);
*****************************************************/
north=y_offset-((start_line-1)*yspot);
south=y_offset-(end_line*yspot);
east=x_offset+(end_element*xspot);
west=x_offset+((start_element-1)*xspot);
fprintf(outfile,"north: %.2f\nsouth: %.2f\neast: %.2f\nwest: %.2f\n",
        north,south,east,west);
fprintf(outfile,"rows: %d\ncols: %d\n",
        rows_to_process,elements_to_process);

/******************* TEST IRECLN ******************/
first_data_rcd=(bytes_in_head/irecln); /* Equal to Header offset */
recs_in_scan=(bytes_per_record/irecln); /* Sets number of R/W needed to
                                           transfer each scan line */
recno=first_data_rcd+(((chosen_channel-1)*(recs_in_scan)) + 
 ((start_line-initial_line)*(recs_in_scan*number_channels))); 

count=0;
for (row=1;row<=rows_to_process;row++) 
     {col_count=initial_element-1;
      fun=3; 
      lu=1;l=0;
      gotoxy(1,10); printf("Data records processed: %d\n",count);count++;
      for(i=0;i<recs_in_scan;i++)
       {
         eltran_(&lu,&fun,&recno,&irecln,bytearray,&l); recno++;
         if(l<0) {printf("Read error on row %d, record %d,  code=%d\n\n",
                         row,recno,l); exit(4);} 
         if(i<(recs_in_scan-1))
            for (col=0;col<irecln;col++)
              {
               col_count++;
                if((col_count>=start_element) && (col_count<=end_element))
                  fprintf(outfile,"%d ",bytearray[col]);
              } /* end for col */
         else /* Executed only for the final record in scan line */
          {
            col_printed=i*irecln;
            num_to_print=last_element-col_printed;
            for(col=0;col<num_to_print;col++)
              {
                col_count++;
                if((col_count>=start_element) && (col_count<=end_element))
                  fprintf(outfile,"%d ",bytearray[col]);
              } /* end for col */
          } /* end else */
        } /* end if i < recs_in_scan */
        fprintf(outfile,"\n");
        recno=recno+(recs_in_scan*(number_channels-1));
    } /*end for row*/
fclose(outfile);
fclose(filename);
} /*End make_import */

/********************** DUMP ************************/
dump(b,rg)
{
  int R,G,B;

  B=(b & 017); /*bitwise AND sets b to octal 17 or 15 decimal */
  G=(rg & 017); /* bitwise AND sets rg to ocatal 17 or decimal 15 */
  R=((rg >> 4) & 017); /* shifts rg right 4 bits and sets rg to octal 17 */
  chk_colr[count]=R/15.0; count++; /* RED val in percent */
  chk_colr[count]=G/15.0; count++; /* GREEN val in percent */
  chk_colr[count]=B/15.0;count++; /* BLUE val in percent */
} /* End dump */
/********************** MK_GCOLTAB *********************

  
  pgm reads first 992 bytes of ELAS header file 
      into unsigned char array. This includes all
      of header data and color table information
      (ie. words 1-248). b is then set to point to byte 
      480 (buf + 120*4), and then the 2 byte
      integer values for x and y are read in for the
      next 512 bytes (ie. 256 interations), and dumped
      for conversion from ELAS BRG coltab values
      to the RBG values which GRASS expects. The program
      calculates the point in the color table at which
      color data ends and truncates the grass colr table
      at that point.
***************************************************/

mk_gcoltab()

{
      FILE *gcolorout;
      unsigned char buf[248*4], *b;
      int ECOLR,i,f1; /* f1=input file */
      int x,y,cats,done,cval;
      char gcolor_loci[140],*name;

      count=cats=done=cval=0;
      ECOLR=255;
      get_eloci();
      get_dataset(elas_location);
      get_gloci();
      get_gmapset();
      f1=open(elas_fullpath,0); /* Open input file which is ELAS header */
      
/************** Reading ELAS header, processing begins at byte 480 *****/
      read (f1,buf,sizeof(buf));
      b=buf + 120*4;
      for(i=0;i<=ECOLR;i++) 
      {
          x = *b++;
          y = *b++;
          dump (x,y);
      } /* end for i < ECOLR */

/********************* Calculating # of categories in coltab **************/
      for(cval=0;cval<768;cval=cval+3)
         if(chk_colr[cval]!=0 && chk_colr[cval+1]!=0 && chk_colr[cval+2]!=0)
            cats=(cval+1)/3;

/*********** Writing new color table in GRASS format (ie. RGB) ************/ 
      sprintf(gcolor_loci,"%s/%s/%s/colr/%s",
              GDIR,location_name,mapset,grass_layer);
      name=gcolor_loci; 
      gcolorout=fopen(name,"w");
      fprintf(gcolorout,"%d categories\n",cats);
      for(i=0;i<cval-3;i=i+3)
       { fprintf(gcolorout,"%4.3f %4.3f %4.3f\n",
                 chk_colr[i],chk_colr[i+1],chk_colr[i+2]); 
       } /* end for cats */
  fclose(gcolorout);
} /* End mk_gcoltab */


grass_to_elas ()

{

unsigned char r,g,rg;
int n;
int k;
int write_cnt;
int read_cnt;
int remainder;

/* Read cell header information into cellhd */

if ((G_get_cellhd(grass_layer,mapset,&cellhd)) == -1)
{
  printf("ERROR: cell header for file '%s' not found\n",
         grass_layer);
  return;
}

/* Read color table - print error message if table is not found
   but continue to transfer without table 
 */

if ((G_get_colr(grass_layer,mapset,&colr)) == -1)
{
  printf("ERROR: color table for file '%s' not found\n",
         grass_layer);
}

/* original from GRASS 2.0  - for (k=0,n=0;n<=colr.num;n++) */
for (k=0,n=0;n<=colr.max;n++)
{
   elas_hd.Color_tab[k++] = colr.blu[n] * 15.0;
   r = (colr.red[n] * 15.0);
   r = r << 4;
   g = colr.grn[n] * 15.0;
   elas_hd.Color_tab[k++] = r | g;
/*
   printf("red=%f green=%f blue=%f\n",colr.red[n],colr.grn[n],colr.blu[n]);
   printf("Color[b]=%d Color[rg]=%d /n",elas_hd.Color_tab[k-2],
         elas_hd.Color_tab[k-1]);
*/
}

/* Get number of rows and columns in the grass data file */

nrows = G_window_rows();
ncols = G_window_cols();

/* ELAS data file records have to be multiples of 512 bytes. Determine
 * if the grass data file has records that are multiples of 512. Else
 * add the to record size to make it a multiple of 512.
 */

if (ncols < 512)
  write_size = 512;
else if ( (remainder = ncols % 512) == 0)
  write_size = ncols;
else
  write_size = (512 - remainder) + ncols;

/* open the cell file for reading */

if ((fdin = G_open_cell_old(grass_layer,mapset)) < 0)
{
  printf("can't open cell file '%s'\n",grass_layer);
  return;
}

/* open elas file for writing */

if ((fdout = open (elas_fullpath,O_RDWR | O_CREAT,0666)) < 0)
{
  printf("ERROR: Open failed.\n");
  return;
}

/* write the header and color information of the elas data file */

write_elas_header();

/* allocate the cell buffer */

cell = G_allocate_cell_buf();

/* process each row of the window */

for (row=0;row < nrows;row++)
{
   if (( G_get_map_row (fdin,pc=cell,row)) < 0)
   {
     printf("ERROR: reading cell file '%s' \n", grass_layer);
     sleep(3);
     return;
   } 
   
   /* Show user status of transfer */
   gotoxy(1,10);
   printf("Data records processed: %d\n",row);

   if ((write_cnt = write(fdout,cell,write_size)) < write_size)
   {
     printf("ERROR: write failed only %d written\n",write_cnt);
     sleep(3);
     return;
   }
}

/* closed cell and elas file */

close(fdout);
G_close_cell(fdin);
}

write_elas_header()
{

int n;


elas_hd.window[0] = 1024;
elas_hd.window[1] = 512;
elas_hd.window[2] = 1;
elas_hd.window[3] = nrows;
elas_hd.window[4] = 1;
elas_hd.window[5] = ncols;
elas_hd.window[6] = 1;
elas_hd.window[7] = 4321;
sprintf(elas_hd.Nor,"NOR");
elas_hd.Y_offset = (int) cellhd.north;
sprintf(elas_hd.Eas,"EAS");
elas_hd.X_offset = (int) cellhd.west;
elas_hd.XY_spot[0] = cellhd.ns_res;
elas_hd.XY_spot[1] = cellhd.ew_res;
elas_hd.Trans_matrix[0] = 1.;
elas_hd.Trans_matrix[1] = 0.;
elas_hd.Trans_matrix[2] = 0.;
elas_hd.Trans_matrix[3] = -1.;

/*
for (n=0;n<8;n++)
   printf("%d \n",elas_hd.window[n]);

printf("%s %d %s %d %f %f\n",elas_hd.Nor,elas_hd.Y_offset,
        elas_hd.Eas,elas_hd.X_offset,elas_hd.XY_spot[0],elas_hd.XY_spot[1]);
for (n=0;n<4;n++)
   printf("%f \n",elas_hd.Trans_matrix[n]);
*/

if ((n=write(fdout,&elas_hd,sizeof(elas_hd))) < 1024)
{
  printf("ERROR: write failed only %d written\n",n);
  sleep(2);
  return;
}

}
/********************* DISCLOSURE AAS *********************/
disclosure()
{
  int i;
  clearend(1);
  gotoxy(5,5);
  for(i=0;i<66;i++) printf("="); 
  gotoxy(5,7);
  printf("    GRELAS GRASS-ELAS INTERFACE - BETA TEST RELEASE");
  gotoxy(5,9);
  for(i=0;i<66;i++) printf("="); 
  gotoxy(5,10);
  printf(" This software developed by the Arkansas Archeological Survey");
  gotoxy(5,11);
  printf(" with support and assistance from the following organizations:");
  gotoxy(25,13);
  printf("ITD/SRSC, CERL, NPS-Denver");
  gotoxy(5,15);
 printf("Please foreward any information on bugs or suggested enhancements");
  gotoxy(5,16);
printf("to Jim Farley C/O GRASS Support Center, ITD Space Remote Sensing ");
gotoxy(5,17); printf("Center. Suite 308, Building 1100, NSTL MS 39529");
  gotoxy(5,18);
  for(i=0;i<66;i++) printf("="); 
  printf("\n\n");
  waitspace();
}

/*********************** EXECUTION MAIN BEGINS *********************/
main()
{
   extern char *get_gloci();
   long *seconds,time();
   char *today,*ctime();
   char ans[2];
   int j,i,count,line,file_check,line_no;
   j=i=count=line=file_check=line_no=0;
  
   do
   {
     clearend(1);
     *seconds=time(0);
     today=ctime(seconds);
     printf("\n\n%s        %s\n\n",version,today); 
     printf("\nMake a selection from the menu below\n\n");
     printf("     1) ELAS data to GRASS map layer\n");
     printf("     2) ELAS color table to GRASS \n");
     printf("     3) GRASS to ELAS \n");
     printf("     4) Run GRASS (full menu version)\n");
     printf("     5) Run GRASS (command line version)\n");
     printf("     6) Run ELAS \n\n");
     printf("Enter x to terminate GRELAS file transfer\n\n");
     printf("Enter selection: ");
     gets(ans);
     switch (ans[0])
         {
          case '1' : screen_head("ELAS to GRASS data transfer facility\n\n");
                     get_eloci();
                     get_dataset(elas_location);
                     get_gloci();
                     get_gmapset(grass_location);
                     gr_readhd(elas_fullpath);
                     make_import(elas_fullpath);
                     sprintf(import_cmd,"%s %s",import_pgm,grass_layer);
                 printf("\nLoading data into GRASS map layer...please wait.\n");
                     system(import_cmd);
                     waitspace();
                     break;
          case '2' : screen_head("ELAS to GRASS color table transfer\n\n");
                     mk_gcoltab();break;
          case '3' : screen_head("GRASS to ELAS file transfer facility\n\n");
                       G_gisinit("Transfer");
                       get_gloci();
                       get_mapset(grass_location);
                       G_ask_cell_in_mapset("",grass_layer);
                       get_eloci();
                       ask_elas_file(elas_location);
                       grass_to_elas ();
                       break;
          case '4' : printf("GRASS Full Menu Version");
                       sleep(1);
                       system(menu_gis);
                       break;
          case '5' : printf("Grass Command Line Version");
                       sleep(1);
                       system(cmdline_gis);
                       break;
          case '6' : printf("ELAS Remote Sensed Imagery");
/*                       get_eloci();
                       printf("Elas location is: %s\n",elas_location);
                       sprintf(change_elas_wrkdir,"cd %s/%s",
                               EDIR,elas_location);
                       system(change_elas_wrkdir);
*/
                       system(GO_ELAS);
                       break;
            case 'x' : break;
            case 'X' : break;
            default  : printf("\n\007 ** Unrecognizable Response ** \007\n");
                       waitspace();break;
          } /* end case */
    } while (ans[0]!='x' && ans[0]!='X');
} /* end main */



