
char Id[]="@(#) 1.1 /ncc1/grass/grass3.1a/src/grelas/s.grelas.c 5/26/90 15:53:14";
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
#include "gis.h"
#include <unistd.h>
#include <termio.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <sys/signal.h>

#define version "GRELAS version 1.14(BETA) 11-10-87"
#define GDIR "/usr2/data"
#define EDIR "/usr/elas/data"
#define elist "ls /usr/elas/data"
#define glist "ls /usr2/data"
#define LIST "list"
#define GO_ELAS "/usr/scripts/elas"
#define menu_gis "/usr/local/bin/gis"
#define cmdline_gis "/usr/local/bin/GIS"
#define import_pgm "/usr/grass3.1a/bin/Mimportcell ETRANS "
#define GDEFAULT_FILE "TRANS_ELAS"
#define target_file "ETRANS"
#define DEFAULT_MAPSET "PERMANENT"

/*********************** DEFINE GLOBAL VARIABLES *********************/

char grass_layer[40], elas_location[40],grass_location[40],
dataset_to_trans[40],change_elas_wrkdir[80],
elas_fullpath[100],grass_fullpath[100],
location_name[41],*mapset,
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
{
	char s[2];
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



/*********************** GR_READHD   FUNCTION *********************/
gr_readhd(object_data)
char *object_data;
{
	float test;
	char *answer;
	i=recno=c=row=col=channel=count=chosen_channel=last_line=irecln=0;
	mark='\056';
	printf("Object_data is: %s\n", object_data);

	fun=5; /*Open file*/
	irn=0;
	lu=1;
	nbtm=1024;
	l=0;

	/* assign input file name and check open */
	sprintf(tmp,"%s",object_data);
	filename=tmp;
	eltran_(&lu,&fun,&irn,&nbtm,filename,&l);
	if(l<0) {
		printf("File open error code=%d\n\n",l);
		exit(2);
	}
	printf("\nFile %s opened: \n",filename);

	fun=1; /* Read header in floating point to get XSPOT,YSPOT */
	irn=0;
	lu=1;
	nbtm=1024;
	l=0;
	eltran_(&lu,&fun,&irn,&nbtm,floatarray,&l);
	if(l<0) {
		printf("Read error on header,  code=%d\n\n",l);
		exit(3);
	}
	yspot=floatarray[12];
	xspot=floatarray[13];

	/*validate xspot/yspot */
	if(yspot != xspot)
	{
		printf("Error...xspot not equal to yspot. Program terminated\n");
		waitspace();
		exit(1);
	}

	fun=10; /* Rewind data file for read of integers */
	irn=0;
	lu=1;
	nbtm=1024;
	l=0;
	eltran_(&lu,&fun,&irn,&nbtm,filename,&l);

	fun=1; /*Read header in integer for other numeric data*/
	irn=0;
	lu=1;
	nbtm=1024;
	l=0;
	eltran_(&lu,&fun,&irn,&nbtm,ia,&l);
	if(l<0) {
		printf("Read error on header,  code=%d\n\n",l);
		exit(3);
	}
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
	{
		y_offset=9999999.0;
		printf("Y_offset in ELAS header was equal to 0. Reset to 9999999.0\n");
	}
	if(x_offset<=0)
	{
		x_offset=100000.0;
		printf("X_offset in ELAS header was equal to 0. Reset to 100000.0\n");
	}
	if(yspot<=0)
	{
		printf("Spot values (ie. cell dimensions), are 0 in ELAS header\n");
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
	do {
		i=i/2;
		test=(bytes_per_record % i);
	} while (test!=0);
	irecln=i;
} /* END GR_READHD */

/*********************** MAKE_IMPORT FUNCTION *********************/
make_import(object_data, outopt_answer)
char *object_data, *outopt_answer;
{
	char command[256];
	FILE *outfile;
	char *outfilename;
	long first_data_rcd,recs_in_scan;
	int col_printed,num_to_print,col_count,elements_to_process;
	int start_line,end_line,start_element,end_element,rows_to_process;

	/* Initializing numeric values to 0 */

	i=recno=c=row=col=channel=count=chosen_channel=col_count=elements_to_process=0;
	rows_to_process=col_printed=num_to_print=start_line=end_line=0;
	first_data_rcd=recs_in_scan=start_element=end_element=0;
	mark='\056';


	fun=5; /*Open file*/
	irn=0;
	lu=1;
	nbtm=1024;
	l=0;

	/* assign input file name and check open */

	sprintf(tmp,"%s",object_data);
	filename=tmp;
	eltran_(&lu,&fun,&irn,&nbtm,filename,&l);
	if(l<0) {
		printf("File open error code=%d\n\n",l);
		exit(2);
	}

	/* Get User parameters to define portion of ELAS file to
   transfer. User enters channel #, start_line, end_line,
   start_element, end_element */

	while ((chosen_channel>number_channels) || (chosen_channel<=0))
	{
		printf("\n\nWhich channel do you want output (1 - %d) ? ",number_channels);
		scanf("%d",&chosen_channel);
	}/* end chosen_channel */
	while((start_line<initial_line) || (start_line>=last_line))
	{
		printf("\nInitial line in file is: %4d",initial_line);
		printf("  Enter starting line number to transfer: ");
		scanf("%d",&start_line);
	} /* end while start_line */
	while((end_line<=start_line) || (end_line>last_line))
	{
		printf("\nLast line in file is:    %4d",last_line);
		printf("  Enter last line to transfer: ");
		scanf("%d",&end_line);
	} /* end while end_line */
	rows_to_process=(end_line-start_line)+1; /* designates # scan lines to trans */

	while((start_element<initial_element) || (start_element>=last_element))
	{
		printf("\nFirst element in scan line is: %4d",initial_element);
		printf("  Enter first element to transfer: ");
		scanf("%d",&start_element);
	} /* end while start_line */
	while((end_element<=start_element) || (end_element>last_element))
	{
		printf("\nLast element in scan line is:    %4d",last_element);
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

	outfilename = G_tempfile();
	outfile=fopen(outfilename,"w");
	if (outfile==NULL) 
		G_fatal_error("Unable to open temp file");
	fprintf(stderr,"\n\nReading ELAS data file:\n ");

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
	{
		col_count=initial_element-1;
		fun=3;
		lu=1;
		l=0;
		gotoxy(1,10);
		printf("Data records processed: %d\n",count);
		count++;
		for(i=0;i<recs_in_scan;i++)
		{
			eltran_(&lu,&fun,&recno,&irecln,bytearray,&l);
			recno++;
			if(l<0) {
				printf("Read error on row %d, record %d,  code=%d\n\n",
				    row,recno,l);
				exit(4);
			}
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
	fprintf(stderr, "Running r.in.ascii in=%s out=%s\n", outfilename, outopt_answer);
	sprintf(command, "%s/bin/r.in.ascii input=%s output=%s ", G_gisbase(), outfilename, outopt_answer);
	G_system(command);
	unlink(outfilename);
} /*End make_import */

/********************** DUMP ************************/
dump(b,rg)
{
	int R,G,B;

	B=(b & 017); /*bitwise AND sets b to octal 17 or 15 decimal */
	G=(rg & 017); /* bitwise AND sets rg to ocatal 17 or decimal 15 */
	R=((rg >> 4) & 017); /* shifts rg right 4 bits and sets rg to octal 17 */
	chk_colr[count]=R/15.0;
	count++; /* RED val in percent */
	chk_colr[count]=G/15.0;
	count++; /* GREEN val in percent */
	chk_colr[count]=B/15.0;
	count++; /* BLUE val in percent */
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

mk_gcoltab(in,out,colors)
	char *in, *out;
	struct Colors *colors;

{
	FILE *gcolorout;
	unsigned char buf[248*4], *b;
	int ECOLR,i,f1; /* f1=input file */
	int x,y,cats,done,cval;
	char gcolor_loci[140],*name;

	count=cats=done=cval=0;
	ECOLR=255;

	f1=open(in,0); /* Open input file which is ELAS header */

	/************** Reading ELAS header, processing begins at byte 480 *****/
	read (f1,buf,sizeof(buf));
	b=buf + 120*4;
	for(i=0;i<=ECOLR;i++)
	{
		x = *b++;
		y = *b++;
		/*printf("i= %d, x=%d, y=%d\n",i,x,y);*/
		dump (x,y);
	} /* end for i < ECOLR */
	/********************* Calculating # of categories in coltab **************/
	for(cval=0;cval<768;cval=cval+3)
		if(chk_colr[cval]!=0 && chk_colr[cval+1]!=0 && chk_colr[cval+2]!=0)
			cats=(cval+1)/3;

	/*********** Writing new color table in GRASS format (ie. RGB) ************/
	for(i=0;i<cval;i=i+3)
	{
		/*
		/*printf("%4.3f %4.3f %4.3f\n",
		    chk_colr[i],chk_colr[i+1],chk_colr[i+2]);*/
		G_set_color((CELL)(i/3), (int)(chk_colr[i]*255.),
			(int)(chk_colr[i+1]*255.), (int)(chk_colr[i+2]*255.), colors);
	} /* end for cats */
} /* End mk_gcoltab */


grass_to_elas ()

{

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


/*********************** EXECUTION MAIN BEGINS *********************/
main(argc,argv)
int argc;
char **argv;
{
	long *seconds,time;
	char *today,*ctime();
	char ans[2];
	int j,i,count,line,file_check,line_no;

	struct Colors colors;
	struct Cell_stats cellstat;
	struct Option *inopt;
	struct Option *outopt;
	struct Flag *colorflag;

	G_gisinit();
	inopt = G_define_option();
	inopt->key 		= "input";
	inopt->description	= "Full path name of ELAS file";
	inopt->type		= TYPE_STRING;
	inopt->required		= YES;

	outopt = G_define_option();
	outopt->key		= "output";
	outopt->description	= "Name of new RASTER file";
	outopt->type		= TYPE_STRING;
	outopt->required	= YES;
	outopt->gisprompt	= "new,cell,raster";

	colorflag = G_define_flag();
	colorflag->key		= 'c';
	colorflag->description	= "Extract ELAS color table";

	if(G_parser(argc,argv))
		exit(-1);
	j=i=count=line=file_check=line_no=0;

	if((access(inopt->answer, R_OK) != 0))
		G_fatal_error("Can't open ELAS file");

	gr_readhd(inopt->answer);
	make_import(inopt->answer, outopt->answer);
	G_init_colors(&colors);
	if(colorflag->answer)
		mk_gcoltab(inopt->answer, outopt->answer, &colors);
	else
		G_make_grey_scale_colors(&colors, 0, 255);
	G_write_colors(outopt->answer, G_mapset(), &colors);

} /* end main */



