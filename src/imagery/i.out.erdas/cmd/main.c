#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"
#include "local_proto.h"
#define IAUTYP "NAHO"
#define MAXBND 7
#define MAXNUMBER 9999999999.0

/*
 * $Id$
 */

int ERDFTYP;   /* ERDAS file type ( 4 or other) */


/******** Routine that does byte swapping for 2 or 4 byte word in place *******/
static void convbuf (void *buf, int ftype)
{
	int i;
	register char *buf1;
	char buf2[4];

	buf1 = (char *)buf;
	if (ftype != 4 && ftype != 2) {
		fprintf(stderr,"Error in type conversion\n");
		return;
	}
	if (ftype == 4){
		buf2[0] = *(buf1+3);
		buf2[1] = *(buf1+2);
		buf2[2] = *(buf1+1);
		buf2[3] = *(buf1);
	}
	else if (ftype == 2){
		buf2[0] = *(buf1+1);
		buf2[1] = *(buf1);
	}
	for (i=0;i<ftype;i++) *(buf1+i) = buf2[i];
}
/******************** End convbuf ************************************/


/******** Routine to do byte swapping for necessary elements on 3B2 **********/
/******** this routine necessary because ERDAS file from the DOS system ******/
static void conv3b2hd (struct erdheader *buf)
{
	convbuf(&(buf->pack),2);
	convbuf(&(buf->nbands),2);
	convbuf(&(buf->rcols),4);
	convbuf(&(buf->rrows), 4);
	convbuf(&(buf->rx), 4);
	convbuf(&(buf->ry),4);
	convbuf(&(buf->maptyp), 2);
	convbuf(&(buf->nclass),2);
	convbuf(&(buf->utyp),2);
	convbuf(&(buf->area), 4);
	convbuf(&(buf->mx), 4);
	convbuf(&(buf->my), 4);
	convbuf(&(buf->xcel), 4);
	convbuf(&(buf->ycel),4);
}
/********************* End conv3b2hd ******************************/



/*********** Routine that prints out the ERDAS file header data ***********/
static void 
printhd (struct erdheader *hd)
{
	int i;
	for (i=0;i<6;i++) fprintf(stderr,"%c",hd->hdwrd[i]);
	fprintf(stderr,"\n");
	fprintf(stderr,"pack type------ %d == ",hd->pack);
	if (hd->pack == 0) fprintf(stderr,"8 bit/pixel\n");
	else if(hd->pack == 1) fprintf(stderr,"4 bit/pixel\n");
	else if(hd->pack == 2) fprintf(stderr,"16 bit/pixel\n");
	else fprintf(stderr,"Unknown - try forcing swapping\n");
	fprintf(stderr,"number bands----------- %d\n",hd->nbands);
	fprintf(stderr,"number cols,rows------- %ld, %ld\n",hd->rcols,hd->rrows);
	fprintf(stderr,"starting coordinate --- %ld, %ld\n",hd->rx,hd->ry);
	fprintf(stderr,"map type -------------- %d\n",hd->maptyp);
	fprintf(stderr,"number classes -------- %d\n",hd->nclass);
	fprintf(stderr,"area ------------------ %f %c\n",hd->area,IAUTYP[hd->utyp]);
	fprintf(stderr,"map coordinate -------- %f, %f\n",hd->mx,hd->my);
	fprintf(stderr,"pixel size ------------ %f, %f\n\n",hd->xcel,hd->ycel);
}
/***************** End printhd *********************************************/


/*New main routine for i.out.erdas */
int main (int argc, char *argv[])
{
/*Define some variables*/
	struct erdheader erdashd;
	struct GModule *module;
	struct Option *erdasopt, *inopt ;
	struct Flag *headflag,*swapflag,*bit16flag;
	struct Cell_head reg ;
	int ActuallySwap,use16bits,showhead;
	int *inpfd;
	FILE *erdfd;
	char *mapset;
	int i,j,k;
	unsigned int nbands;
	char tempbuf[1000], *eightbitbuf;
	short *sixteenbitbuf;
	CELL *rowbuf;
	
	G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
		"Creates ERDAS files from raster files.";
					        
	headflag = G_define_flag();
	headflag->key = 'l';
	headflag->description = "List the ERDAS header only";

    bit16flag = G_define_flag();
    bit16flag->key = 'b';
    bit16flag->description = "Output 16-bit image (default 8)";

    swapflag = G_define_flag();
    swapflag->key = 's';
    swapflag->description = "Force byte swapping";
    
	inopt = G_define_option();
	inopt->key             = "input";
	inopt->type            =  TYPE_STRING;
	inopt->required        =  YES;
	inopt->multiple        =  YES;
	inopt->gisprompt       =  "old,cell,raster";
	inopt->description     = "Input raster layers.";

	erdasopt = G_define_option();
	erdasopt->key             = "output";
	erdasopt->type            =  TYPE_STRING;
	erdasopt->required        =  YES;
	erdasopt->description     = "Erdas output file name";


	/* heeeerrrrrre's the   PARSER */
	if (G_parser (argc, argv))
		exit (-1);
	

/*Get and Check command line parameters*/
	ActuallySwap = swapflag->answer ;
	use16bits = bit16flag->answer ;
	showhead = headflag->answer;
/*Check Input Filenames                  */
	for (nbands=0 ; inopt->answers[nbands] != NULL ; nbands ++ );
	if (nbands == 0) G_fatal_error("Please provide at least one band of imagery");
	
	inpfd = (int * ) G_malloc(nbands * sizeof(int)) ;
	for (i=0;i<nbands;i++)
		{
		if ((mapset = G_find_cell(inopt->answers[i], "")) == NULL )
			{
			sprintf(tempbuf,"Unable to find cell map <%s>\n",inopt->answers[i]);
			G_fatal_error(tempbuf);
			}
		if ((inpfd[i] = G_open_cell_old(inopt->answers[i],mapset)) < 0)
			{
			sprintf(tempbuf,"Error Opening %s\n",inopt->answers[i]);
			G_fatal_error(tempbuf);
			}	
		}
	

/*Get the GRASS Window*/
	G_get_set_window(&reg);
	
/*Build erdas header*/
	for (i=0;i<6;i++) erdashd.hdwrd[i]="HEAD74"[i];
	if (use16bits) erdashd.pack = 2 ;
	else erdashd.pack = 0;               /* Not bothering with 4-bit pack type. So sue me... */
	erdashd.nbands = nbands ;	
	for (i=0;i<6;i++) erdashd.fil1[i]=0;
	erdashd.rcols = (unsigned int) 	G_window_cols();
	erdashd.rrows = (unsigned int) 	G_window_rows();
	erdashd.rx=0;
	erdashd.ry=0;
	for (i=0;i<56;i++) erdashd.fill1[i]=0;
	erdashd.maptyp=0; /*Modify this to deal with projections from GRASS */
	erdashd.nclass=0;
	for (i=0;i<14;i++) erdashd.fill2[i]=0;
	erdashd.utyp = 0 ;
	erdashd.area = reg.ew_res * reg.ns_res ; 
	erdashd.mx = reg.west + (reg.ew_res / 2) ; /* Cell centres, not GRASS's Edges */
	erdashd.my = reg.north - ( reg.ns_res / 2 ) ; /* Ditto */
	erdashd.xcel = reg.ew_res ;
	erdashd.ycel = reg.ns_res ;

	printhd(&erdashd);
	
	if(showhead) exit (0);
	if (ActuallySwap) conv3b2hd(&erdashd);
/*Check Output filename*/
	if ((erdfd = fopen(erdasopt->answer,"wb")) == NULL) 
		{
		fprintf(stderr,"Error can not open ERDAS file\n");
		exit(0);
		}

/*Write out header*/
	if (fwrite(&erdashd,sizeof(erdashd),1,erdfd) != 1)
		G_fatal_error("Failed writing header\n");
		
/*Write out data in BIL order*/
	rowbuf = (CELL *) G_allocate_cell_buf();
	eightbitbuf = (char *) G_calloc(1,reg.cols);
	sixteenbitbuf = (short *) G_calloc(2,reg.cols);
	for (i=0;i<reg.rows;i++)
		{
		for (j=0;j<nbands;j++)
			{
			G_get_map_row(inpfd[j],rowbuf,i);
			for(k=0;k<reg.cols;k++)
				{
				
				if (use16bits)
					{
					sixteenbitbuf[k] = (unsigned short) rowbuf[k];
					if(ActuallySwap) convbuf(&(sixteenbitbuf[k]),2);
					
					}
				else /* !use 16 bit ==> 8 bit) */
					{
					eightbitbuf[k] = (unsigned char) rowbuf[k];
					}
				}
			if(use16bits) fwrite(sixteenbitbuf,2,reg.cols,erdfd);
			else /* !use16bits */ fwrite(eightbitbuf,1,reg.cols,erdfd);
			
			}
		}
	/* And C closes all fd's etc. Bad practice for me to rely on it, but.... */
	printf("i.out.erdas completed normally (I think...)\n");
}










