/*
   reads USGS 3 arc second DEM prepared
   from a 15 min. quad and prepares a data
   file suitable for input into m.rot90, r.in.ll
   data files created: runlog, dem_dat
   Usage: dd if=tapedrive ibs=blocksize |\
           a.out top=value bottom=value left=value right=value output=name logfile=name
   Note: the values for parms are not in lat / lon
	 but rather express relative row / col
	 values within the standard 1201x1201 (15 min)
	 matrix which is used
*/

#include "gis.h"
#include <stdlib.h>
#include <unistd.h>

char *dem_out;
char *log_out;
int show(double, char *);
int skip_pro(int, int);
int profile(int, int, int, int, int, int);

int main (int argc, char *argv[])
{
	struct GModule *module;
	struct
	{
		struct Option *bottom, *top, *left, *right, *output, *logfile;
	} parm;
	int i,j;
	int MIN,MAX; 
	int TOP, BOTTOM, LEFT, RIGHT;
	int PROFILES,OBS,SKIPB,SKIPT;
	int data13;
	char word[1024];
	double sw_lon, sw_lat;
	double nw_lon, nw_lat;
	double se_lon, se_lat;
	double ne_lon, ne_lat;

	FILE *fp, *fopen();

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Extracts digital terrain elevation data (DTED) "
		"produced by the Defense Mapping Agency (DMA) "
		"but supplied by the USGS (in a different tape format) "
		"on 1/2-inch magnetic tape.";

	parm.top = G_define_option();
	parm.top->key = "top";
	parm.top->type = TYPE_INTEGER;
	parm.top->required = YES;
	parm.top->options = "1-1201";
	parm.top->description = "First row of data window to extract";

	parm.bottom = G_define_option();
	parm.bottom->key = "bottom";
	parm.bottom->type = TYPE_INTEGER;
	parm.bottom->required = YES;
	parm.bottom->options = "1-1201";
	parm.bottom->description = "Last row of data window to extract";

	parm.left = G_define_option();
	parm.left->key = "left";
	parm.left->type = TYPE_INTEGER;
	parm.left->required = YES;
	parm.left->options = "1-1201";
	parm.left->description = "First column of data window to extract";

	parm.right = G_define_option();
	parm.right->key = "right";
	parm.right->type = TYPE_INTEGER;
	parm.right->required = YES;
	parm.right->options = "1-1201";
	parm.right->description = "Last column of data window to extract";

	parm.output = G_define_option();
	parm.output->key = "output";
	parm.output->type = TYPE_STRING;
	parm.output->required = YES;
	parm.output->description = "Output file to hold DEM data";

	parm.logfile = G_define_option();
	parm.logfile->key = "logfile";
	parm.logfile->type = TYPE_STRING;
	parm.logfile->required = YES;
	parm.logfile->description = "File to log other related information";

	if (G_parser(argc,argv))
		exit(1);

	dem_out = parm.output->answer;
	log_out = parm.logfile->answer;
	sscanf (parm.top->answer, "%d", &TOP);
	sscanf (parm.bottom->answer, "%d", &BOTTOM);
	sscanf (parm.left->answer, "%d", &LEFT);
	sscanf (parm.right->answer, "%d", &RIGHT);

	fp=fopen(dem_out,"w");
	if (fp == NULL)
	{
		perror(dem_out);
		exit(1);
	}
	fclose(fp);

	/* header record - reads DEM header data to file */

	fp=fopen(log_out,"w"); /* open file for data write */
	for (i=0; i < 144; i++) /* skip Quadrangle name */
		getchar();


	for (i = 2; i <= 5; i++)
	{
		scanf ("%s", word);
		fprintf (fp,"%d: %s\n", i, word);
	}

	j=6;
	fprintf(fp,"\nMap Projection Parameters (15 values)\n");
	for (i = 0; i < 15; i++)
	{
		scanf ("%s", word);
		fprintf(fp,"%d.%d: %s\n",j,i,word);
	}
	j++;
	for (i = 7; i <= 9; i++)
	{
		scanf ("%s", word);
		fprintf (fp,"%d: %s\n", i, word);
		j++;
	}
	fprintf(fp,"\nGround Coordinates of the 4 corners of the DEM:\n");
	scanf ("%s", word);
	sscanf (word, "%lf", &sw_lat);
	fprintf (fp, "   %s: \n", word);
	show (sw_lat,"EW");
	j++;

	scanf ("%s", word);
	sscanf (word, "%lf", &sw_lon);
	fprintf (fp, "   %s: \n", word);
	show (sw_lon,"EW");

	scanf ("%s", word);
	sscanf (word, "%lf", &nw_lat);
	fprintf (fp, "   %s: \n", word);
	show (nw_lat,"EW");

	scanf ("%s", word);
	sscanf (word, "%lf", &nw_lon);
	fprintf (fp, "   %s: \n", word);
	show (nw_lon,"EW");

	scanf ("%s", word);
	sscanf (word, "%lf", &ne_lat);
	fprintf (fp, "   %s: \n", word);
	show (ne_lat,"EW");

	scanf ("%s", word);
	sscanf (word, "%lf", &ne_lon);
	fprintf (fp, "   %s: \n", word);
	show (ne_lon,"EW");

	scanf ("%s", word);
	sscanf (word, "%lf", &se_lat);
	fprintf (fp, "   %s: \n", word);
	show (se_lat,"EW");

	scanf ("%s", word);
	sscanf (word, "%lf", &se_lon);
	fprintf (fp, "   %s: \n", word);
	show (se_lon,"EW");

	for (i = 1; i <= 2; i++)
	{
		scanf ("%s", word);
		fprintf (fp,"%d.%d: %s\n", j, i, word);
	} /* Data Category 11 */
	j++;
	scanf ("%s",word); /* data category 12 .. val should be 0.0 */
	fprintf (fp,"%d: %s\n",j, word); 
	j++;

	scanf("%1d",&data13);
	fprintf (fp,"%d: %d\n",j, data13); 
	j++;

	scanf ("%s",word); /* data category 14 .. reads a 3 element array
								  containing DEM spatial resolution */
	fprintf (fp,"%d: %s\n",j,word); 
	j++;

	scanf ("%s",word); /* data category 15.1 1st element array R/C of 
					    profiles in DEM (1,1201) val=MIN */
	fprintf (fp,"%d: %s\n",j,word); j++;
	MIN=atoi(word);
	scanf ("%s",word); /* data category 15.2  2nd element array R/C of 
					    profiles in DEM (1,1201) val=MAX */
	fprintf (fp,"%d: %s\n",j,word);j++;
	MAX=atoi(word);

	fprintf (fp,"********** File header complete *************\n\n");

	fclose(fp);

	/* PROCESSING DATA RECORD */

	if (((TOP<MIN) || (TOP>MAX)) || ((LEFT<MIN) || (LEFT>MAX)) || ((BOTTOM<MIN) || (BOTTOM>MAX)) || ((RIGHT<MIN) || (RIGHT>MAX)))
	{
	   fprintf(stderr,"usage: coordinate range limited to 1 to 1201\n");
	   exit(1);
	}
	fprintf(stderr,"The requested window is:\n");
	fprintf(stderr,"rows: %d-%d, cols %d-%d\n",TOP,BOTTOM,LEFT,RIGHT);

	/* Locate window within 1201,1201 array of profiles */

	PROFILES=((RIGHT-LEFT)+1); /*calculate # profiles in window */
	OBS=((BOTTOM-TOP)+1); /* calculate # obs written to file */
	SKIPB=TOP-MIN; /* calculate # obs skipped below window */
	SKIPT=MAX-BOTTOM; /* calculate # obs skipped above window */

	if(((TOP==MIN) && (LEFT==MIN)) || ((LEFT==MIN) && (BOTTOM==MAX))) 
	/* box is along sw/nw axis and no profiles skipped */
	{
	profile(PROFILES,OBS,SKIPB,SKIPT,LEFT,RIGHT);
	}
    else
	{
		skip_pro(LEFT,MAX);
		profile(PROFILES,OBS,SKIPB,SKIPT,LEFT,RIGHT);
	}

	exit(0);
} /* end of main */

int 
show (double x, char *s)
{
	long y;

	y = x;
	if (y < 0) y = -y;

/*	fprintf (stderr, "%3ld.%02ld.%02ld%c\n", y/3600, (y%3600)/60, y%60, x<0 ? s[0]:s[1]); */
	return 0;
} /*end show */

int 
skip_pro (int SKIP, int MAX)
{
	int i,j,k;
	char word[1024];

	for (i=1;i<=SKIP;i++) /* skip first SKIP profiles on tape */
	{
		for (k=1;k<=9;k++)
		{
			scanf ("%s",word); /* reading DEM type B header */
		}
		for (j=1;j<=MAX;j++) /* read 1201 data values in profile */
		{
			scanf("%s",word); /* read data value and discard */
		}
	}
	return 0;
} /* end skip_pro */

int 
profile ( /* will skip select obs in profile */
    int PROFILES,
    int OBS,
    int SKIPB,
    int SKIPT,
    int LEFT,
    int RIGHT
)
{
	int profile,i,anint,j,k,l;
	short shortint;
	char word[1024];
	unsigned char c;

	FILE *fopen(), *fp, *fr;

	for (profile=1;profile<=PROFILES;profile++) /* reading window */
	{
		for (i=1;i<=9;i++)
			scanf ("%s",word); /* reading DEM type B header */
		if (SKIPB>0)
			for (j=1;j<=SKIPB;j++)
				scanf ("%s",word); /* discarding profile obs below window */
	fp =  fopen(dem_out,"a"); /* open file for data write */
	for (k=1;k<=OBS;k++)
	{
		scanf ("%d",&anint);
		shortint=anint;
		/*
		fwrite((char *)&shortint,sizeof(shortint),1,fp);
		*/
		/* convert native negative to "signed value" 
		** to be read correctly by Mimport.ll
		**  i.e. positive # w/ high bit set  -dpg Mar. 1990
		** and in big-endian order
		*/
		if (shortint < 0)
		    shortint = (-shortint) | 0x8000;
		
		c = (shortint >> 8) & 0xff;
		fwrite(&c,1,1,fp);
		c = shortint & 0xff;
		fwrite(&c,1,1,fp);
	}
	if (SKIPT>0)
		for (l=1;l<SKIPT;l++)
			scanf ("%s",word); /* discarding profile obs above window */
	fclose(fp);
	} /* end for profile */

/* Write summary information to run log file */
 fr = fopen(log_out,"a"); /* open and append to run log */
 fprintf(fr,"*******************************************\n\n");
 fprintf(fr,"Summary of tape processing:\n\n");
 fprintf(fr,"   Profiles selected: %d (cols %d - %d)\n",PROFILES,LEFT,RIGHT);
 fprintf(fr,"   Observations processed from each profile: %d\n",OBS);
 fprintf(fr,"   Observations below window rejected: %d\n",SKIPB);
 fprintf(fr,"   Observations above window rejected: %d\n",SKIPT);
 fclose(fr);
	return 0;
} /* end profile */
