/* %W% %G% */
/* genrd.c ... function:
   reads USGS 3 arc second DEM prepared
   from a 15 min. quad and prepares a data
   file suitable for input into Mrot90, Mimport.ll
   data files created: runlog, dem_dat
   Usage: genread arg1 arg2 arg3 agr4 
	  where arg1 = row sw
		arg2 = col sw
		arg3 = row ne
		arg4 = col ne
   Note: the values for all args are not in lat / lon
	 but rather express relative row / col
	 values within the standard 1201x1201 (15 min)
	 matrix which is used
*/

#include <stdio.h>
#define dat_out	"dem_data"
#define run_out "runlog"
main(argc,argv)
int argc;
char *argv[];
{
	int i,anint,j,k;
	int RSW,CSW,RNE,CNE,MIN,MAX; 
	int PROFILES,OBS,SKIPB,SKIPT;
	short shortint;
	int data13;
	char word[1024];
	double sw_lon, sw_lat;
	double nw_lon, nw_lat;
	double se_lon, se_lat;
	double ne_lon, ne_lat;

	FILE *fp, *fopen();

	if (argc < 5)
	{
	   fprintf(stderr,"usage: genread rsw csw rne cne\n");
	   exit(1);
	}
       

    fp=fopen(dat_out,"w");
	fclose(fp);

	/* header record - reads DEM header data to file */

	fp=fopen(run_out,"w"); /* open file for data write */
	for (i=0; i < 144; i++) /* skip Quadrangle name */
		getchar();


	for (i = 2; i <= 5; i++)
	{
		scanf ("%s", word);
		fprintf (fp,"%d: %s\n", i, word);
	}

	j=6;
	fprintf(fp,"\nMap Projection Perameters (15 values)\n");
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

	RSW=atoi(argv[1]);
	CSW=atoi(argv[2]);
	RNE=atoi(argv[3]);
	CNE=atoi(argv[4]);
	if (((RSW<MIN) || (RSW>MAX)) || ((CSW<MIN) || (CSW>MAX)) || ((RNE<MIN) || (RNE>MAX)) || ((CNE<MIN) || (CNE>MAX)))
	{
	   fprintf(stderr,"usage: coordinate range limited to 1 to 1201\n");
	   exit(1);
	}
	fprintf(stderr,"The coordinates for window are, rsw,csw by rne,cne:\n");
	fprintf(stderr,"%d,%d and %d,%d\n",RSW,CSW,RNE,CNE);

	/* Locate window within 1201,1201 array of profiles */

	PROFILES=((CNE-CSW)+1); /*calculate # profiles in window */
	OBS=((RNE-RSW)+1); /* calculate # obs written to file */
	SKIPB=RSW-MIN; /* calculate # obs skipped below window */
	SKIPT=MAX-RNE; /* calculate # obs skipped above window */

	if(((RSW==MIN) && (CSW==MIN)) || ((CSW==MIN) && (RNE==MAX))) 
	/* box is along sw/nw axis and no profiles skipped */
	{
	profile(PROFILES,OBS,SKIPB,SKIPT,CSW,CNE);
	}
    else
	{
		skip_pro(CSW,MAX);
		profile(PROFILES,OBS,SKIPB,SKIPT,CSW,CNE);
	}

} /* end of main */

show (x, s)
double x;
char *s;
{
	long y;

	y = x;
	if (y < 0) y = -y;

/*	fprintf (stderr, "%3ld.%02ld.%02ld%c\n", y/3600, (y%3600)/60, y%60, x<0 ? s[0]:s[1]); */
} /*end show */

skip_pro(CSW,MAX)
int CSW,MAX;
{
	int i,j,k;
	char word[1024];

	for (i=1;i<=CSW;i++) /* skip first CSW profiles on tape */
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
} /* end skip_pro */

profile(PROFILES,OBS,SKIPB,SKIPT,CSW,CNE) /* will skip select obs in profile */

int PROFILES,OBS,SKIPB,SKIPT,CSW,CNE;
{
	int profile,i,anint,j,k,l;
	short shortint;
	char word[1024];
	unsigned char c;
	unsigned char *p;

	FILE *fopen(), *fp, *fr;

	for (profile=1;profile<=PROFILES;profile++) /* reading window */
	{
		for (i=1;i<=9;i++)
			scanf ("%s",word); /* reading DEM type B header */
		if (SKIPB>0)
			for (j=1;j<=SKIPB;j++)
				scanf ("%s",word); /* discarding profile obs below window */
	fp =  fopen(dat_out,"a"); /* open file for data write */
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
 fr = fopen(run_out,"a"); /* open and append to run log */
 fprintf(fr,"*******************************************\n\n");
 fprintf(fr,"Summary of tape processing:\n\n");
 fprintf(fr,"   Profiles selected: %d (cols %d - %d)\n",PROFILES,CSW,CNE);
 fprintf(fr,"   Observations processed from each profile: %d\n",OBS);
 fprintf(fr,"   Observations below window rejected: %d\n",SKIPB);
 fprintf(fr,"   Observations above window rejected: %d\n",SKIPT);
 fclose(fr);
} /* end profile */
