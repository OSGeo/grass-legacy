/* %W% %G% */
/* get_stp_code.c    1.0   5/22/91
*    Created by : M.L.Holko , Soil Conservation Service, USDA
*    Purpose: function
*			Provide a series of functions to look up FIPS
*			State Plane codes and State Plane projection 
*			Parameters.
*    Input arguements : empty string
*    Output arguements: projection parameters in string
*
*		Note: All functions are callable directly see
*			g.get_stp	g.get_fips	g.stp_proj
*
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
char *strtok();
#include <sys/types.h>

void get_stp_proj(string)
char string[];
{
int code;

if ((code = get_stp_num(0,0)) == 0) 
	G_fatal_error("ERROR: Can not get State Plane Code Number\n") ;
if (get_stp_code(code,string) == 0)
	G_fatal_error("ERROR: This should not happen see your system admin\n");
}

int get_stp_code(code,string)
int code;
char string[];
{
char nad27[100], buff[200], *p;
int ier, gotit=0, stp;
FILE *fp;


	sprintf(nad27,"%s/etc/state27\0",getenv("GISBASE"));
/* open input */
	   if ((ier = access(nad27,4)) != 0)
		{
		sprintf(buff,"ERROR: Can not open NAD27 file %s\n",nad27);
		G_fatal_error(buff);
		}
	   fp = fopen (nad27,"r");
	   while (! gotit) {
		if (fgets(buff,200,fp) == NULL) break;
		if (buff[0] != '#') {
			sscanf(buff,"%d:",&stp);
			if (stp == code) {
				p = strtok(buff,":");
				p = strtok(NULL,"\n");
				while (*p == ' ') p++;
				sprintf(string,"%s",p);
				gotit = 1;
				}
			}
		}
fclose(fp);
return(gotit);
}





int get_stp_num(sf,cf)
int sf, cf;
{
	FILE *fipsfile;
    char FIPSfile[100], buff[81];
	int NUM_ZON, sfips, cfips, SFIPS=0, CFIPS=0, lookup, i;
	int record, ier, icode, reccnt;
	char STabbr[50], COname[50], answer[50];

	sprintf(FIPSfile,"%s/etc/FIPS.code\0",getenv("GISBASE"));

try_agin:

/* open input */
fipsfile = fopen (FIPSfile,"r");
if ( sf == 0 || cf == 0 ) {
	   if ((ier = access(FIPSfile,4)) != 0)
			{
			G_fatal_error("ERROR: Can not open FIPS code file\n") ;
			}
/* Get State and county FIPS */
		SFIPS = sf; CFIPS = cf;
		get_fips(&SFIPS,&CFIPS,fipsfile);
		}
else
	{SFIPS = sf; CFIPS = cf;}

/* combine SFIPS and CFIPS to make lookup */
/*DEBUG fprintf(stderr,"FIPS = %d %d\n",SFIPS,CFIPS);*/
	   for (record=0;;++record)
	        {
			icode = 0;
			reccnt++;
			if (fgets(buff,80,fipsfile) == NULL) break;
			sscanf (buff,"%d%d%s%s%d",&sfips,&cfips,STabbr,COname,&NUM_ZON);
/* compare for match */
	       if (SFIPS == sfips && CFIPS == cfips)
				{
				icode = 1;
				break;
				}
			}      			/* end file search */
  	if (icode == 0)
			{                    /* no match */
			fprintf(stderr,"No match of fips state %d county %d \n",SFIPS,CFIPS);
			fclose (fipsfile);
			goto try_agin;
			}


/**** SPECIAL CASE FOR MICHIGAN ****, could be mercator or lambert */
	if (SFIPS == 26)
	  {
          fprintf(stderr,"For Michigan select- 1- East to West\n");
          fprintf(stderr,"                     2- North to South\n: ");
	  gets(answer);
	  if (strlen(answer) == 0) return(0);
	  if (*answer != '1' && *answer != '2') return(0);
	  if (*answer == '2') NUM_ZON = NUM_ZON + 10;
	  }
/**** SPECIAL CASE FOR ALASKA *****  */
	if (SFIPS == 2)
	  {
	  i = 0;
	  while ( i < 1 || i > 9) {
      fprintf(stderr,"For Alaska enter the zone (1 through 9): ");
	  gets(answer);
	  if (strlen(answer) == 0) return(0);
	  sscanf(answer,"%d",&i);
	  }
	  NUM_ZON = NUM_ZON + i;
	  }


 /* all done, good-bye */
        fclose(fipsfile);
	return(NUM_ZON);
}

int get_fips(s,c,fp)
int *s,*c;
FILE *fp;
{
int i, FIPS = 0, NUM_ZON, SFIPS, sfips, cfips;
char STabbr[50], COname[50], answer[50], buff[81], k;

fprintf(stderr,"Enter State FIPS code or RETURN if unknown :");
gets(answer);
if (strlen(answer) == 1 || strlen(answer) == 2)  {
	sscanf(answer,"%d",s);
	}
if (*s != 0) {
	fprintf(stderr,"Enter County FIPS code or RETURN :");
	gets(answer);
	if (strlen(answer) > 0 && strlen(answer) < 4)  {
		sscanf(answer,"%d",c);
		}
	}
if (*s == 0) {
/*  get FIPS code arguements */
	while (FIPS == 0) {
		{
   	     fprintf(stderr,"Enter Two-Letter State or Territory Abbreviation :");
   	     gets(answer);
		if (strlen(answer) == 2) {
			while (fgets(buff,80,fp) != NULL) {
				sscanf (buff,"%d%d%s%s%d",&sfips,&cfips,STabbr,COname,&NUM_ZON);
				for (i=0;i<2;i++)
					{if (islower(answer[i])) answer[i] = toupper(answer[i]);
					if (islower(STabbr[i])) STabbr[i] = toupper(STabbr[i]);
					}
				if (strncmp(answer,STabbr,2) == 0) {FIPS = sfips;break;}
				}
			}
		}
		rewind(fp);
		}
	*s = FIPS;
	FIPS = 0;
	}
if (*c == 0 && *s > 0) {
	while (FIPS == 0) {
		{
   	     fprintf(stderr,"Enter County Name :");
   	     gets(answer);
			if (strlen(answer) > 0) {
				while (fgets(buff,80,fp) != NULL) {
					sscanf (buff,"%d%d%s%s%d",&sfips,&cfips,STabbr,COname,&NUM_ZON);
					for (i=0;i<strlen(answer);i++) 
						if (islower(answer[i])) answer[i] = toupper(answer[i]);
					for (i=0;i<strlen(COname);i++) 
						if (islower(COname[i])) COname[i] = toupper(COname[i]);
					if (strncmp(answer,COname,strlen(COname)) == 0 && sfips == *s) {FIPS = cfips;break;}
					}
				if (FIPS == 0) fprintf(stderr,"County %s not found\n",answer);
				}
			}
		rewind(fp);
		}
	*c = FIPS;
	}
return;
}

int get_st_fips(s,fp)
int *s;
FILE *fp;
{
int i, FIPS = 0, NUM_ZON, SFIPS, sfips, cfips;
char STabbr[50], COname[50], answer[50], buff[81], k;

/*  get FIPS code arguements */
	while (FIPS == 0) {
		{
   	     fprintf(stderr,"Enter Two-Letter State or Territory Abbreviation :");
   	     gets(answer);
		if (strlen(answer) == 2) {
			while (fgets(buff,80,fp) != NULL) {
				sscanf (buff,"%d%d%s%s%d",&sfips,&cfips,STabbr,COname,&NUM_ZON);
				for (i=0;i<2;i++)
					{if (islower(answer[i])) answer[i] = toupper(answer[i]);
					if (islower(STabbr[i])) STabbr[i] = toupper(STabbr[i]);
					}
				if (strncmp(answer,STabbr,2) == 0) {FIPS = sfips;break;}
				}
			}
		}
		rewind(fp);
		}
	*s = FIPS;
	FIPS = 0;
return;
}
