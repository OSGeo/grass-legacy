/* 01/21/93 */
/*Preliminary step to converting an Atlas BNA file to GRASS.*/
/*It eliminates all duplicate arcs from the geo file.*/

/* grfix.c */

#include <stdio.h>
#include <time.h>
#include <string.h>

char inbuf2[100];
char b1[50], b2[50];
double x1pts[4096], y1pts[4096], x2pts[4096], y2pts[4096];
int aa[100], ab[100], a1, a2, q=0, x=0, b3, b23, bs=0;
FILE *fp1, *fp2, *fp3, *log;

grfix(inp,out,tmp)
char *inp, *out, *tmp;
{
	char *buf1p, b21[50], b22[50];
	int i, n, m, notdone;
	int done=0, w=0;
	int match();

	if((log=fopen("log","a"))==NULL)
	{	return(-1);
	}
	if((fp1=fopen(inp,"r"))==NULL)
	{	printf("ERROR - see log\n");
		fprintf(log,"USAGE: grfix input-file output-file temp-file\n");
		fprintf(log,"cannot find input-file %s\n",inp);
		fprintf(log,"***********************************\n");
		return(-1);
	}
	if((fp2=fopen(out,"w"))==NULL)
	{	printf("ERROR - see log\n");
		fprintf(log,"USAGE: grfix input-file output-file temp-file\n");
		fprintf(log,"cannot create output-file %s\n",out);
		fprintf(log,"***********************************\n");
		return(-1);
	}
	if((fp3=fopen(tmp,"w"))==NULL)
	{	printf("ERROR - see log\n");
		fprintf(log,"USAGE: grfix input-file output-file temp-file\n");
		fprintf(log,"cannot create temp-file %s\n",tmp);
		fprintf(log,"***********************************\n");
		return(-1);
	}
	fprintf(log,"%s START GRFIX.\n",out);
	print_time();

	while(!done)
	{
	while(fgets(inbuf2,100,fp1)!=NULL)
	{
		w=0;
		/* Read in Area A1*/
		/* changed if to while 10-01-91*/
		while(inbuf2[0]!=34) fgets(inbuf2,100,fp1);
		buf1p=strtok(inbuf2,",\n");
		strcpy(b1,buf1p);
		buf1p=strtok(NULL,",\n");
		strcpy(b2,buf1p);
		buf1p=strtok(NULL,",\n");
		sscanf(buf1p,"%d",&b3);
		if(b3 < 0) bs = 1;
		b3=abs(b3);
		if(b3 > 4096)
		{	printf("ERROR - see log\n");
			fprintf(log,"Maximum Points in an Area (4096) exceeded\n");
			fprintf(log,"***********************************\n");
			return(-1);
		}

/*printf("REMOVE DUP ARCS--Area: b1= %s, b2= %s, b3= %d\n",b1,b2,b3);*/

		for(i=0; i<b3; i++)
		{
			fgets(inbuf2,100,fp1);
			buf1p=strtok(inbuf2,",\n");
			sscanf(buf1p,"%lf",&x1pts[i]);
			buf1p=strtok(NULL,",\n");
			sscanf(buf1p,"%lf",&y1pts[i]);
		}
		a1=0;
		while(fgets(inbuf2,100,fp1)!=NULL)
		{
			/* Read in area A2*/
		/* changed if to while 10-01-91*/
			while(inbuf2[0]!=34) fgets(inbuf2,100,fp1);
			buf1p=strtok(inbuf2,",\n");
			strcpy(b21,buf1p);
			buf1p=strtok(NULL,",\n");
			strcpy(b22,buf1p);
			buf1p=strtok(NULL,",\n");
			sscanf(buf1p,"%d",&b23);
			b23=abs(b23);
			if(b23 > 4096)
			{	printf("ERROR - see log\n");
				fprintf(log,"Maximum Points in an Area (4096) exceeded\n");
				fprintf(log,"**************************\n");
				return(-1);
			}

			for(i=0; i<b23; i++)
			{
				fgets(inbuf2,100,fp1);
				buf1p=strtok(inbuf2,",\n");
				sscanf(buf1p,"%lf",&x2pts[i]);
				buf1p=strtok(NULL,",\n");
				sscanf(buf1p,"%lf",&y2pts[i]);
			}
			/*added 10-01-91*/
			if(b1==b21 && b2==b22) continue;
			notdone=1; a2=0;
			while(notdone)
			{
				m = match();
				if(a1<b3-1 && m!=0 && a2<b23-1) continue;
				if(a2<b23-1)  /*must be 2 or more points*/
				{
					fprintf(fp3,"%s,%s,%d\n",b21,b22,b23-a2);
/*printf("After match call %s: b23=%d\n",b21,b23); */
					for(n=a2; n<b23; n++)
						fprintf(fp3,"%lf,%lf\n",x2pts[n],y2pts[n]);
				}
				while(q>0 && a1>=b3-1)
				{
					q--;
					a2=aa[q];
					b23=ab[q];
					if(a2<b23-1)  /*must be 2 or more points*/
					{
/*printf("From q-stack %s: a2=%d b23=%d\n",b21,a2,b23); */
						fprintf(fp3,"%s,%s,%d\n",b21,b22,b23-a2);
						for(n=a2; n<b23; n++)
							fprintf(fp3,"%lf,%lf\n",x2pts[n],y2pts[n]);
					}
				}
				if(q>0 && a1<b3-1)
				{
					q--;
					a2=aa[q];
					b23=ab[q];
					continue;
				}
				notdone = 0;
			}
			if(m!=0 && a1<b3-1) continue;
			a2=0;
			/*added on 10-02-91*/
			if(a1>=b3-1) break;
		}
		/*if A1 is gone, continue;*/
		if(a1>=b3-1) 
		{
			a1=0;
			/*added on 10-02-91*/
			while(fgets(inbuf2,100,fp1)!=NULL)
				fprintf(fp3,"%s",inbuf2);
			fswitch(inp,tmp);
			w=1;
			/*end add*/
			continue;
		}
/* Write rest of A1 to output file.*/
		if(bs == 1)
			fprintf(fp2,"%s,%s,-%d\n",b1,b2,b3-a1);
		else
			fprintf(fp2,"%s,%s,%d\n",b1,b2,b3-a1);
/*printf("At end: a1= %d\n",a1);*/
		for(n=a1; n<b3; n++)
			fprintf(fp2,"%lf,%lf\n",x1pts[n],y1pts[n]);
		a1=0;
		fswitch(inp,tmp);
		w=1;
	}	

	if(w==0)
	{
		fswitch(inp,tmp);
		w=1;
	}
	else done=1;
	}

/* Final close of all files.*/
	fclose(fp1);
	fclose(fp2);
	fclose(fp3);
	print_time();
	fprintf(log,"%s END OF GRFIX.\n",out);
	fclose(log);
	return(0);
}

print_time()
{
	int sec, min, hour, day, mon, yr;
	struct tm *curr;
	long ltime;

	time(&ltime);
	curr = localtime(&ltime);
	hour = curr->tm_hour;
	min = curr->tm_min;
	sec = curr->tm_sec;
	day = curr->tm_mday;
	mon = curr->tm_mon + 1;
	yr = curr->tm_year;
	fprintf(log,"Date %i-%i-%i Time %i:%i:%i\n",mon,day,yr,hour,min,sec);
}

/* Close input and temp files.*/
fswitch(ar1,ar3)
char *ar1, *ar3;
{
	fclose(fp1);
	fclose(fp3);
	if(x==0)
	{
		fp3=fopen(ar1,"w");
		fp1=fopen(ar3,"r");
		x=1;
	}
	else
	{
		fp1=fopen(ar1,"r");
		fp3=fopen(ar3,"w");
		x=0;
	}
}

match()
{
	int i, j, k=0, m=0;

	for (i=a1; i<b3-1; i++)
	{
		for(j=a2; j<b23; j++)
		{
			if(x1pts[i] == x2pts[j] && y1pts[i] == y2pts[j])
			{
				if(x1pts[i+1] == x2pts[j+1] && y1pts[i+1] == y2pts[j+1] && j<b23-1)
				{
					k=2; m=1;
					while(i+k < b3 && j+k < b23)
					{
						if(x1pts[i+k] == x2pts[j+k] && y1pts[i+k] == y2pts[j+k])
						{
							k++;	
						}
						else break;
					}
					fix(i,j,k-1,m);
					return m;
				}
				else if (j==0) continue;
				else if (x1pts[i+1] == x2pts[j-1] && y1pts[i+1] == y2pts[j-1])
				{
					k=2; m=-1;
					while(i+k < b3 && j-k >= 0)
					{
						if(x1pts[i+k] == x2pts[j-k] && y1pts[i+k] == y2pts[j-k])
						{
							k++;
						}
						else break;
					}
/*printf("In match: j= %d\n",j); */
					fix(i,j,k-1,m);
					return m;
				}
			}
		}
	}
	return m;
}

fix(i,j,k,m)
int i, j, k, m;
{
	int n;

/*printf("Write to output: %s\n",b1);*/
	if(b3 == 1)
		fprintf(fp2,"%s,%s,-%d\n",b1,b2,k+1);
	else
		fprintf(fp2,"%s,%s,%d\n",b1,b2,k+1);
	for(n=i; n<=i+k; n++)
		fprintf(fp2,"%lf,%lf\n",x1pts[n],y1pts[n]);

	if(i>a1)
	{
		fprintf(fp3,"%s,%s,%d\n",b1,b2,i+1-a1);
		for(n=a1; n<=i; n++)
			fprintf(fp3,"%lf,%lf\n",x1pts[n],y1pts[n]);
	}
	a1 = i+k;
/*printf("In fix: j= %d\n",j); */

	if(j>a2 && m==1)
	{
		/* save limits of A2c for later*/
		if(j+k+1<b23) 
		{	
			aa[q]=j+k;
			ab[q]=b23;
			q++;
		}
		b23 = j+1;	
	}
	if(j-k>a2 && m==-1)
	{
		if(j+1<b23)
		{	
			aa[q]=j;
			ab[q]=b23;
			q++;
		}
		b23 = j-k+1;	
	}

	if(m==1 && j==a2)
		a2 = k;
	if(m==-1 && j-k==a2)
		a2 = j;
}
