/* 01/29/93 */
/*Preliminary step to converting an ASCII file (.bna) exported from AGIS
  to GRASS format.  Isle edits the ASCII file to eliminate extra lines 
  extending to islands.  It also eliminates some digitizing errors like
  the consecutive repetition of the same coordinate pair.  Garbage data
  is recognized, so it can be fixed before processing.  It outputs the 
  updated ASCII file.*/

  /*isle.c*/

#include <stdio.h>
#include <string.h>

char inbuf2[100];
FILE *fp1, *fp2, *log;

isle(inp,out)
char *inp, *out;
{
	double xpts[4096], ypts[4096], *xp, *yp, xi[100], dx, dy, ex, ey;
	double Nmax, Smax, Wmax, Emax;
	char *buf1p, b1[50], b2[50];
	int z, ii, q, vod=0;
	int i, b3, np, x, j, ia=0, a[1000], a2[1000], a3[1000], sav[100], east;
	long curr;

	if((log=fopen("log","a"))==NULL)
	{	return(-1);
	}
	if((fp1=fopen(inp,"r"))==NULL)
	{	printf("ERROR - see log\n");
		fprintf(log,"USAGE: isle input-file output-file\n");
		fprintf(log,"cannot find input-file %s\n",inp);
		fprintf(log,"************************************\n");
		return(-1);
	}
	if((fp2=fopen(out,"w"))==NULL)
	{	printf("ERROR - see log\n");
		fprintf(log,"USAGE: isle input-file output-file\n");
		fprintf(log,"cannot create output-file %s\n",out);
		fprintf(log,"************************************\n");
		return(-1);
	}
	fprintf(log,"%s START ISLE.\n",inp);
	/* Read input records.*/
	while(fgets(inbuf2,100,fp1)!=NULL)
	{
		if(inbuf2[0]!=34)
		{	printf("ERROR - see log\n");
			fprintf(log,"ERROR: Area label must begin with double quote.\n");
			fprintf(log,"Current line=%s\n",inbuf2);
			fprintf(log,"Last Area=%s,%s\n",b1,b2);
			fprintf(log,"************************************\n");
			return(-1);
		}
		buf1p=strtok(inbuf2,",\n");
		strcpy(b1,buf1p);
		buf1p=strtok(NULL,",\n");
		strcpy(b2,buf1p);
		buf1p=strtok(NULL,",\n");
		sscanf(buf1p,"%d",&b3);
		if(b3 < -1)
		{	
			fprintf(log,"%s ERROR: LINE IN AREA FILE \n",inp);
			fclose(fp1);
			fclose(fp2);
			fclose(log);
			return(-1);
		}
		if(b3 == 1) 
		{
			fprintf(log,"%s ERROR: POINT IN AREA FILE \n",inp);
			fclose(fp1);
			fclose(fp2);
			fclose(log);
			return(-1);
		}
		if(b3 < 1) return(-1);
		/*printf("ISLAND-VALIDATION Areas completed: %d\n",ia);
		ia++;*/

/* j is the number of pairs in sub-area.*/
/* x is index for saving sub-area info.*/

		j=1; x=0;
		xp = xpts;
		yp = ypts;
		for(np=0; np<b3; np++, xp++, yp++)
		{
			if(fgets(inbuf2,100,fp1)==NULL)
			{	printf("ERROR - see log\n");
				fprintf(log,"ERROR: EOF came too soon.\n");
				fprintf(log,"************************************\n");
				return(-1);
			}
			if(inbuf2[0]==34)  /*double quote mark*/
			{
				j--;
				curr=strlen(inbuf2);
				fseek(fp1,-curr,1);
		fprintf(log,"Count Error Fixed: %s, %d\n",b1,b3);
				break;
			}

			for(i=0; inbuf2[i]!=10 && inbuf2[i]!=NULL; i++)  /*new line character*/
			{
				if((inbuf2[i]<44 || inbuf2[i]>57 || inbuf2[i]==47))
				{  /*0-9 comma period minus sign are ok
					inbuf2[i]=10;
					inbuf2[i+1]=NULL;
					printf("ERROR - see log\n");
					fprintf(log,"ERROR: Invalid coordinate data. Only 0-9, comma, period, minus sign.\n");
					fprintf(log,"AREA: %s,%s,%d LINE: %d\n",b1,b2,b3,np+1);
					fprintf(log,"************************************\n");
					return(-1);*/
				}
			}

			buf1p=strtok(inbuf2,",\n");
			sscanf(buf1p,"%lf",&xpts[np]);
			buf1p=strtok(NULL,",\n");
			sscanf(buf1p,"%lf",&ypts[np]);

			if (np==0)
			{
				Smax = Nmax = *yp;
				Emax = Wmax = *xp;
			}
			if(x==0)
			{
				Nmax = ( *yp > Nmax) ? *yp : Nmax;
				Smax = ( *yp < Smax) ? *yp : Smax;
				Emax = ( *xp > Emax) ? *xp : Emax;
				Wmax = ( *xp < Wmax) ? *xp : Wmax;
			}
			if(np>0 && xpts[np]==xpts[np-1] && ypts[np]==ypts[np-1])
			{
				np--; b3--; j--;
				yp--; xp--; 
			}

			if(np>0 && xpts[0]==xpts[np] && ypts[0]==ypts[np])
			{
				if(x==0)
				{  /*main polygon*/
					a[x]=j; j=1; a2[x]=np+1;
					if(np<b3-1) x++;
					a3[x]=0;
				}
				else
				{
					if(xpts[a2[x-1]]==xpts[np-1] &&
					   ypts[a2[x-1]]==ypts[np-1])
					{  /*island*/
						a[x]=j-1; j=1; a2[x]=np+1;
						if(Nmax < ypts[np-1] || Smax > ypts[np-1] || Emax < xpts[np-1] || Wmax > xpts[np-1])
						{ /*true island*/
							a3[x]=0;
		/*fprintf(log,"TRUE ISLAND:%s,%s,%d\n",b1,b2,a[x]);*/
						}
						else
						{
							i=0;
							for (z=0; z<a[0]-1; z++)
							{
								if((ypts[z]>ypts[np-1]&&ypts[z+1]<ypts[np-1]) || (ypts[z]<ypts[np-1]&&ypts[z+1]>ypts[np-1]) )
								{
									xi[i]=0.0;
									sav[i++]=z;
								}	
								else if(z>0&&ypts[z]==ypts[np-1]&&((ypts[z+1]>ypts[np-1]&&ypts[z-1]<ypts[np-1])||(ypts[z+1]<ypts[np-1]&&ypts[z-1]>ypts[np-1])))
								{
									sav[i]=-1;
									xi[i++]=xpts[z];
								}
								else if(z==0&&ypts[z]==ypts[np-1]&&((ypts[z+1]>ypts[np-1]&&ypts[np-2]<ypts[np-1])||(ypts[z+1]<ypts[np-1]&&ypts[np-2]>ypts[np-1])))
								{
									sav[i]=-1;
									xi[i++]=xpts[z];
								}
							}
							east=0;
							for (z=0; z<i;z++)
							{
								if(sav[z]>=0)
								{
									q = sav[z]+1;
									dx = xpts[q]-xpts[sav[z]];
									dy =fabs(ypts[sav[z]]-ypts[q]);
									ey =fabs(ypts[sav[z]]-ypts[np-1]);
									if(dy!=0.0)
										ex = (dx*ey)/dy;
									else
										ex = 0.0;
									xi[z] = xpts[sav[z]]+ex;
								}
								if(xi[z]>xpts[np-1])
									east++;
							}
							if(east==(east/2)*2)
							{
								a3[x]=0;
		/*fprintf(log,"TRUE ISLAND:%s,%s,%d\n",b1,b2,a[x]);*/
							}
							else
							{
								a3[x]=1;
		if(ii==0){
			fprintf(log,"CHECK AREAS WITH INTERNAL VOIDS FOR POSSIBLE LABEL PLACEMENT PROBLEMS.\n");
			printf("WARNING - see log\n");
			ii=1;
		}
		fprintf(log,"INTERNAL VOID:%s,%s,%d\n",b1,b2,a[x]);
		vod = 1;
							}
						}
						if(np<b3-1) x++;
					}
					else
					{  /*interior island*/
		if(ii==0){
			fprintf(log,"CHECK AREAS WITH INTERIOR ISLANDS FOR POSSIBLE LABEL PLACEMENT PROBLEMS.\n");
			printf("WARNING - see log\n");
			ii=1;
		}
						a[x]=j+1; j=1;
						a2[x]=np+1;
						a2[x-1]--;		
						a3[x]=1;
						if(np<b3-1) x++;
		fprintf(log,"INTERIOR ISLAND:%s,%s,%d\n",b1,b2,a[x]);
					}
				}
			}
			else j++;
		}

		/*Write results to output file*/
		fprintf(fp2,"%s,%s,%d\n",b1,b2,a[0]);
		for(i=0; i<a[0]; i++)
		{	
			fprintf(fp2,"%lf,%lf\n",xpts[i],ypts[i]);
		}
		if (x > 0)
		{
			for(j=1; j<=x; j++)
			{
				if(vod == 1)
					fprintf(fp2,"V\n");
				fprintf(fp2,"%s,%s,%d\n",b1,b2,a[j]);
				for(i=0; i<a[j]; i++)
				{	
					fprintf(fp2,"%lf,%lf\n",xpts[a2[j-1]+i],ypts[a2[j-1]+i]);
				}
			}
			if(vod == 1) vod = 0;
		}
	}
	fprintf(log,"%s AREA FILE - END OF ISLE.\n",inp);
	fclose(fp1);
	fclose(fp2);
	fclose(log);
	return(0);
}
