/*
 * $Id$
 * atl2dig1.c 01/29/93 */
/* Creates cats file and att file in GRASS format from an ASCII file
   that represents an ATLAS area, line or point file.  */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"

char inbuf[2000],outbuf[1000];
FILE *fp1, *fp2, *att, *jog;

Vimport1(inp,out)
char *inp,*out;
{
	int x, y, z, n_read, np, cat_num, att_num, i, sav[100], east, west;
	char buf[100],buf1[100], *buf1p;
	char dig_name[100];
	struct Categories cats;
	double xpts[10000], ypts[10000], *xp, *yp, xi[100], dx, dy, ex, ey;
	double Cy, Cx, Nmax, Smax, Emax, Wmax, Eclose, Wclose, E2, W2;
	int stzone, zone, vod=0;

char b1[80],b2[80],*b;
int b3,b4,bs,qa=0;


	if((jog=fopen("log","a"))==NULL)
		return(-1);
	
	/*if(argc!=4){
		printf("ERROR - see log\n");
		fprintf(jog,"Usage: Vimport.atlas1 atlas_file dig_file\n");
		fprintf(jog,"%s\n",out);
		fprintf(jog,"******************************\n");
		return(-1);
	}*/
	G_strcpy(buf,inp);
	if((fp1=fopen(inp,"r")) == NULL){
		printf("ERROR - see log\n");
		fprintf(jog,"File not found: %s\n",inp);
		fprintf(jog,"******************************\n");
		return(-1);
	}
	G_strcpy(dig_name, out);
	G_gisinit("Vimport.atlas1");
	fprintf(jog,"%s START Vimport.atlas1\n",out);

	if(G_read_vector_cats(dig_name, G_mapset(), &cats) == -1){
		fprintf(jog,"Creating new dig_cats file %s\n",dig_name);
		G_init_cats((CELL)0, "Created from v.in.atlas", &cats);
		G_set_cat(0, "no data", &cats);
		/*G_write_vector_cats(dig_name, &cats);*/
	}
	fp2 = G_fopen_append("dig_cats", dig_name);
	np=0;
	att_num = 1;
	buf1p = buf1;
	xp = xpts;
	yp = ypts;
	/* now start reading thru file one area, line or point at a time*/
	/* 1-21-93 Now handles commas inside name */
	while(fgets(inbuf,2000,fp1) != NULL){
	   if( inbuf[0] == 86) 
		{
			vod = 1;
			fgets(inbuf,2000,fp1);
		}
		buf1p=strtok(inbuf+1, "\"\n");
		G_strncpy(b1, buf1p, strlen(buf1p));
		b=strtok(NULL, "\"\n");
		buf1p=strtok(NULL, "\"\n");
		if(buf1p[0]==44)
		{
			b2[0]=NULL;
			sscanf(buf1p+1, "%d", &bs);
		}
		else
		{
			G_strncpy(b2, buf1p, strlen(buf1p));
			buf1p=strtok(NULL, ",\n");
			sscanf(buf1p, "%d", &bs);
		}
		b3 = abs(bs);
		if(strcmp(b1,b2)!=0 && strlen(b2)>0)
		{
			G_strcat(b1, ":");
			G_strcat(b1, b2);
		}
		if (qa == 0)
		{
			if((att = G_fopen_new("dig_att", dig_name)) == NULL)
			{	
				printf("ERROR - see log\n");
				fprintf(jog,"Can't create new att file %s\n",dig_name);
				fprintf(jog,"******************************\n");
				return(-1);
			}
		}
		qa = 1;
		np = 0;
		Nmax = Smax = Emax = Wmax = 0.0;
		/* read coordinate pair data for the area, line or point*/
		for (b4=1; b4 <= b3; b4++){
			fgets(inbuf,2000,fp1); 
			xp = xpts;
			yp = ypts;
			/* move up to ',' or 'CR'  */
			buf1p=strtok(inbuf, ",\n");
			sscanf(buf1p, "%lf", &xpts[np]);
			buf1p=strtok(NULL, ",\n");
			sscanf(buf1p, "%lf", &ypts[np]);
/*printf(" %d, :%lf: :%lf:\n", np, xpts[np], ypts[np]);*/
			np++;
		}
		if(vod == 1)
		{
			vod = 0;
			continue;
		}
		xp = xpts;
		yp = ypts;
		for (y=0;y<np;y++,xp++,yp++){

		if(bs < -1)
		{
			dx=xpts[1]-xpts[0];
			dy=ypts[1]-ypts[0];
			ey=dy/2.0;
			ex=dx/2.0;
			Cx=xpts[0]+ex;
			Cy=ypts[0]+ey;
		}

		/* find the centroid of the area */
		/* Nmax is the northernmost point in the area.
   		   Smax is the southernmost point in the area.
  		   Emax is the easternmost point in the area.
 	 	   Wmax is the westernmost point in the area. */

			if(bs > 1){
				if (y == 0){
					Smax = Nmax = *yp;
					Emax = Wmax = *xp;
				}
				Nmax = ( *yp > Nmax) ? *yp : Nmax;
				Smax = ( *yp < Smax) ? *yp : Smax;
				Emax = ( *xp > Emax) ? *xp : Emax;
				Wmax = ( *xp < Wmax) ? *xp : Wmax;
			}
		}

/* Determine coordinates of label point within area. */

/* Cy, Cx are the x,y coordinates of the label point.
   sav array contains the numbers (0 to np) of the first point of each pair 
     of points whose line segment intersects the Cy axis.
   xi array contains the x coordinates of points on the area boundary that 
     have Cy as a y coordinate.
   np is the total number of points in the area. */

		if(bs > 1){
			Cy = (Nmax + Smax) / 2.0;
			Cx = (Wmax + Emax) / 2.0;
			i = 0;
			for(z=0; z<np-1; z++)
			{
				if((ypts[z]>Cy&&ypts[z+1]<Cy)||(ypts[z]<Cy&&ypts[z+1]>Cy))
				{
					xi[i]=0.0;
					sav[i++]=z;
				}
				else if(z>0&&ypts[z]==Cy&&((ypts[z+1]>Cy&&ypts[z-1]<Cy)||(ypts[z+1]<Cy&&ypts[z-1]>Cy)))
				{
					sav[i]=-1;
					xi[i++]=xpts[z];
				}
				else if(z==0&&ypts[z]==Cy&&((ypts[z+1]>Cy&&ypts[np-2]<Cy)||(ypts[z+1]<Cy&&ypts[np-2]>Cy)))
				{
					sav[i]=-1;
					xi[i++]=xpts[z];
				}
			}
			if((i!=(i/2)*2)||(i==0))
			{
				printf("ERROR - see log (np=%d, Nmax=%f, Smax=%f, Cy=%f)\n",np,Nmax,Smax,Cy);
				fprintf(jog,"Open Area Error: %s,%d\n",b1,b3);
				for(z=0; z<np-1; z++)
					fprintf(jog,"%18.8f, %18.8f\n",xpts[z],ypts[z]);
				continue;
			}

/*east, west are the number of xi points to the east and west of the centroid 
    of the area.
  dx is the x distance between a sav point and its partner.
  dy is the y distance between a sav point and its partner.
  ex is the x distance between a sav point and its Cy intersecting point.
  ey is the y distance between a sav point and Cy.
  Eclose is the xi point east of the centroid which is closest to it.
  Wclose is the xi point west of the centroid which is closest to it.
  E2 is the xi point east of the centroid which is 2nd closest to it.
  W2 is the xi point west of the centroid which is 2nd closest to it.*/

			east=west=Eclose=Wclose=E2=W2=0;
			for(z=0;z<i;z++)
			{
				if(sav[z]>=0)
				{
					x = sav[z]+1;
/* dx is positive when moving east */
					dx = xpts[x]-xpts[sav[z]];
					dy = fabs(ypts[sav[z]]-ypts[x]);
					ey = fabs(ypts[sav[z]]-Cy);
					ex = (dx*ey)/dy;	
					xi[z] = xpts[sav[z]]+ex;
/*		fprintf(jog,"xi=%18.8f\n",xi[z]);
		fprintf(jog,"xpts[sav[z]]=%18.8f, ex=%18.8f\n",xpts[sav[z]],ex);
		fprintf(jog,"dx=%18.8f, dy=%18.8f, ey=%18.8f\n",dx,dy,ey);*/
				}
				if(xi[z]>Cx)
				{
					east++;	
					if(Eclose==0.0)
					{
						Eclose = xi[z];
					}
					else if(fabs(xi[z]-Cx)<fabs(Eclose-Cx))
					{
						E2 = Eclose;
						Eclose = xi[z];
					}
					else if((E2==0.0)||(fabs(xi[z]-Cx)<fabs(E2-Cx)))
					{
						E2 = xi[z];
					}
				}
				else if(xi[z]<=Cx)
				{
					west++;
					if(Wclose==0.0)
					{
						Wclose = xi[z];
					}
					else if(fabs(Cx-xi[z])<fabs(Cx-Wclose))
					{
						W2 = Wclose;
						Wclose = xi[z];
					}
					else if((W2==0.0)||(fabs(Cx-xi[z])<fabs(Cx-W2)))
					{
						W2 = xi[z];
					}
				}
			}
		/*	fprintf(jog,"east=%d, west=%d\n",east,west);*/
/* If the centroid is outside of Area polygon, move it to the East midway i
   between Eclose and E2 unless none of the polygon lies to the east.*/

			if((east==(east/2)*2) && east > 0)
			{
			/*	fprintf(jog,"AREA LABEL FOR %s MOVED FROM %18.8f ",b1,Cx);/**/
				if(Eclose!=0.0 && E2!=0.0)
					Cx = (Eclose + E2) / 2;
			/*	fprintf(jog,"TO %18.8f\n",Cx);
				fprintf(jog,"Eclose=%18.8f\n",Eclose);
				fprintf(jog,"E2=%18.8f\n",E2);/**/
			}
/* When polygon only lies to the west move the centroid west to get it inside.*/

			else if((west==(west/2)*2) && west > 0)
			{
			/*	fprintf(jog,"AREA LABEL FOR %s MOVED FROM %18.8f ",b1,Cx); */
				if(Wclose!=0.0 && W2!=0.0)
					Cx = (Wclose + W2) / 2;
			/*	fprintf(jog,"TO %18.8f\n",Cx);
				fprintf(jog,"Wclose=%18.8f\n",Wclose);
				fprintf(jog,"W2=%18.8f\n",W2); */
			}
/* When the centroid is inside polygon already, center it between Eclose and 
   Wclose. */

			if((east!=(east/2)*2)&&east>0&&west>0&&west!=(west/2)*2)
			{
		/*		fprintf(jog,"ALREADY INSIDE\n");
				fprintf(jog,"AREA LABEL FOR %s MOVED FROM %18.8f ",b1,Cx); */
				if(Eclose!=0.0 && Wclose!=0.0) 
					Cx = (Eclose + Wclose) / 2;
		/*		fprintf(jog,"TO %18.8f\n",Cx);
				fprintf(jog,"Eclose=%18.8f\n",Eclose);
				fprintf(jog,"Wclose=%18.8f\n",Wclose); */
			}
		}

		/* add to category list if new label */
		z = 0;
		/*printf("count :%d:\n", cats.ncats);*/
		for (x=0; x < cats.ncats; x++){
		/*printf("x %d:\n", x);*/
		/*printf("num %d: list :%s: label :%s:\n", cats->labels[i], cats.labels[i], b1);*/
			if((y = strncmp(cats.labels[i], b1, 99)) == 0){
				z=1;
				break;
			}
		}
		if (z != 1)
		{
			G_set_cat(cats.ncats, b1, &cats); /* was cats.count in 4.x*/
			/*fprintf(fp2,"%d:%s\n",cats.ncats -1,b1);*/
		}
		att_num = x;

		/* write record to attribute file */
		if(bs < -1){
			fprintf( att, "L  %18.8f  %18.8f        %d\n", Cx, Cy, att_num);
		}
		else if(bs > 1){
			fprintf( att, "A  %18.8f  %18.8f        %d\n", Cx, Cy, att_num);
		}
		else if(bs == 1){
			fprintf( att, "P  %18.8f  %18.8f        %d\n", xpts[0], ypts[0], att_num);
		}
	}	
	if (bs != 1)
		fclose(att);
	G_write_vector_cats(dig_name,&cats);
	fprintf(jog,"%s END of Vimport.atlas1\n",out);
	fclose(fp1);
	fclose(fp2);
	fclose(jog);
	return(0);	
}
