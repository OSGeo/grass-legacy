/* atl2dig2.c 08/03/92 */
/* Creates dig file in Grass format from ASCII file representing an
ATLAS area, line or point file. */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"

char inbuf[2000],outbuf[1000];
FILE *fp1, *dig, *jog;

main(argc,argv)
int argc;
char **argv;
{
	int x, y, z, n_read, np, on_line;
	char buf[100], buf1[100], *buf1p;
	char dig_name[100];
	struct dig_head d_head;
	double xpts[10000], ypts[10000], *xp, *yp;
	double Nmaxx, Smaxx, Emaxx, Wmaxx;
	int zone=99;

char b1[80],b2[80],*b,typ[4];
int b3,b4,bs;

	struct Map_info Map;
	struct line_pnts *Points;

	G_gisinit("Vimport.atlas2");
	if((jog=fopen("log","a"))==NULL)
	{	return(-1);
	}
	/*if(argc!=4){
		printf("ERROR - see log\n");
		fprintf(jog,"Usage: Vimport.atlas2 atlas_file dig_file\n");
		fprintf(jog,"%s\n",argv[2]);
		fprintf(jog,"******************************\n");
		return(-1);
	}*/
	G_strcpy(buf,argv[1]);
	if((fp1=fopen(argv[1],"r")) == NULL){
		printf("ERROR - see log\n");
		fprintf(jog,"File not found: %s\n",argv[1]);
		fprintf(jog,"******************************\n");
		return(-1);
	}
	G_strcpy(dig_name, argv[2]);
	fprintf(jog,"%s START Vimport.atlas2\n",argv[2]);

	/*G_ask_any("NEW DIG FILE", dig_name, "dig", "DIG",1);*/
	if(Vect_open_old(&Map, dig_name,G_mapset()) > 0)
	{	/*G_fatal_error(" Digit file already exists",dig_name);*/
		printf("ERROR - see log\n");
		fprintf(jog,"Digit file already exists %s\n",dig_name);
		fprintf(jog,"******************************\n");
		return(-1);
	}
	Vect_close(&Map);
	if(Vect_open_new(&Map, dig_name) != 1)
	{	/*G_fatal_error(" Can't create new dig file");*/
		printf("ERROR - see log\n");
		fprintf(jog,"Can't create new dig file %s\n",dig_name);
		fprintf(jog,"******************************\n");
		return(-1);
	}
/*	dig_write_head_binary( dig, &d_head);*/

	z=0;
	np=0;
	on_line = 0;
	buf1p = buf1;
	xp = xpts;
	yp = ypts;
	Points = Vect_new_line_struct();
	/* now start reading thru file */
	while(fgets(inbuf,2000,fp1) != NULL){
	   if( inbuf[0] != 34) fgets(inbuf,2000,fp1);
		buf1p=strtok(inbuf, ",\n");
		b = buf1p + 1;
		G_strncpy(b1, b, strlen(buf1p)-2);
		buf1p=strtok(NULL, ",\n");
		b = buf1p + 1;
		G_strncpy(b2, b, strlen(buf1p)-2);
		buf1p=strtok(NULL, ",\n");
		sscanf(buf1p, "%d", &bs);
		b3 = abs(bs);
	/*printf(":%s: :%s: :%d:\n", b1, b2, b3);*/
		if( b1 != b2 )
		{
			G_strcat(b1, ":");
			G_strcat(b1, b2);
		}
	/*printf(":%s:\n", b1);*/
		np = 0;
		/* read coordinate pair data */
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
		if (b3 == 1)
		{
			xpts[1] = xpts[0];
			ypts[1] = ypts[0];
			np = 2;
		}
		xp = xpts;
		yp = ypts;
		for (y=0;y<np;y++,xp++,yp++){
			if (on_line == 0){
				Smaxx = Nmaxx = *yp;
				Emaxx = Wmaxx = *xp;
				on_line = 1; 
			}
			Nmaxx = ( *yp > Nmaxx) ? *yp : Nmaxx;
			Smaxx = ( *yp < Smaxx) ? *yp : Smaxx;
			Emaxx = ( *xp > Emaxx) ? *xp : Emaxx;
			Wmaxx = ( *xp < Wmaxx) ? *xp : Wmaxx;
		}
 /*fprintf(stderr,"\n%lf %lf %lf %lf \n",Nmax, Smax, Wmax, Emax);*/
		if(bs < -1)
		{
			Vect_copy_xy_to_pnts(Points, xpts, ypts, np);
			Vect_write_line(&Map, LINE, Points);
			/*dig_Write_line(dig, LINE, xpts, ypts, np);*/
		}
		else if(bs > 1)
		{
			Vect_copy_xy_to_pnts(Points, xpts, ypts, np);
			Vect_write_line(&Map, AREA, Points);
		}
		else if(bs == 1)
		{
			Vect_copy_xy_to_pnts(Points, xpts, ypts, np);
			Vect_write_line(&Map, DOT, Points);
		}
	}
	G_strcpy(Map.head.line_3, "Created from atl2dig.");
	Map.head.plani_zone = zone;
	Map.head.N = Nmaxx + ( Nmaxx - Smaxx ) * 0.05;
	Map.head.S = Smaxx - ( Nmaxx - Smaxx ) * 0.05;
	Map.head.E = Emaxx + ( Nmaxx - Smaxx ) * 0.05;
	Map.head.W = Wmaxx - ( Nmaxx - Smaxx ) * 0.05;
	Map.head.digit_thresh = 0.0;
	Map.head.map_thresh = 0.0;
	Vect_close(&Map);
	Vect_destroy_line_struct(Points);
	fprintf(jog,"%s END of Vimport.atlas2\n",argv[2]);
	fprintf(jog,"******************************\n");
	fclose(fp1);
	fclose(jog);
	return(0);
}
