/***************************************************************************
Read the DSI-Header
****************************************************************************/
#include "dev.h"
#include "gis.h"

int dsiread (infile, eindat, dsipo)

char	eindat[];
struct header *dsipo;
FILE *infile;

{
	FILE * dsifile;
	register int	ii, jj, j, kk, ret;
	int	xxres, yyres;
	int	grad, mmin, sek;
	float	start;
	char	dsi[DSIHEAD], str[10], str1[10], str2[10];
	char	lat[10], lon[11], vd[4], hd[5], cd[5];
	char	cc;
	char	xres[5], yres[5], col[5], row[5], help[MAXLENGTH];
	char	*po;
	char	ssgrad[3], sgrad[2], smin[2], ssek[2];
	char    *pos;
	struct Cell_head region;
	
	static char	*text[] = {
		"Latitude / Longitude of SW corner of data:",
		"Latitude / Longitude of NW corner of data:",
		"Latitude / Longitude of NE corner of data:",
		"Latitude / Longitude of SE corner of data:"
	};

	printf ("\nReading Data Set Identification record of %s\n\n", eindat);
	printf ("Creating output file %s.dsi\n\n", eindat);

	strcpy(help, eindat);
	strcat(help, ".dsi");

    
        /* Neue Datensaetze (beginnend mit UHL) haben einen anderen Offset
           fuer den Header. Deswegen erstmal 256 Bytes einlesen und nach dem
           Magic DSI suchen. Dann hier den Headeranfang setzen.
           -emes- 19/9/94
        */  

	if(!fread(dsi,1,256,infile)){
	   fprintf(stderr,"Error reading header. Terminating!\n");
	   exit(-1);
	}
        
        if(!(pos = strstr(dsi,"DSI"))){
           fprintf(stderr,"Bad Magic. Terminating!\n");
           exit(-1);
        }	
	
	fprintf(stdout,"Found magic at pos %d.\n",pos-dsi);
        fseek(infile,pos-dsi,0);

        if(fread(dsi,1,DSIHEAD,infile) != DSIHEAD) {
           fprintf(stderr,"Error reading header. Terminating!\n");
           exit(-1);
        }        
        

	G_get_set_window(&region);
	
	if ((dsifile = fopen(help, "w+")) != NULL) {
		fprintf (dsifile, "Geographic and cartographic information about the elevation data set: %s\n\n", eindat);

		/*Read annotation information from DSI-Header*/
		po = &dsi[141];
		for (ii = 0; ii < 3; ii++)
			vd[ii] = *(po + ii);
		vd[ii]=0;
		fprintf (dsifile, "Vertical date: %s\n", vd);
		po += ii;

		for (ii = 0; ii < 5; ii++)
			hd[ii] = *(po + ii);
		hd[ii]=0;
		fprintf (dsifile, "Horizontal datum code: %s\n", hd);
		po += ii + 10;

		for (ii = 0; ii < 4; ii++)
			cd[ii] = *(po + ii);
		cd[ii]=0;
		fprintf (dsifile, "Compilation date (year/month): %s\n", cd);

		/*Read the geographic coordinates from the DSI-Header*/
		po = &dsi[185];

		for (ii = 0; ii < 8; ii++)
			str[ii] = *(po + ii);
		str[ii]=0;
		fprintf (dsifile, "\nLatitude of origin (lower left) of data: %s\n", str);
		(((cc = *(po + (ii))) == 'N') ? fprintf(dsifile,
		    "North hemisphere\n") : fprintf(dsifile, "South hemisphere\n"));

		for (ii = 0, jj = 2, kk = 4; ii < 2; ii++, jj++, kk++)  {
			sgrad[ii] = str[ii];
			smin[ii] = str[jj];
			ssek[ii] = str[kk];   
		}
		grad = atoint(sgrad);
		mmin = atoint(smin);
		sek = atoint(ssek);
		start = grad + (mmin / 60.) + (sek / 3600.);
		dsipo ->xstart = (((int)start) + 0.5 < start ? (int)start + 1 : (int)start);


		po = &dsi[194];
		for (ii = 0; ii < 9; ii++)
			str1[ii] = *(po + ii);
		str1[ii]=0;
		fprintf (dsifile, "Longitude of origin (lower left) of data: %s\n", str1);
		(((cc = *(po + (ii))) == 'E') ? fprintf (dsifile, "East of Greenwich\n\n") : fprintf (dsifile, "West of Greenwich\n\n"));

		for (ii = 0; ii < 3; ii++)
			ssgrad[ii] = str1[ii];
		for (ii = 0, jj = 3, kk = 5; ii < 2; ii++, jj++, kk++)  {
			smin[ii] = str1[jj];
			ssek[ii] = str1[kk];		  
		}

		grad = atoint(ssgrad);
		mmin = atoint(smin);
		sek = atoint(ssek);
		start = grad + (mmin / 60.) + (sek / 3600.);
		dsipo ->ystart = (((int)start) + 0.5 < start ? (int)start + 1 : (int)start);


		/* read the geographic coordinates of the corners of the milgeo-file */
		po = &dsi[204];
		jj = 0;

		fprintf (dsifile, "Corner coordinates:\n");
		fprintf (dsifile, "Latitude in gradgradminminsecsec\n");
		fprintf (dsifile, "Longitude in gradgradgradminminsecsec\n\n");
		do {
			j = 0;
			for (ii = 0; ii < 7; ii++){
			   if(ii == 2 || ii == 4){
			     lat[ii+j] = ':';
			     j++;
			   }
			   lat[ii+j] = *(po + ii);
			}
			lat[ii+j]=0;
			
			po += ii;
			j=0;
			for (ii = 0; ii < 8; ii++){
			   if(ii == 3 || ii == 5){
			     lon[ii+j] = ':';
			     j++;
			   }
			   lon[ii+j] = *(po + ii);
			}
			po += ii;
			lon[ii+j]=0;
			
			fprintf (dsifile, "%s %s %s\n", text[jj], lat, lon);
			if(jj == 0){
			   G_scan_northing(lat,&region.south,PROJECTION_LL);
			   G_scan_easting(lon,&region.west,PROJECTION_LL);
			} else
			if(jj == 2){
			   G_scan_northing(lat,&region.north,PROJECTION_LL);
			   G_scan_easting(lon,&region.east,PROJECTION_LL);
			}
			
			jj += 1;
		} while (jj < 4);

		/* read the geographic coordinates of the milgeo-file */
		po = &dsi[219];

		for (ii = 0 ; ii < 7 ; ii++)
			str1[ii] = *(po + ii);
		po += ii;

		/*convert the string to floating point */
		for (ii = 0, jj = 2, kk = 4; ii < 2; ii++, jj++, kk++)  {
			sgrad[ii] = str1[ii];
			smin[ii] = str1[jj];
			ssek[ii] = str1[kk];   
		}
		grad = atoint(sgrad);
		mmin = atoint(smin);
		sek = atoint(ssek);
		dsipo ->xmap = grad + (mmin / 60.) + (sek / 3600.);

		for (ii = 0 ; ii < 8 ; ii++)
			str2[ii] = *(po + ii);
		po += ii;

		/* convert the string to floating point */
		for (ii = 0; ii < 3; ii++)
			ssgrad[ii] = str2[ii];
		for (ii = 0, jj = 3, kk = 5; ii < 2; ii++, jj++, kk++)  {
			smin[ii] = str2[jj];
			ssek[ii] = str2[kk];		  
		}
		grad = atoint(ssgrad);
		mmin = atoint(smin);
		sek = atoint(ssek);
		dsipo ->ymap = grad + (mmin / 60.) + (sek / 3600.);

		fprintf (dsifile, "\nSpatial resolution of the data set:\n");
		po = &dsi[273];
		for (ii = 0; ii < 4; ii++)
			xres[ii] = *(po + ii);
		xres[ii]=0;
		fprintf (dsifile, "Latitude interval in tenth of seconds between rows of elevation values: %s\n", xres);
		po += ii;
		xxres = atoint(xres);
		dsipo ->xcell = 111000. / (360. * xxres);


		for (ii = 0; ii < 4; ii++)
			yres[ii] = *(po + ii);
		yres[ii]=0;
		fprintf (dsifile, "Longitude interval in tenth of seconds between columns of elevation values: %s\n", yres);
		po += ii;
		yyres = atoint(yres);
		dsipo ->ycell = 111000. / (360. * yyres);

		fprintf (dsifile, "\nSize of the data set:\n");
		for (ii = 0; ii < 4; ii++)
			col[ii] = *(po + ii);
		col[ii]=0;
		fprintf (dsifile, "Number of latitude lines of input data = columns: %s\n", col);
		po += ii;
		dsipo ->col = atoint(col);

		for (ii = 0; ii < 4; ii++)
			row[ii] = *(po + ii);
		row[ii]=0;
		fprintf (dsifile, "Number of longitude lines of input data = rows: %s\n", row);

		dsipo ->row = atoint(row);
		region.rows = xxres = dsipo->row;
		region.cols = yyres = dsipo->col;
	
	
	}/*end of if*/ 
	    else
		err ("ERROR opening DSI-Header file");

	fclose (dsifile);
	
	ret = G_set_window(&region);
	return ret;
}


