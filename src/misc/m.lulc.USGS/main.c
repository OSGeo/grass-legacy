#include <stdio.h> 
#include <string.h>
#include <stdlib.h> 
#include <math.h>
#include <gis.h>

#define MAX_OVERLAYS	6
#define CTG_HEADER	"LULC Binary File"
#define COMPRESSION	1
#define HEADER_SIZE	(sizeof(struct Cell_head)+sizeof(char)*96+sizeof(int)*2)
#define HYDROLOGIC	3
#define CENSUS		2
#define POLITICAL	1
#define LULC		0
#define FEDERAL		4
#define STATE		5

int  format[] = {0,1,0,2,0,0};

char *prompt[MAX_OVERLAYS] = {"LAND USE/LAND COVER","POLITICAL","CENSUS",
	"HYDROLOGIC","FEDERAL LAND OWNERSHIP","STATE LAND OWNERSHIP"};
char buffer[255];

int Create_Grid_Cell(FILE *, int, char *, struct Cell_head *);
int set_status(int, int);
int get_overlay_name(char *, char *);

int main (int argc, char *argv[])
{
	int i, map_code, num;
	char header[25], map_name[80], name[80];
	FILE *f1;
	struct Cell_head window;
	struct Colors colors;
	CELL max;

	G_gisinit(argv[0]);

	if ((f1 = fopen(argv[1],"rb")) == NULL) {
		fprintf(stderr,"Cannot open USGS CTG file\n");
		exit(1);
	}

	fread((char *) header,sizeof(char),16,f1);
	if (strncmp(header,CTG_HEADER,strlen(CTG_HEADER)) != 0) {
		fprintf(stderr,"\n<%s> is not a valid LULC Binary File\n",argv[1]);
		fprintf(stderr,"Use Mlulc.extract to convert a USGS LULC file\n");
		fprintf(stderr,"to a LULC Binary File\n\n");
		fclose(f1);
		exit(0);
	}

	fread((char *) &num,sizeof(int),1,f1);	
	fread((char *) &map_code,sizeof(int),1,f1);
	fread((char *) map_name,sizeof(char),80,f1);
	fread((char *) &window,sizeof(struct Cell_head),1,f1);

	window.compressed = COMPRESSION;

	fprintf (stdout,"\n\nMAP TITLE: %s\n",map_name);
	fprintf (stdout,"The Composite Theme Grid file contains <%d> overlays\n",num);
	fprintf (stdout,"and has a map code type of <%d>\n",map_code);

	for (i = 0; i < MAX_OVERLAYS; i++) {
		if (set_status(i,map_code) == 0) continue;
		sprintf(buffer,"\nDo you Wish to Create <%s> Grid Cell",prompt[i]);
		if (G_yes(buffer,1) == 0) continue;
		if (get_overlay_name(prompt[i],name) == 0) continue;
		if ((max = Create_Grid_Cell(f1,i,name,&window)) == -1) continue;

		fprintf (stdout,"\nNumber of Categories: %d  (UNLABELED)\n",max);
		puts("Writing Cell Header Information");
		G_put_cellhd(name,&window);
		puts("Writing Color Table Information\n");
		G_make_random_colors(&colors,0,max);
		G_write_colors(name,G_mapset(),&colors);
		G_free_colors(&colors);
	}
	
	puts("\n\nConversion is Complete");
	fclose(f1);

	exit(0);
}


int Create_Grid_Cell (FILE *f1, int n, char *name, struct Cell_head *window)
{
	int j, k, fd, utm[2], value[MAX_OVERLAYS];
	double north;
	CELL *cells, max = 0;

	window->format = format[n];
	G_set_window(window);
	if ((fd = G_open_cell_new(name)) < 0) return(-1);

	fprintf (stdout,"Creating <%s> from <%s> Overlay:  ",name, prompt[n]);
	cells = G_allocate_cell_buf();

	north = window->north - window->ns_res/2.0;
	fseek(f1,(long)HEADER_SIZE,0);
	fread((char *)utm,sizeof(int),2,f1);
	fread((char *)value,sizeof(int),MAX_OVERLAYS,f1);
	for (j = 0; j < window->rows; j++) {
		G_percent(j,window->rows,5);
		G_zero_cell_buf(cells);
		while (north == utm[1] && !feof(f1))  {
			if (n == HYDROLOGIC) value[n] %= 100;
			k = (utm[0] - window->west)/window->ew_res;	
			cells[k] = (CELL) value[n];
			if (value[n] > max) max = value[n];
			fread((char *)utm,sizeof(int),2,f1);
			fread((char *)value,sizeof(int),MAX_OVERLAYS,f1);
		}	
		G_put_raster_row(fd, cells, CELL_TYPE);
		north -= window->ns_res;
	}
	G_close_cell(fd); 
	return(max);
}


int 
set_status (int value, int map_code)
{
	int i, flag = 0;

	switch (value) {
		case LULC:
			/* Map Code Type = 1 */	
			if (map_code%2) flag = -1;
			break;
		case POLITICAL:
			/* Map Code Type = 2 */	
			i = map_code % 10;
			if (i == 2 || i == 7 || !(i%3)) flag = -1; 
			break;
		case CENSUS:
			/* Map Code Type = 4 */
			i = map_code % 10;
			/*if (i >= 4) flag = -1;*/
			break;
		case HYDROLOGIC:
			/* Map Code Type = 10 */	
			i = map_code / 10;
			if (map_code%2) flag = -1; 
			break;
		case FEDERAL:
			/* Map Code Type = 20 */	
			i = map_code / 10;
			if (i == 2 || i == 7 || !(i%3)) flag = -1;
			break;
		case STATE:
			/* Map Code Type = 40 */	
			if (map_code >=	 40) flag = 0;
			break;
	}
	return(flag);
}


int 
get_overlay_name (char *prompt, char *name)
{
	char *mapset;
	
	sprintf(buffer,"Enter File Name for %s Overlay",prompt);
	mapset = G_ask_new(buffer,name, "cats","grid cell");

	if (mapset == NULL) return(0);
	return(-1);
}
