/* %A% Neueste Deltaversion %I% vom %G% um %U%*/
/*gezogen am %H% um %T% */

#include "gis.h"
#include "tools.h"


/* Grass Library Routinen */

extern int G_fatal_error(char*);
extern int G_get_cellhd(char*,char*,struct Cell_head*);
extern int G_set_window(struct Cell_head*);
extern int G_open_cell_old(char*,char*);
extern int G_get_map_row(int,CELL*,int);
extern int G_close_cell(int);
extern int G_read_range(char* ,char* ,struct Range*);


/* Folgende Variablen werden von der Routine Procag uebergeben:
	
	filename - Name des Rasterfiles, das in die Matrix eingelesen
		   werden soll
	mapset	 - Grass mapset in dem das File lokalisiert ist
	maxrow,
	maxcol		- Zeilen und Spaltenanzahl des Files

   Die Restlichen Variablen werden nur als Adresse uebergeben, da sie
   in dieser Routine ermittelt werden und an die Aufrufroutine zurueckgegeben
   werden

	matrix		- Array in das die Daten eingelesen werden
	offset		- Falls negative Werte auftauchen ist dies der 
			  betragsmaessig groesste negative Wert der zu
			  allen Klassenwerten addiert wird, damit keine
			  negativen Werte im Array stehen.   */


void read_data_ ( char* filename, char* mapset,
		  int* matrix, int* maxcol, int* maxrow, int* offset)
{
	char msg[100];
        struct Cell_head window;
    	CELL *buf;
   	int fd;
	int col;
	int row;
	int value;
	struct Range range;


/* Open raster file and allocate row buffer */

    	fd = G_open_cell_old (filename, mapset);
	if (fd < 0) TERMINATE("Can't open old cell file\n");
	
   	buf = G_allocate_cell_buf();

	G_read_range(filename,mapset,&range);	

        *offset = range.nmin * (-1);
        
	for (row = 1; row <= *maxrow; row++)
	{
		if (G_get_map_row (fd, buf, row-1) == -1 )
			TERMINATE("Could't read cell file\n");

		for (col = 1; col <= *maxcol; col++)
		{
      			value = buf[col-1] + *offset;

			matrix[col+row*(*maxcol+2)] = value;			
    		}
  	}
	free(buf);
	G_close_cell(fd);
}
