/*********************************************************************

NAME:		i.in.pri

FUNCTION:	ERS1.SAR.PRI file to GRASS conversion

INPUT FILE:	on disc (e.g. CD)

OUTPUT FILES:   raw band cell file

REMARKS:        derived from i.tape.gtc
                Olaf Hellwich, TUM, February 94
********************************************************************/

#include <string.h>
#include <stdlib.h>
#define NBANDS 1
#define GLOBAL
#include "pri.h"

typedef struct longRow {
   char head[4];
   short int sample[1];
} LongRow;
void ini_pos();

int 
main (int argc, char *argv[])
{
   int info;
   char *infonly;
   struct {
      struct Option *infonly, *voldirname, *leadername, *dataname, *output;
   } parm;
   int i;
   char *err, *G_adjust_Cell_head();
printf(" WARNING: This program accesses every pixel wrongly. DO NOT USE!!!\n");

    G_gisinit (argv[0]);

   parm.infonly = G_define_option();
   parm.infonly->key = "infonly";
   parm.infonly->type = TYPE_STRING;
   parm.infonly->required = NO;
   parm.infonly->description = "Data information only (y/n) [n]";
   parm.infonly->answer = "n";
   parm.infonly->options = "y,n,";

	parm.voldirname = G_define_option();
	parm.voldirname->key = "voldirname";
	parm.voldirname->type = TYPE_STRING;
	parm.voldirname->required = YES;
	parm.voldirname->description = "Volume directory filename";

	parm.leadername = G_define_option();
	parm.leadername->key = "leadername";
	parm.leadername->type = TYPE_STRING;
	parm.leadername->required = YES;
	parm.leadername->description = "Leader filename";

	parm.dataname = G_define_option();
	parm.dataname->key = "dataname";
	parm.dataname->type = TYPE_STRING;
	parm.dataname->required = YES;
	parm.dataname->description = "Data filename";

	parm.output = G_define_option();
	parm.output->key = "output";
	parm.output->type = TYPE_STRING;
	parm.output->required = YES;
	parm.output->description = "Name for resultant raster map";
	parm.output->gisprompt = "any,cell,raster";

	if (G_parser(argc,argv))
		exit(1);
	voldirname = parm.voldirname->answer;
	leadername = parm.leadername->answer;
	dataname = parm.dataname->answer;
	output = parm.output->answer;
        infonly = parm.infonly->answer;
   if ((infonly[0]=='y')||(infonly[0]=='Y')) {
      info = 1;
   } else {
      info = 0;
   }

/*    I_must_be_imagery_projection();*/
    G_want_histogram(1);

/*
   read tape format from tape
*/
   get_format();
   if (info) exit(0);
/*
   arrange cell header
*/
   for (i=0;i<nbands;i++) {
      cellhd[i] = (struct Cell_head *)G_malloc(sizeof(struct Cell_head));
      range[i] = (struct Range *)G_malloc(sizeof(struct Range));
      cellhd[i]->format = 1;
      cellhd[i]->compressed = 0;
      cellhd[i]->rows = numrow;
      cellhd[i]->cols = numdgprow;
      cellhd[i]->proj = 0;
      cellhd[i]->zone = 0;
      cellhd[i]->north = 0.5;
      cellhd[i]->south = -(double)(cellhd[i]->rows) + 0.5;
      cellhd[i]->east = (double)(cellhd[i]->cols) - 0.5;
      cellhd[i]->west = -0.5;

      if (err=G_adjust_Cell_head(cellhd[i],1,1)) {
         fprintf(stderr,"%s: ** errors detected ** %s\n",
          G_program_name(),err);
      }
   }
/*
   set window
*/
   if(G_set_window (cellhd[0]) < 0)
    exit(3);
   nrows = G_window_rows();

/*
   allocate cell buffer
*/
    tapebuf = (unsigned char *) G_malloc (tapebufsize);
    for (i=0;i<nbands;i++) cellbuf[i] = G_allocate_cell_buf();

    fprintf(stderr, "\n\n");

/*
   read pri data 
*/
   pri();

    free((char *)tapebuf);
    for (i=0;i<nbands;i++) {
       free((char *)range[i]);
       free((char *)cellhd[i]);
    }
    exit(0);
}

int 
get_format (void)
{
   int i,j,n,len;
   char buf[32767];
   char bu[25][120];
   char dummy1[80],dummy2[80];
   char data_type_code[5];
   char check_name[17];
   char refell[33];
   char utmdes[33];
   char utmzon[5];
   int numrec,numchan;
   long maxrange;
   long *pnum;
   char dftc[6],dfti[30];
   char leader_file_name[17];
   char scd[33];
   char sct[33];
   float lat,lon,dir;
   char orb[9];
   char pts[33];
   float a11,a12,a13,a14,a21,a22,a23,a24;
   float b11,b12,b13,b14,b21,b22,b23,b24;
   nbands = NBANDS;
   blocking_factor=1;
   tapebufsize = 0;
/*
   read from volume directory file
   fin = fopen(voldirname,"rb");
   ini_pos(fin);
   get_next_rec(fin,buf);
   get_next_rec(fin,buf);
   get_next_rec(fin,buf);
   sscanf(&(buf[20]), "%16s", data_file_name);
   sscanf(&(buf[96]), "%4s", data_type_code);
   if (strncmp (data_type_code,"MBAA",4)!=0) {
      perror("data set not in MIXED BINARY AND ASCII format");
      exit(1);
   }
   fclose(fin);
*/
/*
   read from leader file
*/
   fin = fopen(leadername,"r");
   fgets(bu[0],118,fin);
   fgets(bu[1],118,fin);
   fgets(bu[2],118,fin);
   fgets(bu[3],118,fin);
   fgets(bu[4],118,fin);
   fgets(bu[4],118,fin);
   fgets(bu[4],118,fin);
   fgets(bu[4],118,fin);
   fgets(bu[5],118,fin);
   fgets(bu[6],118,fin);
   fgets(bu[6],118,fin);
   fgets(bu[7],118,fin);
   fgets(bu[8],118,fin);
   fgets(bu[9],118,fin);
   fgets(bu[10],118,fin);
   fgets(bu[11],118,fin);
   fgets(bu[12],118,fin);
   fgets(bu[12],118,fin);
   fgets(bu[12],118,fin);
   fgets(bu[12],118,fin);
   fgets(bu[13],118,fin);
   fgets(bu[14],118,fin);
   fgets(bu[15],118,fin);
   fgets(bu[16],118,fin);
   fgets(bu[17],118,fin);
   fgets(bu[18],118,fin);
   fgets(bu[19],118,fin);
   fgets(bu[20],118,fin);
   fgets(bu[21],118,fin);
   fgets(bu[22],118,fin);
   fgets(bu[23],118,fin);
   fgets(bu[24],118,fin);
   for (i=0;i<25;i++) {
      printf("%s",bu[i]);
      len=strlen(bu[i]);
      bu[i][len-2]='\0';
   }
   fclose(fin);
   fin = fopen(leadername,"r");
   for (i=0;i<92;i++) fgets(buf,118,fin);
   fscanf(fin,"%s%s%4d",dummy1,dummy2,&numrow);
   fgets(buf,118,fin);
   fscanf(fin,"%s%s%4d",dummy1,dummy2,&numdgprow);
   reclen = 2*numdgprow+4;
   printf(" numrow: %d; numdgprow: %d; reclen: %d\n",numrow,numdgprow,reclen);
   fclose(fin);
/*
   read from data file
*/
   fin = fopen(dataname,"r");
   position = 8365L;
   fseek(fin,position,0);
/*
   generate history
    maximum 25 lines
*/
   for (i=0;i<nbands;i++) {
      for (j=0;j<25;j++) {
         sprintf(history[i].edhist[j],"%s",bu[j]);
      }
      history[i].edlinecnt = 25;
   }
   tapebufsize = reclen;

   return 0;
}


int 
pri (void)
{
    int band;
    int row;
    char name[4][17];
    struct Colors *colors;
    CELL min, max;

    for (band=0; band < nbands; band++)
    {
       strcpy(name[band],output);
       if (G_legal_filename(name[band])<0) {
          fprintf(stderr, "%s is not a legal filename\n",name[band]);
          exit(1);
       }
       bandfd[band] = G_open_cell_new_random (name[band]);
/*       bandfd[band] = G_open_cell_new (name[band]);*/
       if (bandfd[band]<0) {
          perror("error opening new raster file");
          exit(1);
       }
       G_short_history (name[band], "raster", &history[band]);
    }

    fprintf (stderr, "\nextracting ... "); fflush (stdout);
    for (row = 0; row < nrows; row+=blocking_factor)
    {
	G_percent (row, nrows, 2);
	if(!readpri (row)) goto done;
    }
    G_percent (row, nrows, 2);
done:
    fprintf (stderr, "\n");
    colors = (struct Colors *)G_malloc(sizeof(struct Colors));
    min = 0;
    max = 65535;
    fprintf(stdout,"\n");
    for (band=0; band < nbands; band++) {
       G_close_cell(bandfd[band]);
       G_write_history(name[band], &history[band]);
/*
    create color table
*/
       G_get_range_min_max (range[band], &min, &max);
       fprintf(stdout," band %d: minimum = %d, maximum = %d\n",band,min,max);
       G_make_grey_scale_colors (colors, min, max);
       G_write_colors (name[band], G_mapset(), colors);
    }
    free((char *)colors);

   return 0;
}


int 
readpri (int row)
{
    int n;
    register int j;
    unsigned char *m;
    long *rsn;
    static int too_short = 0;

    G_zero (tapebuf, tapebufsize);
    n = fread(tapebuf, 1, reclen, fin);
    rsn = (long *)tapebuf;
    recseqnum = *rsn;
/*    n = get_next_rec(fin,tapebuf);*/

    if (n < 0)
    {
	fprintf(stderr,"\n");
	fprintf(stderr,"ERROR bad tape read (row %d )\n",row);
	fprintf(stderr,"      raster files will be incomplete\n");
	return 0;
    }

    if (n == 0)
    {
	fprintf(stderr,"\n");
	fprintf(stderr,"ERROR only found %d rows on tape\n", firstrow+row);
	fprintf(stderr,"      raster files will be incomplete\n");
	return 0;
    }

    if (n < tapebufsize)
    {
	if (!too_short)
	{
	    too_short = 1;
	    fprintf(stderr,"\n");
	    fprintf (stderr, "WARNING: data records shorter than expected\n");
	    fprintf (stderr, "        only found %d bytes\n", n);
	}
    }
    put_row (row, tapebuf);
    return 1;
}


int 
put_row (int row, unsigned char *buf)
{
   LongRow *Row;
   int ncols;
   int i,j;
   long x;
   Row = (LongRow *)buf;
   ncols = G_window_cols();
   for (i=0;i<nbands;i++) {
      for (j=0;j<ncols;j++) {
         x = (long)Row->sample[j];
         cellbuf[i][j] = x;
      }
      G_put_map_row_random (bandfd[i], cellbuf[i], nrows-row-1, 0, ncols);
/*      G_put_map_row (bandfd[i], cellbuf[i]);*/
      G_row_update_range (cellbuf[i],ncols,range[i]);
   }

   return 0;
}


void 
ini_pos (FILE *f)
{
   position=0;
   fseek(f,0L,0);
}
