/*********************************************************************

NAME:		i.in.gtc

FUNCTION:	ERS1.SAR.GTC file to GRASS conversion

INPUT FILE:	on disc (e.g. CD)

OUTPUT FILES:   raw band cell file

REMARKS:        derived from i.tape.slc
                Olaf Hellwich, TUM, November 93
********************************************************************/

#define NBANDS 1
#define GLOBAL
#include <string.h>
#include <stdlib.h>
#include "gtc.h"

typedef struct longRow {
   unsigned long int num;
   char frstc;
   char rtc;
   char srstc;
   char trstc;
   unsigned long int length;
   short int sample[1];
} LongRow;
long get_next_rec();
long get_reclen();
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
      cellhd[i]->proj = 1;
      cellhd[i]->zone = zone;
      if (tln<bln) {
/* this is necessary, because some gecs contain values in the wrong place */
         cellhd[i]->north = (double)bln;
         cellhd[i]->south = (double)tln;
      } else {
         cellhd[i]->north = (double)tln;
         cellhd[i]->south = (double)bln;
      }
      if (tre<tle) {
         cellhd[i]->east = (double)tle;
         cellhd[i]->west = (double)tre;
      } else {
         cellhd[i]->east = (double)tre;
         cellhd[i]->west = (double)tle;
      }

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
   read gtc data 
*/
   gtc();

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
   int i,n;
   char buf[32767];
   char data_type_code[5];
   char check_name[17];
   char refell[33];
   char utmdes[33];
   char utmzon[5];
   int numrec,numchan;
   long maxrange;
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
*/
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
/*
   read from leader file
*/
   fin = fopen(leadername,"r");
   ini_pos(fin);
   n = get_next_rec(fin,buf);
   strncpy (leader_file_name,&(buf[48]),16);
   n = get_next_rec(fin,buf);
   strncpy (scd,&(buf[36]),32);
   strncpy (sct,&(buf[68]),32);
   sscanf (&(buf[117]), "%15f", &lat);
   sscanf (&(buf[133]), "%15f", &lon);
   sscanf (&(buf[149]), "%15f", &dir);
   strncpy (orb,&(buf[444]),8);
   strncpy (pts,&(buf[1110]),32);
   n = get_next_rec(fin,buf);
   strncpy (refell,&(buf[236]),32);
   strncpy (utmdes,&(buf[460]),32);
   strncpy (utmzon,&(buf[492]),4);
   sscanf (&(buf[494]), "%2d", &zone);
   sscanf (&(buf[961]), "%15f", &tln);
   sscanf (&(buf[977]), "%15f", &tle);
   sscanf (&(buf[993]), "%15f", &trn);
   sscanf (&(buf[1009]), "%15f", &tre);
   sscanf (&(buf[1025]), "%15f", &brn);
   sscanf (&(buf[1041]), "%15f", &bre);
   sscanf (&(buf[1057]), "%15f", &bln);
   sscanf (&(buf[1073]), "%15f", &ble);
   sscanf (&(buf[1280]), "%20e", &a11);
   sscanf (&(buf[1300]), "%20e", &a12);
   sscanf (&(buf[1320]), "%20e", &a13);
   sscanf (&(buf[1340]), "%20e", &a14);
   sscanf (&(buf[1360]), "%20e", &a21);
   sscanf (&(buf[1380]), "%20e", &a22);
   sscanf (&(buf[1400]), "%20e", &a23);
   sscanf (&(buf[1420]), "%20e", &a24);
   sscanf (&(buf[1440]), "%20e", &b11);
   sscanf (&(buf[1460]), "%20e", &b12);
   sscanf (&(buf[1480]), "%20e", &b13);
   sscanf (&(buf[1500]), "%20e", &b14);
   sscanf (&(buf[1520]), "%20e", &b21);
   sscanf (&(buf[1540]), "%20e", &b22);
   sscanf (&(buf[1560]), "%20e", &b23);
   sscanf (&(buf[1580]), "%20e", &b24);
   fclose(fin);
/*
   read from data file
*/
   fin = fopen(dataname,"r");
   ini_pos(fin);
   headlen = get_next_rec(fin,buf);
   sscanf(&(buf[48]), "%16s", check_name);
   if (strncmp (check_name,data_file_name,16)!=0) {
      printf("contradiction in data filename: %s %s\n",
       check_name,data_file_name);
   }
   sscanf (&(buf[180]), "%6d", &numrec);
   sscanf (&(buf[186]), "%6d", &reclen);
   sscanf (&(buf[224]), "%4d", &bpdg);
   sscanf (&(buf[232]), "%4d", &numchan);
   sscanf (&(buf[236]), "%8d", &numrow);
   sscanf (&(buf[248]), "%8d", &numdgprow);
   strncpy (dfti,&(buf[408]),28);
   strncpy (dftc,&(buf[436]),4);
   sscanf (&(buf[448]), "%8ld", &maxrange);
/*
   output information
*/
   fprintf(stdout,"  DATA SET INFORMATION\n");
   fprintf(stdout," data file name:                      \"%s\"\n",
    data_file_name);
   fprintf(stdout," data type code:                      \"%s\"\n",
    data_type_code);
/*   fprintf(stdout," leader file name:                    \"%s\"\n",
    leader_file_name);*/
   fprintf(stdout," product type id.: \"%s\"\n",pts);
   fprintf(stdout," orbit: \"%s\"  frame: \"%s\"\n",orb,scd);
   fprintf(stdout," scene centre time:   \"%s\"\n", sct);
   fprintf(stdout," scene centre geodetic latitude:  %16.7f\n",lat);
   fprintf(stdout," scene centre geodetic longitude: %16.7f\n",lon);
   fprintf(stdout," scene centre true heading:       %16.7f\n",dir);
   fprintf(stdout," UTM descriptor:      \"%s\"\n", utmdes);
   fprintf(stdout," UTM zone:            \"%s\"\n", utmzon);
   fprintf(stdout," reference ellipsoid: \"%s\"\n", refell);
   fprintf(stdout," top left:     %16.7f %16.7f\n",tln,tle);
   fprintf(stdout," top right:    %16.7f %16.7f\n",trn,tre);
   fprintf(stdout," bottom right: %16.7f %16.7f\n",brn,bre);
   fprintf(stdout," bottom left:  %16.7f %16.7f\n",bln,ble);
   fprintf(stdout," a11, a12: %e %e\n",a11,a12);
   fprintf(stdout," a13, a14: %e %e\n",a13,a14);
   fprintf(stdout," a21, a22: %e %e\n",a21,a22);
   fprintf(stdout," a23, a24: %e %e\n",a23,a24);
   fprintf(stdout," b11, b12: %e %e\n",b11,b12);
   fprintf(stdout," b13, b14: %e %e\n",b13,b14);
   fprintf(stdout," b21, b22: %e %e\n",b21,b22);
   fprintf(stdout," b23, b24: %e %e\n",b23,b24);
   fprintf(stdout," length of header record:             %6ld\n",headlen);
   fprintf(stdout," number of bytes per data group:        %4d\n",bpdg);
   fprintf(stdout," number of SAR DATA records:          %6d\n",numrec);
   fprintf(stdout," SAR DATA record length:              %6d\n",reclen);
   fprintf(stdout," number of SAR channels:                %4d\n",numchan);
   fprintf(stdout," number of lines per data set (min) %8d\n",numrow);
   fprintf(stdout," number of data groups/row/channel  %8d\n",numdgprow);
   fprintf(stdout," SAR data format type identifier      \"%s\"\n",dfti);
   fprintf(stdout," SAR data format type code            \"%s\"\n",dftc);
   fprintf(stdout," maximum data range of pixel        %8ld\n",maxrange);
   if ((bpdg * numdgprow + 12)!=reclen) {
      printf("contradictory record length (required, actual): %d %d\n",
       bpdg*numdgprow+12,reclen);
      offset = (reclen - (bpdg*numdgprow+12))/2;
      printf("offset of %d pixels will be used!\n", offset);
   }
/*
   generate history
    maximum 25 lines
*/
   for (i=0;i<nbands;i++) {
      sprintf(history[i].edhist[0],
       " data file name:                      \"%s\"", data_file_name);
      sprintf(history[i].edhist[1],
       " data type code:                      \"%s\"", data_type_code);
      sprintf(history[i].edhist[2],
       " product type id.: \"%s\"",pts);
      sprintf(history[i].edhist[3],
       " orbit: \"%s\"  frame: \"%s\"",orb,scd);
      sprintf(history[i].edhist[4],
       " scene centre time:   \"%s\"", sct);
      sprintf(history[i].edhist[5],
       " scene centre geodetic latitude:  %16.7f", lat);
      sprintf(history[i].edhist[6],
       " scene centre geodetic longitude: %16.7f", lon);
      sprintf(history[i].edhist[7],
       " UTM descriptor:      \"%s\"", utmdes);
      sprintf(history[i].edhist[8],
       " UTM zone:            \"%s\"", utmzon);
      sprintf(history[i].edhist[9],
       " reference ellipsoid: \"%s\"", refell);
      sprintf(history[i].edhist[10],
       " top left:     %16.7f %16.7f",tln,tle);
      sprintf(history[i].edhist[11],
       " top right:    %16.7f %16.7f",trn,tre);
      sprintf(history[i].edhist[12],
       " bottom right: %16.7f %16.7f",brn,bre);
      sprintf(history[i].edhist[13],
       " bottom left:  %16.7f %16.7f",bln,ble);
      sprintf(history[i].edhist[14],
       " length of header record:             %6ld", headlen);
      sprintf(history[i].edhist[15],
       " number of bytes per data group:        %4d",bpdg);
      sprintf(history[i].edhist[16],
       " number of SAR DATA records:          %6d",numrec);
      sprintf(history[i].edhist[17],
       " SAR DATA record length:              %6d",reclen);
      sprintf(history[i].edhist[18],
       " number of lines per data set (min) %8d",numrow);
      sprintf(history[i].edhist[19],
       " number of data groups/row/channel  %8d",numdgprow);
      sprintf(history[i].edhist[20],
       " SAR data format type code            \"%s\"",dftc);
      sprintf(history[i].edhist[21],
       " maximum data range of pixel        %8ld",maxrange);
      history[i].edlinecnt = 22;
   }
   tapebufsize = headlen;

   return 0;
}


int 
gtc (void)
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
/*       bandfd[band] = G_open_cell_new_random (name[band]);*/
       bandfd[band] = G_open_cell_new (name[band]);
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
	if(!readgtc (row)) goto done;
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
readgtc (int row)
{
    int n;
    register int j;
    unsigned char *m;
    long *rsn;
    static int too_short = 0;

    G_zero (tapebuf, tapebufsize);
    n = get_next_rec(fin,tapebuf);
/*    n = fread(tapebuf, 1, reclen, fin);*/
    rsn = (long *)tapebuf;
    recseqnum = *rsn;

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
/*   printf("num%d f%d type%d s%d t%d length%d\n",Row->num,(int)Row->frstc,
    (int)Row->rtc,(int)Row->srstc,(int)Row->trstc,Row->length);*/
   for (i=0;i<nbands;i++) {
      for (j=0;j<ncols;j++) {
         x = (long)Row->sample[j+offset];
         cellbuf[i][j] = x;
/*         if ((i==0)&&(j<20)) printf("%d ",x);*/
      }
/*      G_put_map_row_random (bandfd[i], cellbuf[i], nrows-row-1, 0, ncols);*/
      G_put_map_row (bandfd[i], cellbuf[i]);
      G_row_update_range (cellbuf[i],ncols,range[i]);
   }
/*   printf("\n");*/

    return 0;
}


long 
get_reclen (FILE *f)
{
   char buf[22];
   long *plen;
   long len;
   fread(buf,20,1,f);
   plen = (long *)(&(buf[8]));
   len = *plen;
   fseek(f,position,0);
   return len;
}


long 
get_next_rec (FILE *f, char *buf)
{
   long len;
   len = get_reclen(f);
   fread(buf,len,1,f);
   position+=len;
   return len;
}


void 
ini_pos (FILE *f)
{
   position=0;
   fseek(f,0L,0);
}
