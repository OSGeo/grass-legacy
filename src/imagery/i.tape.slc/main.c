/*********************************************************************

NAME:		i.tape.slc

FUNCTION:	ERS1.SAR.SLC CEOS file tape extraction

INPUT FILE:	tape drive with rewind upon close

OUTPUT FILES:   raw band cell files

REMARKS:        derived from i.tape.other
                Olaf Hellwich, TUM, July 93
                Technische Universitaet Muenchen, Germany
********************************************************************/

#include <string.h>
#include <unistd.h>
#define NBANDS 3
#define GLOBAL
#include "tape.h"

typedef union sample {
   long i;
   struct {
      short int r;
      short int i;
   } c;
} Sample;
typedef struct complexRow {
   char head[8];
   unsigned long int length;
   Sample sample[2500];
} ComplexRow;

int 
main (int argc, char *argv[])
{
   int info;
   char *infonly;
   struct {
      struct Option *infonly;
   } parm;

    char tapename[20];
   int i;
   char *err, *G_adjust_Cell_head();

    G_gisinit (argv[0]);

   parm.infonly = G_define_option();
   parm.infonly->key = "infonly";
   parm.infonly->type = TYPE_STRING;
   parm.infonly->required = NO;
   parm.infonly->description = "Tape information only (y/n) [n]";
   parm.infonly->answer = "n";
   parm.infonly->options = "y,n,";
   if (G_parser(argc,argv)) exit(1);
   infonly = parm.infonly->answer;
   if ((infonly[0]=='y')||(infonly[0]=='Y')) {
      info = 1;
   } else {
      info = 0;
   }

    I_must_be_imagery_projection();
    G_want_histogram(1);

/* mount the tape */

    get_tapename(tapename) ;
    I_ask("Please mount and load tape, then hit RETURN-->", 0, 1) ;
    tapefd = mount_tape (tapename);

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
      if (i<2) {
         cellhd[i]->format = 1;
      } else {
         cellhd[i]->format = 0;
      }
      cellhd[i]->compressed = 0;
      cellhd[i]->rows = numrow;
      cellhd[i]->cols = (headlen-12)/4;
      cellhd[i]->proj = 0;
      cellhd[i]->zone = 0;
      cellhd[i]->north = 0.5;
      cellhd[i]->south = -(double)(cellhd[i]->rows/5) + 0.5;
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
   read slc data 
*/
   slc();

    close (tapefd);

    G_free((char *)tapebuf);
    for (i=0;i<nbands;i++) {
       G_free((char *)range[i]);
       G_free((char *)cellhd[i]);
    }
    exit(0);
}

int 
get_format (void)
{
   int i;
   char buf[32767];
   char data_type_code[5];
   int numrec,reclen,numchan;
   long maxrange;
   char dftc[6],dfti[30];
   char leader_file_name[17];
   char sct[33];
   float lat,lon,dir;
   skipfiles = 2;
   skiprecords = 1;
   nbands = NBANDS;
   bytes_per_pixel = 4;
   blocking_factor=1;
   tapebufsize = 0;
/*
   read parameters from tape
*/
   tape_advance (tapefd, 2);
   read (tapefd, buf, sizeof(buf));
   sscanf(&(buf[20]), "%16s", data_file_name);
   sscanf(&(buf[96]), "%4s", data_type_code);
   if (strncmp (data_type_code,"MBAA",4)!=0) {
      perror("data set not in MIXED BINARY AND ASCII format");
      exit(1);
   }
   tape_advance (tapefd, -1);
/*
   read from leader file
*/
   read (tapefd, buf, sizeof(buf));
   strncpy (leader_file_name,&(buf[48]),16);
   read (tapefd, buf, sizeof(buf));
   strncpy (sct,&(buf[68]),32);
   sscanf (&(buf[116]), "%16f", &lat);
   sscanf (&(buf[132]), "%16f", &lon);
   sscanf (&(buf[148]), "%16f", &dir);
   tape_advance (tapefd, -1);
/*
   read from data file
*/
   read (tapefd, buf, sizeof(buf));
   if (strncmp (&(buf[48]),data_file_name,16)!=0) {
      perror("contradiction in filename of data set");
      exit(1);
   }
   headlen = *((long *)(&(buf[8])));
   sscanf (&(buf[180]), "%6d", &numrec);
   sscanf (&(buf[186]), "%6d", &reclen);
   sscanf (&(buf[224]), "%4d", &bpdg);
   sscanf (&(buf[232]), "%4d", &numchan);
   sscanf (&(buf[236]), "%8d", &numrow);
   sscanf (&(buf[248]), "%8d", &numdgprow);
   strncpy (dfti,&(buf[400]),28);
   strncpy (dftc,&(buf[428]),4);
   sscanf (&(buf[440]), "%8ld", &maxrange);
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
   fprintf(stdout," scene centre time:   \"%s\"\n", sct);
   fprintf(stdout," scene centre geodetic latitude:  %16.7f\n",lat);
   fprintf(stdout," scene centre geodetic longitude: %16.7f\n",lon);
/*   fprintf(stdout," scene centre true heading:       %16.7f\n",dir);*/
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
      perror("contradictionary record length");
      exit(1);
   }
/*
   generate history
*/
   for (i=0;i<nbands;i++) {
      sprintf(history[i].edhist[0],
       "  DATA SET INFORMATION\n");
      sprintf(history[i].edhist[1],
       " data file name:                      \"%s\"\n", data_file_name);
      sprintf(history[i].edhist[2],
       " data type code:                      \"%s\"\n", data_type_code);
      sprintf(history[i].edhist[3],
       " scene centre time:   \"%s\"\n", sct);
      sprintf(history[i].edhist[4],
       " scene centre geodetic latitude:  %16.7f\n", lat);
      sprintf(history[i].edhist[5],
       " scene centre geodetic longitude: %16.7f\n", lon);
      sprintf(history[i].edhist[6],
       " length of header record:             %6ld\n", headlen);
      sprintf(history[i].edhist[7],
       " number of bytes per data group:        %4d\n",bpdg);
      sprintf(history[i].edhist[8],
       " number of SAR DATA records:          %6d\n",numrec);
      sprintf(history[i].edhist[9],
       " SAR DATA record length:              %6d\n",reclen);
      sprintf(history[i].edhist[10],
       " number of SAR channels:                %4d\n",numchan);
      sprintf(history[i].edhist[11],
       " number of lines per data set (min) %8d\n",numrow);
      sprintf(history[i].edhist[12],
       " number of data groups/row/channel  %8d\n",numdgprow);
      sprintf(history[i].edhist[13],
       " SAR data format type identifier      \"%s\"\n",dfti);
      sprintf(history[i].edhist[14],
       " SAR data format type code            \"%s\"\n",dftc);
      sprintf(history[i].edhist[15],
       " maximum data range of pixel        %8ld\n",maxrange);
      history[i].edlinecnt = 16;
   }
   tapebufsize = headlen;

   return 0;
}


int 
slc (void)
{
    int band;
    int row;
    char name[4][17];
    struct Colors *colors;
    CELL min, max;

    for (band=0; band < nbands; band++)
    {
       strcpy(name[band],data_file_name);
       if (band==0) strncpy(&(name[band][12]),"RE16",4);
       if (band==1) strncpy(&(name[band][12]),"IM16",4);
/*
       if (band==2) strncpy(&(name[band][12]),"RE08",4);
*/
       if (band==2) strncpy(&(name[band][12]),"INTE",4);
       if (band==3) strncpy(&(name[band][12]),"IM08",4);
       if (G_legal_filename(name[band])<0) {
          fprintf(stderr, "%s is not a legal filename\n",name[band]);
          exit(1);
       }
       bandfd[band] = G_open_cell_new_random (name[band]);
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
	if(!readslc (row)) goto done;
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
    G_free((char *)colors);

   return 0;
}


int 
readslc (int row)
{
    int n;
    register int j;
    unsigned char *m;
    static int too_short = 0;

    G_zero (tapebuf, tapebufsize);
    n = read (tapefd, tapebuf, tapebufsize);

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
	    fprintf (stderr, "        only found %d bytes on the tape\n", n);
	}
    }
    put_row (row, tapebuf);
    return 1;
}


int 
put_row (int row, unsigned char *buf)
{
   ComplexRow *Row;
   double ampli,real,imag,resu;
   int ncols;
   int i,j;
   unsigned short int *val;
   unsigned short int value;
   long x;
   unsigned short int y;
   Row = (ComplexRow *)buf;
   ncols = G_window_cols();
/*
   separate real from imaginary parts
*/
   if (Row->length!=bpdg*numdgprow+12) {
      perror ("wrong record length");
      exit(1);
   }
   if (bpdg*numdgprow+12!=sizeof(ComplexRow)){
      perror ("wrong length of data structure");
      exit(1);
   }
   for (i=0;i<nbands;i++) {
      for (j=0;j<ncols;j++) {
         if (i==0) {
            cellbuf[i][j] = (long)Row->sample[j].c.r;
         } else if (i==1) {
            cellbuf[i][j] = (long)Row->sample[j].c.i;
         } else if (i==2) {
            real = (double)((float)Row->sample[j].c.r);
            imag = (double)((float)Row->sample[j].c.i);
            resu = pow(real,2.)+pow(imag,2.);
            ampli = sqrt(resu);
            cellbuf[i][j] = (CELL)((long)(ampli+0.5));
         }
/*
         if (i==0) {
            y = *((unsigned short int *)(&(buf[12+4*j])));
            x = (long)y;
            cellbuf[i][j] = (CELL)x;
         } else if (i==1) {
            y = *((unsigned short int *)(&(buf[14+4*j])));
            x = (long)y;
            cellbuf[i][j] = (CELL)x;
         } else if (i==2) {
            y = *((unsigned short int *)(&(buf[12+4*j])));
            x = (long)y;
            cellbuf[i][j] = (CELL)(x/256);
         } else if (i==3) {
            y = *((unsigned short int *)(&(buf[14+4*j])));
            x = (long)y;
            cellbuf[i][j] = (CELL)(x/256);
         }
*/
      }
      G_put_map_row_random (bandfd[i], cellbuf[i], nrows-row-1, 0, ncols);
      G_row_update_range (cellbuf[i],ncols,range[i]);
   }

   return 0;
}
