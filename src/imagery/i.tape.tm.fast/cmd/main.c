/************************************************/
/* NAME:	i.tape.tm.fast			*/
/*						*/
/* FUNCTION:	extract a window out of a set 	*/
/*		of TM tape			*/
/* 						*/
/* USAGE:	i.tape.tm.fast			*/
/*						*/
/* INPUT FILE:	tape files			*/
/*						*/
/* OUTPUT FILE:	raw band cell and support files */
/************************************************/
#define GLOBAL
#include "tape.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int main (int argc, char *argv[])
{
  int i, band_no;
  int firstrow, lastrow, firstcol, lastcol;
  char *inf, *outf, *version, bands[10];
  struct GModule *module;
  struct
  {
        struct Option *input, *group, *win_rows, *win_cols, 
	  *bands_wanted, *title, *header_version;
  } parm;

  struct {
      struct Flag *q;
  } flag;

/* initialize local variables */
  firstrow = lastrow = firstcol = lastcol = 0;

  G_gisinit(argv[0]);

  module = G_define_module();
  module->description =
	"An imagery function that extracts Thematic "
	"Mapper (TM) imagery from tape media.";

  parm.input = G_define_option();
  parm.input->key         ="input";
  parm.input->type        =TYPE_STRING;
  parm.input->required    =YES;
  parm.input->description ="Device name of tape drive";

  parm.group = G_define_option();
  parm.group->key        ="group";
  parm.group->type       =TYPE_STRING;
  parm.group->required   =YES;
  parm.group->description="Group name of the output group raster file";
  
  parm.bands_wanted = G_define_option();
  parm.bands_wanted->key = "bands";
  parm.bands_wanted->type = TYPE_INTEGER;
  parm.bands_wanted->required = YES;
  sprintf (bands, "1-%d", THEMATIC_MAPPER_NBANDS);
  parm.bands_wanted->options = bands;
  parm.bands_wanted->description = "Bands to be extracted";

  parm.win_rows = G_define_option();
  parm.win_rows->key  ="rows";
  parm.win_rows->key_desc  ="firstrow-lastrow";
  parm.win_rows->type =TYPE_INTEGER;
  parm.win_rows->required     =NO;
  parm.win_rows->description=
	"Rows to be extracted, firstrow-lastrow; Default: Full imagery";

  parm.win_cols = G_define_option();
  parm.win_cols->key ="cols";
  parm.win_cols->key_desc  ="firstcol-lastcol";
  parm.win_cols->type =TYPE_INTEGER;
  parm.win_cols->required     =NO;
  parm.win_cols->description=
	"Columns to be extracted, firstcol-lastcol; Default: Full imagery";
  
  /*
  parm.header_version = G_define_option();
  parm.header_version->key = "version";
  parm.header_version->type = TYPE_STRING;
  parm.header_version->required = NO;
  parm.header_version->options = "a,b";
  parm.header_version->description="Imagery header version, a or b";
  parm.header_version->answer="a";
  */
  
  parm.title = G_define_option();
  parm.title->key = "title";
  parm.title->type = TYPE_STRING;
  parm.title->required = NO;
  parm.title->description = "Title for resultant imagery (group) file";
  parm.title->answer="TM Imagery File Extracted From Tape";
  
  flag.q = G_define_flag();
  flag.q->key = 'q';
  flag.q->description = "quiet";

  if (G_parser(argc,argv))
        exit(-1);
        
  I_must_be_imagery_projection();
  G_want_histogram(1);

  inf = parm.input->answer;
  outf = parm.group->answer;
  if (G_legal_filename(outf) < 0) {
    fprintf (stderr, "\nERROR: <%s> - illegal group file name\n", outf);
    G_usage();
    exit(1);
  }

  if (parm.win_rows->answer) 
    if (sscanf(parm.win_rows->answer,"%d-%d",&firstrow, &lastrow) !=2 
	|| lastrow<firstrow || firstrow < 1) 
    {
    fprintf (stderr, "\nERROR: <%s> -- illegal rows\n", parm.win_rows->answer);
    G_usage();
    exit(1);
    }

  if (parm.win_cols->answer)
    if (sscanf(parm.win_cols->answer,"%d-%d",&firstcol, &lastcol) !=2 
	|| lastcol<firstcol || firstcol < 1) 
    {
    fprintf (stderr, "\nERROR: <%s> -- illegal cols\n", parm.win_cols->answer);
    G_usage();
    exit(1);
    }

  for (i = 0; i < THEMATIC_MAPPER_NBANDS; i++)
	tape.wantband[i] = 0;
  if (parm.bands_wanted->answer) {
    for (i=0; parm.bands_wanted->answers[i]; i++) {
      band_no =  atoi(parm.bands_wanted->answers[i]);
      if (band_no <= 0 || band_no > THEMATIC_MAPPER_NBANDS) {
	fprintf (stderr, "\nERROR: <%d> -- illegal band\n", band_no);
	G_usage();
	exit(1);
      }
      tape.wantband[band_no-1] = 1;
    }
  }

  verbose = !flag.q->answer;

  I_clear_tape_info (&tape.info);

/* transfer extract window parameters and title */
  tape.firstrow=firstrow;
  tape.lastrow=lastrow;
  tape.firstcol=firstcol;
  tape.lastcol=lastcol;
  strcpy(tape.info.title, parm.title->answer);

/* examine file name */
  if (test_pathname(inf) == 0)
    exit(0);
  strcpy(tape.name, inf);
  tape.fd = -1;

/* read volume header file from first tape */
    mount_tape ();
    if (!(read_header(0))) {
      unmount_tape();
      exit (-1);
    }

/* copy group name to the global variable */
    strcpy(tape.grp_name, outf);

/* read volume descriptor file */
    header(0);  
    I_tape_advance(tape.fd, -1);

/* band sequential or band interleave? */

    switch (tape.interleaving) {

    case BSQ:	bsq();
		break;

    case BIL:	bil();
		break;

    default:	fprintf(stderr, "Unknown interleaving type\n");
		exit(0);
    }

/* extraction done */

    unmount_tape();
    exit(0);
}
