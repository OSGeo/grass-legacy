#define TOPLEVEL

#include "gis.h"
#include "globals.h"
#include "parse.h"
#include "opcode.h"
#include "make.h"


/* Here is the top-level C routine */
toplevel_(numarg,for_pname,for_fname, pname_len,fname_len)
char *for_fname,*for_pname;
int *numarg,fname_len,pname_len;
{
  int i, temp_int;
  char inp_buffer[INP_SIZ];
  char *progname;

  /* clean up the FORTRAN strings (allocate local space and null terminate) */
  *File_name = 0;
  if (*numarg > 0){
  for (i=0; for_fname[i]!=' ' && i<fname_len; i++);
  fname_len = i;
  if (fname_len>7)
    G_fatal_error("Data base name too long (should be 7 characters or less).");
  strncpy(File_name, for_fname, fname_len);
  File_name[fname_len] = '\0';
  }
  for (i=0; for_pname[i]!=' ' && i<pname_len; i++);
  pname_len = i;
  if ((progname = G_malloc(pname_len+1))==NULL)
    G_fatal_error("Unable to allocate memory in top_level.");
  strncpy(progname, for_pname, pname_len);
  progname[pname_len] = '\0';

  G_gisinit(progname);
  G_sleep_on_error(0);

  /* initialize the field values to NULL */
  Field_num = 0;
  init_field_val();

  /* get a temp file name to dump unwanted output to */
  Tempdumpf = G_tempfile();

  /* check for program executed with a data base name */
  if (*numarg == 1) {
    /* Open the data base files through RIM */
    if (open_db(File_name, TRUE)==FALSE) {
      G_warning("\nThe database does not currently exist.\n");
      SLEEP3;
      parser(P_INST_DB_NA,"");  /* alert the parser to this condition */
      *File_name = '\0';
      *RIM_db_path = '\0';
    }
    else {
      get_field_info();
      init_field_val();
    }

  }

  main_menu();

  if (*File_name) close_db(File_name);

  /* get rid of temp dump file */
  unlink(Tempdumpf);

  return(0);
}


