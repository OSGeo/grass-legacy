#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "local_structs.h"

extern int has_mid;
extern d_type version_no;
extern d_type del0;
extern d_type proj_info;
extern field_data field_info;
extern field_data field_type;
extern int numcols;
extern char delchar;

int sites_write(site_array *s0, char *sitename, d_type *di, d_type del,
		int numsites, FILE *logfp) {

  /* local */

  field_data fd1;
  int i, j;    /* loop */
  int fields;
  int *strflags;

  int len;
  d_type tmp1;
  char *tmpptr;
  char chdel[2];

  Site *site1;
  Site_head *sh1;
  int nnum = 0, nstr = 0;

  FILE *sfp;

  /*
  char shname[64];

  sh1 = (Site_head *)G_malloc( sizeof(Site_head) );
  */

  sfp = G_sites_open_new(sitename);

  fprintf(logfp, "Mapinfo Data File:\n\n");
  fprintf(logfp, "VERSION: %s\n", version_no);
  chdel[0] = delchar; 
  chdel[1] = '\0';
  fprintf(logfp, "DELIMITER: %s\n", chdel);
  fprintf(logfp, "Projection info: %s\n", proj_info);

  fprintf(logfp, "\nField data -- %d columns\n\n", numcols);

  for( i = 0; i < numcols; i++ ) 
    fprintf(logfp, "Field %d is %s of type %s\n", i, field_info[i], field_type[i]);
	  
  for ( i = 0; i < numsites; i++ ) {

    fprintf( logfp, "Site %d\n\n", i );


    if(has_mid) {
      parse_all_fields(di, &fd1, chdel, i, &fields);
      strflags = (int *)G_malloc(fields * sizeof(int) );

      nstr = 0; nnum = 0;
      for( j = 0; j < fields; j++ ) {
	if( fd1[j][0] == '\"' ) {
	  len = strlen(fd1[j]);
	  fd1[j][len - 1] = '\0';
	  strcpy(tmp1, (&fd1[j][0] + 1));
	  strcpy(fd1[j], tmp1);
	  strflags[j] = 1;
	  nstr++;
	}
	else {
	  strflags[j] = 0;
	  nnum++;
	}

      }

      if(i == 0)
	site1 = G_site_new_struct(CELL_TYPE, 2, nstr, nnum);
      /* FIXME: Get parsing of datatypes working */

      nstr = 0; nnum = 0;
      for( j = 0; j < fields; j++ ) {

	if(strflags[j]) {
	  fprintf(logfp, "STRING: %s\n", fd1[j]);
	  site1->str_att[nstr++] = &fd1[j][0];
	}
	
	else {
	  fprintf(logfp, "NUMERIC: %s\n", fd1[j]);
	  site1->dbl_att[nnum++] = atof(fd1[j]);
	}

      }
      
      G_free(strflags);

    }

    else {
      
      if(i == 0) {
	site1 = G_site_new_struct(CELL_TYPE, 2, 0, 0 );
	/* FIXME: Get parsing of datatypes working */
      }

      
    }
    
    site1->ccat = i;
    site1->east = s0->x[i];
    site1->north = s0->y[i];

    G_site_put(sfp, site1);
    
    fprintf(logfp, "\n");
    fprintf(logfp, "X: %lf\n", s0->x[i]);
    fprintf(logfp, "Y: %lf\n", s0->y[i]);
    fprintf(logfp, "\n\n\n");


  }

  G_site_free_struct(site1);
  fclose(sfp);

  return 0;
}




/* Separate out the fields using the established delimiters */

int parse_all_fields(d_type *dt, field_data *fd0, d_type del0, int icnt, int *fcnt) {

  /* local */

  char *tmpbuf;
  int cnt;

  cnt = 0;

  if( (tmpbuf = strtok(dt[icnt], del0)) == NULL ) {
    fprintf( stderr, "Data contains no fields\n");
    return -1;
  }

  strcpy( (*fd0)[cnt++], tmpbuf );

  while( (tmpbuf = strtok(NULL, del0)) != NULL ) {
    strcpy( (*fd0)[cnt++], tmpbuf );
  }

  *fcnt = cnt;

  return 0;
}
