/******************************************************************************
 * dbutils.c
 * functions for handling processing of internal database
 * relating to topological structures held in memory

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 14th. Mar. 2000
 * Last updated 23rd. Apr. 2000
 *

* This file is part of GRASS GIS. It is free software. You can 
* redistribute it and/or modify it under the terms of 
* the GNU General Public License as published by the Free Software
* Foundation; either version 2 of the License, or (at your option)
* any later version.

* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.

 ******************************************************************************/

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <setjmp.h>
#include "dbutils.h"
#include "shapefil.h"
/* #include "gis.h" */


int vertRegister( BTREE *hDB, partDescript *part1, int pt_indx ) {

  static void *ptr_old = NULL;
  static int currerror = 0;

  static int num_registered = 0;
  static int db_allocated = 0;

  /* local */
  int i;
  int np;
  int res, res1;
  int result = 0;
  char *pkey;
  float snap;
  double fe, fn;
  int idig, declen;

  double angle0, angle1;
  int linked, lnum;

  pntDescript *pf, *pb, *pc; /* Holders for current and flanking vertices */
  pntDescript *pbl;

  char *keyHolder;
  pntDescript **dataHolder, **tmpdataHolder;
  pntDescript **pntPtrPtr;

  jmp_buf startpnt;


  np = part1->numPoints;

  /* Go on if any point should be invalid */

  if( setjmp(startpnt) ) return 0;

  /* Retrieve snap distance for map */
  if( procSnapDistance( GET_SD, &snap ) ) {
    fprintf(stderr, "Could not set snap distance. Aborting." );
    exit(1);
  }

  /* Retrieve the parameters for the vbase key */

  if( proc_key_params( GET_VAL, &idig, &fe, &fn ) ) {
    fprintf(stderr, "Could not aquire parameters for setting key values. Aborting." );
    exit(1);
  }

  declen = 16 - idig;
  if( declen < 0 || declen > 16 ) {
    fprintf(stderr, "Key parameters outside acceptable range. Aborting." );
    exit(1);
  }


  /* Assign key value */
  pkey = (char *)malloc( 33 );
  strncpy( pkey, calcKeyValue( &part1->linepnts[pt_indx], snap,
			       declen, fe, fn ), 33 );


  /* Is this point registered in the database? */

  pc = &part1->linepnts[pt_indx];

  pntPtrPtr = (pntDescript **)malloc( sizeof( pntDescript *) );
  keyHolder = (char *)malloc( 33 );
  dataHolder = ( pntDescript ** )malloc( sizeof( pntDescript * ));
  tmpdataHolder = ( pntDescript ** )malloc( sizeof( pntDescript * ));
  
  *pntPtrPtr = pc;

  strncpy(keyHolder, pkey, 33 );
  dataHolder = pntPtrPtr;

  res = btree_find( hDB, keyHolder, (void **)&dataHolder );
  if( res == 0 ) btree_update( hDB, keyHolder, 33, dataHolder, 4 );

  if ( res == 1 ) {
    /* Point is already in database. Modify to reflect new links */
    res1 = 0; /* Make this sensible at some point !! */

    /* Get the point */
    /* res1 = hDB->get( hDB, keyHolder, tmpdataHolder, 0 ); */

    if( res1 == -1 ) {
      /* There was an error */
      /* G_fatal_error( "Error retrieving data from database" ); */
    }
    else if( res1 == 1 ) {
      /* No key. This shouldn't happen. Abort! */
      /* G_fatal_error( "Error retrieving data from database" ); */
    }
    else{
      pc = *( (pntDescript **) dataHolder );

      if( pt_indx == 0 )
	ptr_old = NULL;
      else {
	pb = (pntDescript *) ptr_old;

	/* Is this the same vertex. If so skip */

	if( pc == pb ) longjmp( startpnt, 1);

	/* Are we already linked to this? */
	linked = 0;
	for( i = 0; i < pb->linknum; ++i ) {
	  pbl = pb->linkverts[i];
	  if( pc == pbl )
	    linked = 1;
	}

	/* Determine angle of link to previous vertex */

	angle0 = atan2(( pb->yPosn - pc->yPosn ), ( pb->xPosn - pc->xPosn ) );
	if( angle0 < 0 ) angle0 += 2 * PI ;
	angle1 = angle0 + PI ;
	if( angle1 >= 2 * PI ) angle1 -= 2 * PI ;

	lnum = pc->linknum;

	if( !linked ) {
	  result = 1;
	  if( lnum == 0 ) {
	    pc->linkdirect = (double *)malloc( sizeof( double));
	    pc->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	  }
	  else {
	    pc->linkdirect = (double *)realloc( pc->linkdirect, (lnum + 1) * sizeof( double));
	    pc->linkverts = (pntDescript **)realloc( pc->linkverts, (lnum + 1) *
						  sizeof (pntDescript *));
	  }
	  lnum++;
	  pc->linkdirect[lnum-1] = angle0;
	  pc->linkverts[lnum-1] = pb;
	  pc->linknum = lnum;
	}


	  
	/* Now fill in the fields of the previous link */
	lnum = pb->linknum;

	if( !linked ) {
	  if( lnum == 0 ) {
	    pb->linkdirect = (double *)malloc( sizeof( double));
	    pb->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	  }
	  else {
	    pb->linkdirect = (double *)realloc( pb->linkdirect, (lnum + 1) * sizeof( double));
	    pb->linkverts = (pntDescript **)realloc( pb->linkverts, (lnum + 1) *
						  sizeof (pntDescript *));
	  }
	  lnum++;
	  pb->linkdirect[lnum-1] = angle1;
	  pb->linkverts[lnum-1] = pc;
	  pb->linknum = lnum;
	}

      }
    }

    ptr_old = pc;
    
  }
  else {
    /* Point is added: reflect new links */
    pc = *( (pntDescript **) dataHolder );
    num_registered++;
    result = 1;

    /* Determine angle of link to previous vertex */
    if( pt_indx > 0 ) {
      pb = (pntDescript *) ptr_old;


      angle0 = atan2(( pb->yPosn - pc->yPosn ), ( pb->xPosn - pc->xPosn ) );
      if( angle0 < 0 ) angle0 += 2 * PI ;
      angle1 = angle0 + PI ;
      if( angle1 >= 2 * PI ) angle1 -= 2 * PI ;

      lnum = 0;

      pc->linkdirect = (double *)malloc( sizeof( double));
      pc->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
      lnum++;
      pc->linkdirect[lnum-1] = angle0;
      pc->linkverts[lnum-1] = pb;
      pc->linknum = lnum;


	  
      /* Now fill in the fields of the previous link */
      lnum = pb->linknum;

	if( lnum == 0 ) {
	  pb->linkdirect = (double *)malloc( sizeof( double));
	  pb->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	}
	else {
	  pb->linkdirect = (double *)realloc( pb->linkdirect, (lnum + 1) * sizeof( double));
	  pb->linkverts = (pntDescript **)realloc( pb->linkverts, (lnum + 1) *
						   sizeof (pntDescript *));
	}
	lnum++;
	pb->linkdirect[lnum-1] = angle1;
	pb->linkverts[lnum-1] = pc;
	pb->linknum = lnum;
    }

    ptr_old = pc;

    
  }

  free( tmpdataHolder );
  return (result);

}


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

char *calcKeyValue( pntDescript *pnt1, float sr, int decs, double efalse,
		    double nfalse ) {
  /* local */

  double xtmp, ytmp;
  char xbuf[128], ybuf[128];
  char *retbuf;
  int indx;
  char *indx_ptr;
  int idigits;

  xtmp = ((long long)( (pnt1->xPosn + efalse) / sr )) * sr;
  ytmp = ((long long)( (pnt1->yPosn + nfalse) / sr )) * sr;

  /* To elliminate small negative values */
  if(xtmp < 0.0) xtmp = 0.0;
  if(ytmp < 0.0) xtmp = 0.0;

  if(decs < 0 || decs > 16)
    return NULL;

  idigits = 16 - decs;

  retbuf = (char *)malloc( 33 );
  
  snprintf( xbuf, 127, "%065.20f", xtmp );
  snprintf( ybuf, 127, "%065.20f", ytmp );

  indx_ptr = strchr( xbuf, '.' );
  strncpy( retbuf, indx_ptr - idigits, idigits );
  retbuf[idigits] = '\0';
  strncat( retbuf, indx_ptr + 1, decs );
  retbuf[16] = '\0';

  indx_ptr = strchr( ybuf, '.' );
  strncat( retbuf, indx_ptr - idigits, idigits );
  retbuf[16 + idigits] = '\0';
  strncat( retbuf, indx_ptr + 1, decs );
  retbuf[32] = '\0';

  return retbuf;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* Helper function definitions */

int btree_compare( char *key1, char *key2 ) {
  /* Just compare lexicographically */

  return strncmp( key1, key2, 32 );
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */



int parse_selection_fields(char *field, char *val, char *str_input) {

  char str_buf[512];
  char *str_indx;

  strncpy(str_buf, str_input, 511);

  if( (str_indx = strchr(str_buf, ':')) == NULL )
    return 1;

  else {
    
    *str_indx = '\0';
    strncpy(field, str_buf, str_indx - str_buf + 1);
    strcpy(val, str_indx + 1);
    return 0;
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */



int dbf_field_query(DBFHandle db0, char *cname, int *cnum) {

  int i;  /* loop */
  int c_field;

  if(!db0 || !cname) return -1;

  c_field = -1;

  for( i = 0; i < DBFGetFieldCount(db0); i++ )
    {
      char	field_name[15];

      DBFGetFieldInfo( db0, i, field_name, NULL, NULL );
      if( strcasecmp( cname, field_name ) == 0 )
	c_field = i;
    }

  if(c_field >= 0) {
    *cnum = c_field;
    return 0;
  }

  else return 1;

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


int procSnapDistance( int iswitch, float *sd ) {
  
  /* Set or get the SNAP_DISTANCE variable */

  static float snap_distance = 1.0e-6;

  if( iswitch == SET_VAL ) {
    if(sd) {
      snap_distance = *sd;
      return 0;
    }
    else return 1;
  }
  else if( iswitch == GET_VAL ) {
    *sd = snap_distance;
    return 0;
  }
  else return 1;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


int procMinSubtend( int iswitch, float *dphi ) {
  
  /* Set or get the minimum value at which two radii from a node
     are considered to be separate (non-colinear)
  */

  static float minimum_angle = 1.74533e-6;

  if( iswitch == SET_VAL ) {
    if(dphi) {
      minimum_angle = *dphi;
      return 0;
    }
    else return 1;
  }
  else if( iswitch == GET_VAL ) {
    *dphi = minimum_angle;
    return 0;
  }
  else return 1;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


int proc_key_params( int iswitch, int *idig, double *wbound, double *sbound ) {

  /* Set or get the parameters for fixing the vbase key value from
     the value of a location's co-ordinates
  */

  static int whole_;
  static double west_;
  static double south_;

  if(!idig || !wbound || !sbound)
    return 1;

  if( iswitch == SET_VAL ) {
    whole_ = *idig;
    west_ = *wbound;
    south_ = *sbound;

    return 0;
  }

  else if( iswitch == GET_VAL ) {
    *idig = whole_;
    *wbound = west_;
    *sbound = south_;

    return 0;
  }

  else return 1;
  
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


int proc_max_shapes( int iswitch, int *max_shapes ) {

  /* Set or get the maximum number of shapes to be extracted */

  static int max_shapes_;

  if(!max_shapes )
    return 1;

  if( iswitch == SET_VAL ) {
    max_shapes_ = *max_shapes;

    return 0;
  }

  else if( iswitch == GET_VAL ) {
    *max_shapes = max_shapes_;

    return 0;
  }

  else return 1;
  
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int proc_test_dbf( int iswitch, int *tflag, DBFHandle *hhdbf, char *fval, int *fnum ) {

  /* Set and store flags for selecting particular fields for export,
     and handle on DBF
  */
  static char val_str[512] = "";

  static DBFHandle hdbf = NULL;
  static do_query = 0;
  static field_num = -1;

  if( iswitch == SET_VAL ) {
    if(hhdbf)
      hdbf = *hhdbf;
    else
      hdbf = NULL;
    do_query = *tflag;
    if(fval)
      strncpy(val_str, fval, 511);
    else
      strncpy(val_str, "", 511);
    if(fnum)
      field_num = *fnum;
    else
      field_num = -1;

    return 0;
  }

  else if( iswitch == GET_VAL ) {
    if(hdbf) 
      *hhdbf = hdbf;
    else
      *hhdbf = NULL;
    *tflag = do_query;
    if(fval)
      strcpy(fval, val_str);
    if(fnum)
      *fnum = field_num;

    return 0;
  }

  else return 1;
  
}

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


 
int allocate_recs(duff_recs_t *duff_recs, int size) {

  int incr_size;
  int dr_incr;

  dr_incr = 3000;

  incr_size = dr_incr;

  if(duff_recs->alloc_recs > size)
    return 1;

  if(duff_recs->alloc_recs == 0) {

    if( (duff_recs->duff_rec_list = (duff_rec *)malloc(dr_incr * sizeof(duff_rec)))
	== NULL )
      return -1;

    memset(duff_recs->duff_rec_list, 0, dr_incr * sizeof(duff_rec));
    duff_recs->alloc_recs = dr_incr;
  }

  else {

    incr_size += dr_incr;
    if( (duff_recs->duff_rec_list = (duff_rec *)realloc(duff_recs->duff_rec_list,
							incr_size * sizeof(duff_rec)))
	== NULL )
      return -1;

    memset(&duff_recs->duff_rec_list[incr_size - dr_incr], 0,
	   dr_incr * sizeof(duff_rec));
    duff_recs->alloc_recs = incr_size;
  }

  return 0;

}

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


 
int add_rec_spec(duff_recs_t *duff_recs, int recno, int fduff) { 

  if(recno >= duff_recs->alloc_recs) {
    if(allocate_recs(duff_recs, recno + 1) < 0)
      return -1;
  }

  duff_recs->duff_rec_list[duff_recs->n_recs].rec_no = recno;
  duff_recs->duff_rec_list[duff_recs->n_recs++].is_duff = fduff;

  return 0;
}
