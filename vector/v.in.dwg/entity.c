#define AD_PROTOTYPES
#define AD_VM_PC

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>
#include <unistd.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "ad2.h"
#include "global.h"

#define exampleprintf printf
#define LOCPI 3.14159265358979323846

char buf[1000];
char buf2[1000];

void getEntTypeName ( PAD_ENT_HDR adenhd, char *name )
{
      switch (adenhd->enttype) {
	case AD_ENT_LINE:      strcpy ( name,"LINE");      break;
	case AD_ENT_POINT:     strcpy ( name,"POINT");     break;
	case AD_ENT_CIRCLE:    strcpy ( name,"CIRCLE");    break;
	case AD_ENT_SHAPE:     strcpy ( name,"SHAPE");     break;
	case AD_ENT_ELLIPSE:   strcpy ( name,"ELLIPSE");   break;
	case AD_ENT_SPLINE:    strcpy ( name,"SPLINE");    break;
	case AD_ENT_TEXT:      strcpy ( name,"TEXT");      break;
	case AD_ENT_ARC:       strcpy ( name,"ARC");       break;
	case AD_ENT_TRACE:     strcpy ( name,"TRACE");     break;
	case AD_ENT_SOLID:     strcpy ( name,"SOLID");     break;
	case AD_ENT_BLOCK:     strcpy ( name,"BLOCK");     break;
	case AD_ENT_ENDBLK:    strcpy ( name,"ENDBLK");    break;
	case AD_ENT_INSERT:    strcpy ( name,"INSERT");    break;
	case AD_ENT_ATTDEF:    strcpy ( name,"ATTDEF");    break;
	case AD_ENT_ATTRIB:    strcpy ( name,"ATTRIB");    break;
	case AD_ENT_SEQEND:    strcpy ( name,"SEQEND");    break;
	case AD_ENT_POLYLINE:  strcpy ( name,"POLYLINE");  break;
	case AD_ENT_VERTEX:    strcpy ( name,"VERTEX");    break;
	case AD_ENT_LINE3D:    strcpy ( name,"3DLINE");    break;
	case AD_ENT_FACE3D:    strcpy ( name,"3DFACE");    break;
	case AD_ENT_DIMENSION: strcpy ( name,"DIMENSION"); break;
	case AD_ENT_VIEWPORT:  strcpy ( name,"VIEWPORT");  break;
	case AD_ENT_SOLID3D:   strcpy ( name,"SOLID3D");   break;
	case AD_ENT_RAY:       strcpy ( name,"RAY");       break;
	case AD_ENT_XLINE:     strcpy ( name,"XLINE");     break;
	case AD_ENT_MTEXT:     strcpy ( name,"MTEXT");     break;
	case AD_ENT_LEADER:    strcpy ( name,"LEADER");    break;
	case AD_ENT_TOLERANCE: strcpy ( name,"TOLERANCE"); break;
	case AD_ENT_MLINE:     strcpy ( name,"MLINE");     break;
	case AD_ENT_BODY:      strcpy ( name,"BODY");      break;
	case AD_ENT_REGION:    strcpy ( name,"REGION");    break;
	default:
	    if (adenhd->enttype==adOle2frameEnttype(dwghandle)) 
		strcpy ( name, "OLE2FRAME" );
            else if (adenhd->enttype==adLwplineEnttype(dwghandle)) 
		strcpy ( name, "LWPOLYLINE" );
	    else if (adenhd->enttype==adHatchEnttype(dwghandle)) 
		strcpy ( name, "HATCH" );
	    else if (adenhd->enttype==adImageEnttype(dwghandle)) 
		strcpy ( name, "IMAGE" );
	    else if (adenhd->enttype == adArcAlignedTextEnttype(dwghandle)) 
		strcpy ( name, "ArcAlignedText" );
	    else if (adenhd->enttype == adWipeoutEnttype(dwghandle)) 
		strcpy ( name, "Wipeout" );
	    else if (adenhd->enttype == adRtextEnttype(dwghandle)) 
		strcpy ( name, "Rtext" );
	    else /* regular proxy */
		strcpy ( name, "Proxy" );
	break;
      }
}

int write_line ( PAD_ENT_HDR adenhd, int type, int level )
{
    int i, l;
    double x, y, r, ang;

    adSeekLayer ( dwghandle, adenhd->entlayerobjhandle, Layer);

    /* Transformation, go up through all levels of transformation */
    /* not sure what is the right order of transformation */
    for ( l = level; l >= 0; l-- ) {
	for ( i = 0; i < Points->n_points; i++ ){
	    /* scale */
	    x = Points->x[i] * Trans[l].xscale; y = Points->y[i] * Trans[l].yscale;
	    /* rotate */
	    r = sqrt ( x * x + y * y );
	    ang = atan2 ( y, x ) + Trans[l].rotang;
	    x = r * cos ( ang ); y = r * sin ( ang );
	    /* move */
	    x += Trans[l].dx; y += Trans[l].dy;
	    G_debug ( 3, "level %d : %f %f -> %f %f", l, Points->x[i], Points->y[i], x, y);
	    Points->x[i] = x; Points->y[i] = y;
	}
    }
    
    Vect_reset_cats ( Cats );
    Vect_cat_set ( Cats, 1, cat );
    Vect_write_line ( &Map, type, Points, Cats );

    /* Cat */
    sprintf ( buf, "insert into %s values ( %d", Fi->table,  cat  );
    db_set_string (  &sql, buf);
    
    /* Entity name */
    getEntTypeName ( adenhd, buf2 );
    sprintf ( buf, ", '%s'", buf2 );  
    db_append_string (  &sql, buf);
    
    /* Color */
    sprintf ( buf, ", %d", adenhd->entcolor );  
    db_append_string (  &sql, buf);

    /* Weight */
    sprintf ( buf, ", %d", adenhd->lineweight );  
    db_append_string (  &sql, buf);

    /* Layer name */
    if (!Layer->purgedflag && Layer->name != NULL ) { 
	sprintf ( buf, ", '%s'", Layer->name ); 
    } else { sprintf ( buf, ", ''"); }
    db_append_string (  &sql, buf);

    /* Block name */
    if ( Block != NULL) { 
	db_set_string( &str, Block); 
	db_double_quote_string ( &str ); 
    } else {  
	db_set_string( &str, "");
    }
    sprintf ( buf, ", '%s'", db_get_string ( &str) );  
    db_append_string (  &sql, buf);

    /* Text */
    if ( Txt != NULL) { 
	db_set_string( &str, Txt); 
	db_double_quote_string ( &str ); 
    } else {  
	db_set_string( &str, "");
    }
    sprintf ( buf, ", '%s'", db_get_string ( &str) );  
    db_append_string (  &sql, buf);
    
    db_append_string (  &sql, ")");
    G_debug ( 3, db_get_string ( &sql ) );

    if (db_execute_immediate (driver, &sql) != DB_OK ) {
        db_close_database(driver);
	db_shutdown_driver(driver);
        G_fatal_error ( "Cannot insert new row: %s", db_get_string ( &sql )  );
    }

    cat++;
    return 0;
}

void wrentity (PAD_ENT_HDR adenhd,PAD_ENT aden, int level, AD_VMADDR entlist )
{
  short ret;
  PAD_BLOB_CTRL bcptr;
  AD_LAY layer;
  PAD_ENT_HDR adenhd2;
  PAD_ENT aden2;
  OdaLong il;
  double tempdouble[2],tempbulge,tempwidth[2];
  double x, y, ang;
  PAD_BLKH adblkh;

  Txt = NULL;
  adenhd2=(PAD_ENT_HDR)malloc(sizeof(AD_ENT_HDR));
  aden2=(PAD_ENT)malloc(sizeof(AD_ENT));
  adblkh=(PAD_BLKH)malloc(sizeof(AD_BLKH));
  
  ret=adSeekLayer(dwghandle,adenhd->entlayerobjhandle,&layer);

  getEntTypeName ( adenhd, buf );
  G_debug( 1, "Entity: %s", buf);
      
  Vect_reset_line ( Points );
  
  /* Check space for lower level */
  if ( level + 1 == atrans ) {
      atrans += 10;
      Trans = (TRANS *) G_realloc ( Trans, atrans * sizeof(TRANS) ); 
  }
  
  switch (adenhd->enttype) {
  case AD_ENT_LINE:
    Vect_append_point ( Points, aden->line.pt0[0], aden->line.pt0[1], 0 );
    Vect_append_point ( Points, aden->line.pt1[0], aden->line.pt1[1], 0 );
    write_line ( adenhd, GV_LINE, level );
    break;

  case AD_ENT_TEXT:
    Txt = aden->text.textstr;
    Vect_append_point ( Points, aden->text.pt0[0], aden->text.pt0[1], 0 );
    write_line ( adenhd, GV_POINT, level );
    break;

  case AD_ENT_POINT:
    Vect_append_point ( Points, aden->point.pt0[0], aden->point.pt0[1], 0 );
    write_line ( adenhd, GV_POINT, level );
    break;

  case AD_ENT_ARC:
    for ( ang = aden->arc.stang; ang < aden->arc.endang; ang += 2 * LOCPI / 360 ) {
	x = aden->arc.pt0[0] + aden->arc.radius * cos ( ang );
	y = aden->arc.pt0[1] + aden->arc.radius * sin ( ang );
	Vect_append_point ( Points, x, y, 0 );
    }
    x = aden->arc.pt0[0] + aden->arc.radius * cos ( aden->arc.endang );
    y = aden->arc.pt0[1] + aden->arc.radius * sin ( aden->arc.endang );
    Vect_append_point ( Points, x, y, 0 );
    write_line ( adenhd, GV_LINE, level );
    break;

  case AD_ENT_CIRCLE: 
    for ( ang = 0; ang < 2 * LOCPI; ang += 2 * LOCPI / 360 ) {
	x = aden->circle.pt0[0] + aden->circle.radius * cos ( ang );
	y = aden->circle.pt0[1] + aden->circle.radius * sin ( ang );
	Vect_append_point ( Points, x, y, 0 );
    }
    Vect_append_point ( Points, Points->x[0], Points->y[0], Points->z[0] );
    write_line ( adenhd, GV_LINE, level );
    break;

  /* BLOCK starts block of entities but makes no transformation - is it right ? 
  *  -> do nothing just warn for xref */
  case AD_ENT_BLOCK: 
    if (aden->block.xrefpath[0]) {
        G_warning ("External reference for block not supported.\n  xref: %s", aden->block.xrefpath );
    }
    Block = G_store ( aden->block.name2 );
    break;

  case AD_ENT_ENDBLK: /* endblk - no data */
    G_free ( Block );
    Block = NULL;
    break;

  case AD_ENT_INSERT: /* insert */
    /* get transformation */
    G_debug (3, " x,y: %f, %f",aden->insert.pt0[0], aden->insert.pt0[1]);
    G_debug (3, " xscale, yscale: %f, %f",aden->insert.xscale, aden->insert.yscale);
    G_debug (3, " rotang: %f", aden->insert.rotang);
    G_debug (3, " ncols, nrows: %d, %d",aden->insert.numcols, aden->insert.numrows);
    G_debug (3, " coldist, rowdist: %f, %f",aden->insert.coldist, aden->insert.rowdist);

    /* write block entities */
    adSeekBlockheader ( dwghandle, aden->insert.blockheaderobjhandle, adblkh);
    if ( !adblkh->purgedflag ) {
	adStartEntityGet(adblkh->entitylist);
	while (1) {
	    ret=adGetEntity(adblkh->entitylist,adenhd2,aden2);
	    if ( adenhd2->enttype==AD_ENT_ENDBLK) break;
	    if (ret) {
		/* Set transformation for lower level */
		Trans[level+1].dx = aden->insert.pt0[0];
		Trans[level+1].dy = aden->insert.pt0[1];
		Trans[level+1].dz = aden->insert.pt0[2];
		Trans[level+1].xscale = aden->insert.xscale;
		Trans[level+1].yscale = aden->insert.yscale;
		Trans[level+1].zscale = aden->insert.zscale;
		Trans[level+1].rotang = aden->insert.rotang;
	        wrentity(adenhd2,aden2, level + 1, adblkh->entitylist); 
	    }
        }
    }
    break;

  case AD_ENT_SEQEND: /* seqend */
    break;

  case AD_ENT_POLYLINE:
    while (1) {
        ret = adGetEntity(entlist,adenhd2,aden2);
	if ( ret != 1 ) {
	    G_warning ( "Cannot get entity: %d: %s.", adError(), adErrorStr( adError() ) );
	    break;
	}
	   
	if ( adenhd2->enttype == AD_ENT_SEQEND ) break;
	if ( adenhd2->enttype != AD_ENT_VERTEX ) {
            getEntTypeName ( adenhd2, buf );
	    G_warning ( "Expected VERTEX got %s in POLYLINE -> skip", buf );
	} else {
            Vect_append_point ( Points, aden2->vertex.pt0[0], aden2->vertex.pt0[1], 0 );
	}
    };
    write_line ( adenhd, GV_LINE, level );
    break;

  default: 
    if (adenhd->enttype==adLwplineEnttype(dwghandle)) {
	  G_debug (3, "Npoints: %ld\n",aden->lwpline.numpoints);
	  bcptr=adStartBlobRead(aden->lwpline.ldblob);
	  for (il=0; il<aden->lwpline.numpoints; il++) {
	    adReadBlob2Double(bcptr,tempdouble);
	    Vect_append_point ( Points, tempdouble[0], tempdouble[1], 0 );
	    tempbulge=tempwidth[0]=tempwidth[1]=0.0;
	    if (aden->lwpline.flag & AD_LWPLINE_HAS_BULGES) {
	      adReadBlobDouble(bcptr,&tempbulge);
	    }
	    if (aden->lwpline.flag & AD_LWPLINE_HAS_WIDTHS) {
	      adReadBlob2Double(bcptr,tempwidth);
	    }
	  }
	  G_debug (3, "flag = %d", aden->lwpline.flag );
	  if ( aden->lwpline.flag & AD_LWPLINE_IS_CLOSED ) {
	      G_debug (3, "  -> is closed" );
              Vect_append_point ( Points, Points->x[0], Points->y[0], Points->z[0] );
	  }
	  write_line ( adenhd, GV_LINE, level );
	  adEndBlobRead(bcptr);
    } else {
      getEntTypeName ( adenhd, buf );
      fprintf(stderr, "%s entity not supported\n", buf);
    }
  break;

  } /* end of switch */

  free(aden2);
  free(adenhd2);
}

