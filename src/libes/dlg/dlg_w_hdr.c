/****************************************************************/
/*								*/
/*	dlg_wr_hdr.c	in ~/src/libes/dlg			*/
/*								*/
/*	#include "dlg.h"					*/
/*								*/
/*      dlg_write_header (fd,dlg)				*/
/*      FILE *fd ;						*/
/*      struct dlg *dlg;					*/
/*								*/
/*      This routine takes the dlg header information from 	*/
/*	the structure 'dlg' and writes it to the file 'fd'.	*/
/*								*/
/*      returns:  -1 on error 					*/
/*            	   0 on completion 				*/
/*								*/
/****************************************************************/

#include <stdio.h>
#include "dlg.h"

#define PUT(x,y,z) fwrite(x,y,z,fd)

dlg_write_header (fd,dlg)
	FILE *fd ;
	struct dlg *dlg;
{
	char buffer[81] ;
	int i ;

	dlg->head.nlines = 0 ;

/* Write first (banner) line */
	PUT(dlg->head.banner, sizeof(*dlg->head.banner), 72) ;

/* Write second line 
 *   Contains: cart_unit, source_date, orig_scale
 */
   PUT(dlg->head.cart_unit,sizeof(*dlg->head.cart_unit),40);
   PUT(dlg->head.source_date,sizeof(*dlg->head.source_date),10);
   PUT(dlg->head.orig_scale,sizeof(*dlg->head.orig_scale),8);

/* Write third line 
 *   Used, but meaning undefined
 */
	PUT(dlg->head.line_3, sizeof(*dlg->head.line_3), 72) ;

/* Write fourth line */
  PUT(&dlg->head.level_code,  sizeof(dlg->head.level_code),  1);
  PUT(&dlg->head.plani_code,  sizeof(dlg->head.plani_code),  1);
  PUT(&dlg->head.plani_zone,  sizeof(dlg->head.plani_zone),  1);
  PUT(&dlg->head.plani_units, sizeof(dlg->head.plani_units), 1);
  PUT(&dlg->head.resolution,  sizeof(dlg->head.resolution),  1);
  PUT(&dlg->head.trans_param, sizeof(dlg->head.trans_param), 1);
  PUT(&dlg->head.misc_records,sizeof(dlg->head.misc_records),1);
  PUT (&dlg->head.num_sides,   sizeof(dlg->head.num_sides),  1);
  PUT (&dlg->head.num_cats,    sizeof(dlg->head.num_cats),   1);

/* Write fifth through ninth lines */
	PUT (dlg->proj.params, sizeof(*dlg->proj.params), 15) ;
	
/* Write tenth line */
     PUT(dlg->proj.int_params,sizeof(*dlg->proj.int_params),4);

/* Write eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		if (i==0) sprintf(buffer,"SW");
	   else if (i==1) sprintf(buffer,"NW");
	   else if (i==2) sprintf(buffer,"NE");
	   else		  sprintf(buffer,"SE"); 

		PUT (buffer, sizeof(*buffer), 2) ;

   PUT (&dlg->coors.lat[i],sizeof(dlg->coors.lat[i]),    1);
   PUT (&dlg->coors.lon[i],sizeof(dlg->coors.lon[i]),    1);
   PUT (&dlg->coors.utm_e[i],sizeof(dlg->coors.utm_e[i]),1);
   PUT (&dlg->coors.utm_n[i],sizeof(dlg->coors.utm_n[i]),1);
	}

	dlg->head.nlines = 14 ;

/* Write one more line for each category */

	for (i=0; i<32; i++)
		dlg->cats.read = 0 ;

	for (i=0; i<dlg->head.num_cats; i++)
	{
		dlg->cats.read = 1 ;
  PUT (dlg->cats.name,       sizeof(*dlg->cats.name),    20) ;
  PUT (&dlg->cats.form_code, sizeof(dlg->cats.form_code), 1) ;
  PUT (&dlg->cats.num_nodes, sizeof(dlg->cats.num_nodes), 1) ;
  PUT (&dlg->cats.act_nodes, sizeof(dlg->cats.act_nodes), 1) ;
  PUT (&dlg->cats.nta_link,  sizeof(dlg->cats.nta_link),  1) ;
  PUT (&dlg->cats.ntl_link,  sizeof(dlg->cats.ntl_link),  1) ;
  PUT (&dlg->cats.num_areas, sizeof(dlg->cats.num_areas), 1) ;
  PUT (&dlg->cats.act_areas, sizeof(dlg->cats.act_areas), 1) ;
  PUT (&dlg->cats.atn_link,  sizeof(dlg->cats.atn_link),  1) ;
  PUT (&dlg->cats.atl_link,  sizeof(dlg->cats.atl_link),  1) ;
  PUT (&dlg->cats.area_list, sizeof(dlg->cats.area_list), 1) ;
  PUT (&dlg->cats.num_lines, sizeof(dlg->cats.num_lines), 1) ;
  PUT (&dlg->cats.act_lines, sizeof(dlg->cats.act_lines), 1) ;
  PUT (&dlg->cats.line_list, sizeof(dlg->cats.line_list), 1) ;
	}
	return(0) ;
}
