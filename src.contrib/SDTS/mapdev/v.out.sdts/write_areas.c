/*
**  derived by David Stigberg from v.in.dlg code
**  US Army Construction Engineering Research Lab
**
**  last modifed 11/14/94
*/

#include "Vect.h"
#include "defines.h"
#include "externs.h"
#include "stc123.h"


/*   SDTS Area Module Schema
**   -----------------------
**
**   POLY   MODN    4  A  module name: PC01
**   POLY   RCID    6  I  record id
**   POLY   OBRP    2  I  obj representation code: PC
**
**  REMOVE
**   ATID   MODN    4  A  attribute module name: AP01
**   ATID   RCID    6  I  attribute primary id
**  ^^^^^^^^^^^^^
**
*/
/*   SDTS Area-Point Module Schema
**   -----------------------------
**
**   PNTS   MODN    4  A  module name: NA01
**   PNTS   RCID    6  I  record id
**   PNTS   OBRP    2  I  obj representation code: NA
**   
**   SADR   X      10    I   longitude
**   SADR   y      10    I   latitude
**
**   ATID   MODN     4 A  attrib module name: AP01
**   ATID   RCID     6  I  attribute primary id
**
**   ARID   MODN     4 A  area module name: PC01
**   ARID   RCID     6  I  attribute primary id
*/


write_SDTS_areas (map, pc_name, pc_title, na_name, na_title, ll_input, zone)
    struct Map_info *map;
	char *pc_name, *pc_title;
	char *na_name, *na_title;
	int ll_input, zone;
{
	FILE *fpout_na, *fpout_pc;

    P_AREA *Area;
    double x, y;
    int n_atts;
    register int area;
	char recnum_str[200];
	int  recnum;
	char pc_str[200];
	char na_str[200];
	char sadr_str[200];
	char att_str[200];
	char arid_str[200];
	int option;
	long int_lat, int_lon;

/*OPEN GT POLYGON OUTPUT FILE*/
    if (!open_sdtsfile (pc_name, &fpout_pc, 'W'))
         return (0);

/*OPEN AREA POINT OUTPUT FILE*/
    if (!open_sdtsfile (na_name, &fpout_na, 'W'))
         return (0);


/*BEGIN OUTPUT--GT POLYGON: WRITE DDR RECORD*/
	if (!begin_sdts_ddr (pc_title, fpout_pc))
	{
        end123file (&fpout_pc);
		return (0);
	}

	write_dd_fld (pc_title, fpout_pc, "POLY", "1600;&POLYGON","MODN!RCID!OBRP", "(A(4),I(6),A(2))", 3);

	write_dd_fld (pc_title, fpout_pc, "ATID", "1600;&ATTRIBUTE ID","MODN!RCID", "(A(4),I(6))", 6);


    if ((get_err_stat()) != 1)
    {
        end123file (&fpout_pc);
        end123file (&fpout_na);
        return (0);
    }

    if (!end_dd_rec(pc_title, fpout_pc))
    {
        end123file (&fpout_pc);
        end123file (&fpout_na);
        return (0);
    }


/*BEGIN OUTPUT--AREA POINT: WRITE DDR RECORD*/
	if (!begin_sdts_ddr (na_title, fpout_na))
	{
        end123file (&fpout_pc);
        end123file (&fpout_na);
		return (0);
	}

	write_dd_fld (na_title, fpout_na, "PNTS", "1600;&POINT-NODE","MODN!RCID!OBRP", "(A(4),I(6),A(2))", 6);
	write_dd_fld (na_title, fpout_na, "SADR", "1600;&SPATIAL ADDRESS","X!Y", "(2B(32))", 6);
	write_dd_fld (na_title, fpout_na, "ARID", "1600;&AREA ID","MODN!RCID","(A(4),I(6))",3);
    if ((get_err_stat()) != 1)
    {
        end123file (&fpout_pc);
        end123file (&fpout_na);
        return (0);
    }

    if (!end_dd_rec(na_title, fpout_na))
    {
        end123file (&fpout_pc);
        end123file (&fpout_na);
        return (0);
    }


/*process SDTS universe polygon, before processing the "real" polygons and 
area points*/

	  if (!begin_data_rec (pc_title, fpout_pc))
      {
         end123file (&fpout_na);
		 end123file (&fpout_pc);
		 return (0);
	  }

	  recnum = 1;
	  sprintf (recnum_str, "%6d", recnum);
      write_data_fld (pc_title, fpout_pc, DDF_ID, LEAD_ID, recnum_str, (long) strlen (recnum_str), 2 );

	  sprintf (pc_str, "%4s%6d%2s", "PC01",  recnum,  "PW" );
	  write_data_fld (pc_title, fpout_pc, "POLY", LEAD_ID, pc_str, (long) strlen (pc_str), 3 );

      if (get_err_stat() != 1)
      {
             end123file (&fpout_pc);
             end123file (&fpout_na);
             Mod[PC01].rec_cnt = 0;
             return (0);
      }

	  if (!end_data_rec (pc_title, fpout_pc)) 
      {
			end123file (&fpout_na);
			end123file (&fpout_pc);
            Mod[PC01].rec_cnt = 0;
            return (0);
      }

	  Mod[PC01].rec_cnt++;

/*process real polygons and area points*/

    for (area = 1 ; area <= map->n_areas ; area++)
    {
		Area = &(map->Area[area]);
	      
	    if (AREA_ALIVE (Area))	
		{

/*begin SDTS dd record processing*/
		  if (   (!begin_data_rec (pc_title, fpout_pc)) 
			|| (!begin_data_rec (na_title, fpout_na)) )
          {
             Mod[PC01].rec_cnt = 0;
             Mod[NA01].rec_cnt = 0;
             end123file (&fpout_pc);
             end123file (&fpout_na);
             return (0);
          }

		/*write module rec id and rec id for pc and area point*/
		/*add 1 to each record number since universe polygon = area rec #1*/
		/*this means area point record will begin with 2, which is fine*/
                /* Tin: Notice here recnum is actually the same as 
		               SDTS_poly_index[area]. */

		  ++recnum ;
		  sprintf (recnum_str, "%6d", recnum);

		/*Note: pc module is written with LEAD_ID, since presence of 
		attribute pointer foreign id is variable.  na module is totally
		fixed, however, so LEAD_ID_R is possible.
		*/

		 write_data_fld (pc_title, fpout_pc, DDF_ID, LEAD_ID, recnum_str, (long) strlen (recnum_str), 2);
		 write_data_fld (na_title, fpout_na, DDF_ID, LEAD_ID_R, recnum_str, (long) strlen (recnum_str), 2);

            /*sprintf PC str*/
		 sprintf (pc_str, "%4s%6d%2s", 
				   "PC01",  recnum,  "PC" );

            /*sprintf NA str */
		 sprintf (na_str, "%4s%6d%2s", 
				   "NA01",  recnum,  "NA" );

		/*NOT USED?*/
		/*
		if (Area->att || area == 1)
			n_atts=1;
		else
			n_atts=0;
        */
		/* The following code deals with the situation where 
		   several new polygons are generated inside an old area
		   and they share the same label (attribute) as the
		   old area.   Since all these new polygons share the
		   the same label, their area points can not be
		   obtained from P_att struct.  These new attributes are
		   marked as negative to distiguish from old positive 
		   ones. */ 
		/* x and y = SADR's for AREA POINT (NA) module*/
/*
		if (AREA_LABELED (Area))
*/
		 if (Area->att >0 && AREA_LABELED (Area))
		 {
		 	 x = map->Att[Area->att].x;
			 y = map->Att[Area->att].y;
		 }
		 else
		 {
		/*
		** Find valid area point with 
		** Vect_get_point_in_area (), which replaces dig_find_area()
		*/
			if (Vect_get_point_in_area (map, area, &x, &y) < 0)
			   G_fatal_error ("Can't create area point for area"); 
		 }

		/*now we have x and y, process it*/

#ifdef FOO 
#ifdef NO_CONVERSION
	sprintf (sadr_str, "%10ld%10ld", x, y);
	Mod[NA01].sadr_cnt++;
#else 
		 if (!ll_input)
		 {
			 utm_to_ll (x, y, &int_lat, &int_lon, zone);

/*sprint SADR*/
			 sprintf (sadr_str, "%10ld%10ld", int_lon, int_lat); 

			 Mod[NA01].sadr_cnt++;
		 }
#endif /* NO_CONVERSION */
#endif /*FOO*/

         /*determine write option according to whether it's final record*/

		 /*if final record*/
         if (area == map->n_areas)
		    option = 4;
		 else
		    option = 3;


           /*sprint ATTS and write out PC module record*/
         if (Area->att)
		 {
			/* Since new polygons may have negative att num,
			   their absolute value shall be output instead.*/
			 sprintf (att_str, "%4s%6d", 
			   "AP01", abs (Area->att));

			 write_data_fld (pc_title, fpout_pc, "POLY", LEAD_ID, pc_str, (long) strlen (pc_str), 6);

			 write_data_fld (pc_title, fpout_pc, "ATID", LEAD_ID, att_str, (long) strlen (att_str), option);

		 }
		 else /*if no att, just write out POLY field with option*/
		 {
			 write_data_fld (pc_title, fpout_pc, "POLY", LEAD_ID, pc_str, (long) strlen (pc_str), option);
		 }

         /*sprint ARID for NA module*/

		 /*add 1 to area num for output to sdts to compensate for universe pc*/
		 sprintf (arid_str, "%4s%6d", 
			   "PC01", area + 1 );

		/*now write out records for NA module*/


		  write_data_fld (na_title, fpout_na, "PNTS", LEAD_ID_R, na_str, (long) strlen (na_str), 6);


		  if (!write_NA_coords (fpout_na, x, y, zone))
		  {
		     fprintf (stderr, "Failed to write Area Point coordinates.\n");
             end123file (&fpout_pc);
             end123file (&fpout_na);
             Mod[PC01].rec_cnt = 0;
             Mod[NA01].rec_cnt = 0;
             Mod[NA01].sadr_cnt = 0;
             return (0);
		  }

		  Mod[NA01].sadr_cnt++;

		  write_data_fld (na_title, fpout_na, "ARID", LEAD_ID_R, arid_str, (long) strlen (arid_str), option);

	  /*check on any write errors before ending record, terminate if found*/
          if (get_err_stat() != 1)
          {
              end123file (&fpout_pc);
              end123file (&fpout_na);
              Mod[PC01].rec_cnt = 0;
              Mod[NA01].rec_cnt = 0;
			  Mod[NA01].sadr_cnt = 0;
              return (0);
          }

	/*end DD records*/


	      if (  !end_data_rec (pc_title, fpout_pc) 
	          ||  !end_data_rec (na_title, fpout_na) )
		  {
			  end123file (&fpout_pc);
              end123file (&fpout_na);
              Mod[PC01].rec_cnt = 0;
              Mod[NA01].rec_cnt = 0;
		      Mod[NA01].sadr_cnt = 0;
			  return (0);
          }

		  Mod[PC01].rec_cnt++;
		  Mod[NA01].rec_cnt++;


#ifdef TEST_VER
          if (Mod[PC01].rec_cnt % 10 == 0) {
		   fprintf (stderr, "processing area rec # %d\n", Mod[PC01].rec_cnt);
          }
	       /*only do 50 records*/
		   if (Mod[PC01].rec_cnt == 50) {
				end123file (&fpout_pc);
				end123file (&fpout_na);
				return(1);
		   }
#endif
	   }

    }
	end123file (&fpout_pc);
	end123file (&fpout_na);
	return (1);
}
