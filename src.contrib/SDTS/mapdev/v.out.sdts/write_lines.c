/*
**  Adapted by David Stigberg from v.in.dlg code
**  US Army Construction Engineering Research Lab
**
**  last modifed 11/14/94
*/

#include "Vect.h"
#include "stc123.h"
#include "defines.h"
#include "externs.h"
#include "globals2.h"

int *SDTS_poly_index;

/*      SDTS Complete Chain Module Schema
**      ---------------------------------
**
**      LINE    MODN   4  A  module name: LE01  LINE
**      LINE    RCID   6  I  record id
**      LINE    OBRP   2  A  obj repr code
**
**      ATID    MODN   4  A  module name: AP01  ATTRIBUTE ID
**      ATID    RCID   6  I  foreign rec id
** 
**      PIDL    MODN   4  A  module name: PC01  POLYGON LEFT
**      PIDL    RCID   6  I  foreign rec id
**
**      PIDR    MODN   4  A  module name: PC01  POLYGON RIGHT
**      PIDR    RCID   6  I  foreign rec id
**
**      SNID    MODN   4  A  module name: NO01  STARTNODE ID
**      SNID    RCID   6  I  foreign rec id
**
**      ENID    MODN   4  A  module name: NO01  ENDNODE ID
**      ENID    RCID   6  I  foreign rec id
**
**      repeating fields:
**      SADR    X     10  I  longitude    SPATIAL ADDRESS
**      SADR    Y     10  I  latitude     SPATIAL ADDRESS
**
*/

/*      SDTS Entity Point Module Schema
**
**      PNTS    MODN   4  A  module name: NE01  LINE
**      PNTS    RCID   6  I  record id
**      PNTS    OBRP   2  A  obj repr code
**
**      SADR    X     10  I  longitude    SPATIAL ADDRESS
**      SADR    Y     10  I  latitude     SPATIAL ADDRESS
**
**      ARID    MODN   4  A  module name: "PCO1"ATTRIBUTE ID
**      ARID    RCID   6  I  foreign rec id

**      ATID    MODN   4  A  module name: AP01  ATTRIBUTE ID
**      ATID    RCID   6  I  foreign rec id
**
*/

#define DDF_ID  "0001"
#define MODN    "LE01"
#define OBRP_LE    "LE"
#define OBRP_NE    "NE"
#define LEAD_ID 'D'

write_SDTS_le_and_ne (map, le_name, le_title, ne_name, ne_title, ll_input, zone)
    struct Map_info *map;
	char *le_name;
	char *le_title;
	char *ne_name;
	char *ne_title;
    int ll_input, zone;
{
	FILE *fpout;
    P_LINE *Line;
    P_AREA *Area;
    int area, line, left, right;
    static struct line_pnts *Gpoints;
    static int first = 1;
	char tmp_str[5000];
	int cnt = 0;
	int dot_index = 0;
	long int tmp_cnt = 0;
	long int prev_cnt = 0;
	int ret = 0;
	/*DEBUG*//* int a_n_cnt = 0;*/

    /*
    **  Set up so that we can call this repetitively in the future
    */
    if (first)
    {
		first = 0;
		Gpoints = Vect_new_line_struct ();
    }


/*OPEN SDTS LINE OUTPUT FILE*/
    if (!open_sdtsfile (le_name, &fpout, 'W'))
         return (0);

/*BEGIN OUTPUT: WRITE DDR RECORD*/
    if (!begin_sdts_ddr (le_title, fpout))
    {
        end123ddrec (fpout);
        end123file (&fpout);
        return (0);
    }

	write_dd_fld (le_title, fpout, "LINE", "1600;&LINE","MODN!RCID!OBRP", "(A(4),I(6),2(4))", 6);

	write_dd_fld (le_title, fpout, "ATID", "1600;&ATTRIBUTE ID","MODN!RCID", "(A(4),I(6))", 6);

	write_dd_fld (le_title, fpout, "PIDL", "1600;&POLYGON ID LEFT","MODN!RCID","(A(4),I(6))",6);

	write_dd_fld (le_title, fpout, "PIDR", "1600;&POLYGON ID RIGHT","MODN!RCID","(A(4),I(6))",6);

	write_dd_fld (le_title, fpout, "SNID", "1600;&STARTNODE ID","MODN!RCID", "(A(4),I(6))", 6);

	write_dd_fld (le_title, fpout, "ENID", "1600;&ENDNODE ID","MODN!RCID", "(A(4),I(6))", 6);

	write_dd_fld (le_title, fpout, "SADR", "2600;&SPATIAL ADDRESS","*X!Y", "((2B(32)))", 3);

    if ((get_err_stat()) != 1)
    {
        end123ddrec (fpout);
        end123file (&fpout);
        return (0);
    }

    if (!end_dd_rec(le_title, fpout))
    {
        end123file (&fpout);
        return (0);
    }


/*BEGIN DATA PROCESSING*/
    for (line = 1 ; line <= map->n_lines ; line++)
    {
		Line = &(map->Line[line]);

		if (Aline_only && (Line->type == LINE))
		{
			if ((line == map->n_lines) && (cnt > 0) ) /*if this is final line*/
			{
				if (!backup_and_write_coords (fpout, LEAD_ID))
				{
					end123file (&fpout);
					return (0);
				   
				}
                if (!end_data_rec (le_title, fpout))
                {
                   end123file (&fpout);
                   return (0);
                }
				break;
			}
			else
			{
				continue;
			}
		}


	/* if reach the DOTs, then we are done */
/*aha! -- at this point we start processing SDTS entity points!*/

		if (Line->type == DOT)
		{
			dot_index = line;
			if (cnt) /*backup and rewrite coords for final Line of type LINE 
			         **or AREA, end rec before breaking*/
		    {
				if (!backup_and_write_coords (fpout, LEAD_ID))
				{
					end123file (&fpout);
					return (0);
				   
				}
                if (!end_data_rec (le_title, fpout))
                {
                   end123file (&fpout);
                   return (0);
                }
			}
			break;
        }

       /*if we get this far, we have a real rec to output*/
		cnt++;

        /*BEGIN DATA RECORD PROCESSION*/
        if (!begin_data_rec (le_title, fpout))
        {
           Mod[LE01].sadr_cnt = 0;
           Mod[LE01].rec_cnt = 0;
           end123file (&fpout);
           return (0);
        }


/*WRITE SDTS REC ID*/
		sprintf (tmp_str, "%6d", cnt);
		write_data_fld (le_title, fpout, DDF_ID, LEAD_ID, tmp_str, RCID_LEN, 2);

/*#1 write Line ID*/
		sprintf (tmp_str, "%4s%6d%2s", 
		   "LE01",  line, OBRP_LE );
		write_data_fld (le_title, fpout, "LINE", LEAD_ID, tmp_str, MRO_LEN, 6);

#ifdef FOO
/*#2 - process ATID - attributes **/
		if (Line->att)
			 sprintf (tmp_str, "%s%6d", "AP01", Line->att);
		else 
			 sprintf (tmp_str, "%4s%6s", "AP01", "      ");


		write_data_fld (le_title, fpout, "ATID", LEAD_ID, tmp_str, ATID_LEN, 6);

#endif
/*#2 - process ATID - attributes **/
		if (Line->att)
		{
			 sprintf (tmp_str, "%s%6d", "AP01", Line->att);
		     write_data_fld (le_title, fpout, "ATID", LEAD_ID, tmp_str, ATID_LEN, 6);
		}



/*#3 - process left and right polygons **/

		if (Line->left < 0)
		{
			left = abs(map->Isle[abs (Line->left)].area);
		}
		else
			left = Line->left;

		if (Line->right < 0)
		{
			right = abs(map->Isle[abs (Line->right)].area);
		}
		else
			right = Line->right;

   /*
   fprintf (stderr, "left poly = %d  right poly = %d\n", left, right);
   */

   /* Since there is no universe polygon in GRASS, we use polygon# 0 to
      represent it.  However in SDTS, polygon num# starts from 1, so
      here the polygon num# should be plus 1. All these things now 
      are done by build_all_polygons via SDTS_poly_index array */

		 sprintf (tmp_str, "%4s%6d", "PC01",  SDTS_poly_index[left]);
		 write_data_fld (le_title, fpout, "PIDL", LEAD_ID, tmp_str, PIDL_LEN, 6);

		 sprintf (tmp_str, "%4s%6d", "PC01",  SDTS_poly_index[right]);
		 write_data_fld (le_title, fpout, "PIDR", LEAD_ID, tmp_str, PIDR_LEN, 6);

/*#3 process start and end nodes ***/

   /*
   fprintf (stderr, "start node = %d end node = %d\n", Line->N1, Line->N2);
   */

		 sprintf (tmp_str, "%4s%6d", "NO01", Line->N1 );
		 write_data_fld (le_title, fpout, "SNID", LEAD_ID, tmp_str, SNID_LEN, 6);

		 sprintf (tmp_str, "%4s%6d", "NO01",  Line->N2 );
		 write_data_fld (le_title, fpout, "ENID", LEAD_ID, tmp_str, ENID_LEN, 6);

         /*make sure there are no write errors so far*/
         if (get_err_stat() != 1)
         {
             end123file (&fpout);
             Mod[LE01].sadr_cnt = 0;
             Mod[LE01].rec_cnt = 0;
             return (0);
         }

/*#5 process spatial addresses**/

	/* Note ++   
	**   This should more correctly be done by creating an Xarray/Yarray
	**   and then calling Vect_copy_xy_to_points ()  As dig_alloc_points
	**   is not currently a documented function.
	*/

#ifdef OLD_UNIVERSE_POLYGON

		if (line == 1) /*process universe line spatial addresses*/
		{
			Area = &(map->Area[1]);
			dig_alloc_points (Gpoints, 5);	/*Note ++  */
			Gpoints->n_points = 5;
			Gpoints->x[0] = Area->W; Gpoints->y[0] = Area->N;
			Gpoints->x[1] = Area->E; Gpoints->y[1] = Area->N;
			Gpoints->x[2] = Area->E; Gpoints->y[2] = Area->S;
			Gpoints->x[3] = Area->W; Gpoints->y[3] = Area->S;
			Gpoints->x[4] = Area->W; Gpoints->y[4] = Area->N;
		}
		else /*process all the other lines spatial addresses*/
		{
			if (0 > Vect__Read_line (map, Gpoints, Line->offset))
			{
				fprintf (stderr, "ERROR reading line %d from file\n", line);
			exit (-1);
			}
		}

#else

		if (0 > Vect__Read_line (map, Gpoints, Line->offset))
		{
			fprintf (stderr, "ERROR reading line %d from file\n", line);
		exit (-1);
		}

#endif /*OLD_UNIVERSE_POLYGON*/

		if (line == map->n_lines) /*write out coords and finish*/
			write_LE_coords (fpout, Gpoints->n_points, Gpoints->x, Gpoints->y, zone, 1);
        else /*write out coords*/
			write_LE_coords (fpout, Gpoints->n_points, Gpoints->x, Gpoints->y, zone, 0);

         if (get_err_stat() != 1)
		 {
             end123file (&fpout);
             Mod[LE01].sadr_cnt = 0;
             Mod[LE01].rec_cnt = 0;
             return (0);
		 }
		 else
			Mod[LE01].sadr_cnt += Gpoints->n_points;

		 Mod[LE01].rec_cnt++;

#ifdef TEST_VER
tmp_cnt = Mod[LE01].sadr_cnt - prev_cnt;
prev_cnt = Mod[LE01].sadr_cnt;
if (Mod[LE01].rec_cnt%20 == 0)
	fprintf (stderr, "rec = %ld points = %ld total points = %ld\n", Mod[LE01].rec_cnt, tmp_cnt, Mod[LE01].sadr_cnt);

#endif /*TEST_VER*/

		 if (!end_data_rec (le_title, fpout)){
			/*
			fprintf (stderr, " rec_cnt = %d\n", Mod[LE01].rec_cnt++);
			*/
            Mod[LE01].sadr_cnt = 0;
            Mod[LE01].rec_cnt = 0;
			end123file (&fpout);
			return (0);
         }

#ifdef FOO
    if (Mod[LE01].rec_cnt == 100)
	{
		 end123file (&fpout);
		 return;
		 /*this means DOTS won't be handled at all, ok if there are no sites*/
	}
#endif

    }

    if (!end_sdtsfile (le_title, &fpout))
	   return (0);


	/*now process DOTS--entity points*/

  /*if there are no entity points, just return*/
  if (!dot_index)
    return 1;


   /*OPEN SDTS LINE ENTITY POINTS FILE*/
    if (!open_sdtsfile (ne_name, &fpout, 'W'))
         return (0);

/*BEGIN OUTPUT: WRITE DDR RECORD*/

	if (!begin_sdts_ddr (ne_title, fpout))
    {
        end123ddrec (fpout);
        end123file (&fpout);
        return (0);
    }

	write_dd_fld (ne_title, fpout, "PNTS", "1600;&POINT-NODE","MODN!RCID!OBRP", "(A(4),I(6),A(2))", 6);
	write_dd_fld (ne_title, fpout, "SADR", "1600;&SPATIAL ADDRESS","X!Y", "(2B(32))", 6);
	write_dd_fld (ne_title, fpout, "ATID", "1600;&ATTRIBUTE ID","MODN!RCID", "(A(4),I(6))", 6);
	write_dd_fld (ne_title, fpout, "ARID", "1600;&AREA ID","MODN!RCID", "(A(4),I(6))", 3);

    if ((get_err_stat()) != 1)
    {
        end_dd_rec (ne_title, fpout);
        end123file (&fpout);
        return (0);
    }

    if (!end_dd_rec(ne_title, fpout))
    {
        end123file (&fpout);
        return (0);
	}

	cnt = 0;

   /*DEBUG*//*
   fprintf (stderr, "beginning dots: dot_index = %d  n_lines = %d\n", dot_index, map->n_lines);
 */
    for (line = dot_index ; line <= map->n_lines ; line++)
	{
		Line = &(map->Line[line]);

		if (Line->type != DOT)
		{
			fprintf (stderr, "error: non-DOT line in DOT section\n");
			fprintf (stderr, "continuing\n");
			continue;
		}

		/*
		fprintf (stderr, "E.P. node '%d' lines '%d'\n", Line->N1, map->Node[Line->N1].n_lines);
		fprintf (stderr, "E.P. node '%d' lines '%d'\n", Line->N2, map->Node[Line->N2].n_lines);
		*/
		/*following lines handle situation where the DOT is co-located with
		**other lines. this is illegal in SDTS. If it is the case, we skip it
		**here and then write it out as an (attributed) node later*/
        /*note that we do no special processing for the Aline_only situation*/

		if (map->Node[Line->N1].n_lines > 2)/* co-located with other lines?*/
		{
		   /*DEBUG*/
		   /*
		   a_n_cnt++;
		   */
		   continue; /*skip it. write it out later as node in node processing*/
		}
		else
			cnt++; 

        if (!begin_data_rec (ne_title, fpout))
        {
           Mod[NE01].sadr_cnt = 0;
           Mod[NE01].rec_cnt = 0;
           end123file (&fpout);
           return (0);
        }

/*WRITE SDTS REC ID*/
		sprintf (tmp_str, "%6d", cnt);
		write_data_fld (ne_title, fpout, DDF_ID, LEAD_ID, tmp_str, RCID_LEN, 2);

/*#1 write Line ID*/
		sprintf (tmp_str, "%4s%6d%2s", 
		   "NE01",  line,  OBRP_NE );
		write_data_fld (ne_title, fpout, "PNTS", LEAD_ID, tmp_str, MRO_LEN, 6);


/*process SADRs*/

		if (0 > Vect__Read_line (map, Gpoints, Line->offset))
		{
			fprintf (stderr, "ERROR reading line %d from file\n", line);
			exit (-1);
		}

	   write_EP_coords (fpout, Gpoints->n_points, Gpoints->x, Gpoints->y, zone);


/*write att index, if present*/
		if (Line->att)
		{
			 sprintf (tmp_str, "%4s%6d", 
			   "AP01", Line->att);
		     write_data_fld (ne_title, fpout, "ATID", LEAD_ID, tmp_str, ATID_LEN, 6);
		}

       /*get the polygon that entity point resides within*/

		area = dig_point_to_area ( map,  Gpoints->x[0], Gpoints->y[0]);

/*increment area by 1 since universe polygon is 1 in SDTS*/
/*this does mean both returns of universe polygons and not founds will be
treated the same!, unfortunately!*/

		sprintf (tmp_str, "%4s%6d", "PC01", SDTS_poly_index [area] );
		
		if (line == map->n_lines) /*write out attr index and finish*/
			write_data_fld (ne_title, fpout, "ARID", LEAD_ID, tmp_str, ARID_LEN, 4);
		else
			write_data_fld (ne_title, fpout, "ARID", LEAD_ID, tmp_str, ARID_LEN, 3);

        if (get_err_stat() != 1)
	    {
             end123file (&fpout);
             Mod[NE01].sadr_cnt = 0;
             Mod[NE01].rec_cnt = 0;
             return (0);
		}
		else
		{
			Mod[NE01].sadr_cnt++;
		    Mod[NE01].rec_cnt++;
		}

		if (!end_data_rec (ne_title, fpout))
		{
			end123file (&fpout);
			return (0);
		}
	}
   /*DEBUG*/
   /*
   fprintf (stderr, "ending dots: at_n_lines %d   e_p_lines %d\n", a_n_cnt, cnt);
   */

    end123file (&fpout);

	return (1);
}

is_lonely_node (map, node)
	struct Map_info *map;
    plus_t node;
{
    return (map->Node[node].n_lines <= 2);

}
