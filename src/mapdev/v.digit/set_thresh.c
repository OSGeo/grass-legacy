/*  @(#)set_thresh.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include <unistd.h>
#include "digit.h"
#include "gis.h"
#include "debug.h"
#include "D.h"
#include "Map_proto.h"
#include "dig_curses.h"
#include "local_proto.h"

/**
 *    file contains: set_thresh(), reset_thresh(), calc_thresh();
 *
 *    set_thresh()   - sets thresholds at start of program.
 *    reset_thresh() - resets the thresholds at anytime in the program.
 *        should be changed by user  the first time the digit file is created if
 *        ever.
 *    calc_thresh()  -  calculates and updates thresholds.
 *    map_to_dig_thresh() -  computes digit_thresh from map_thresh
**/

/*  all the CMap->head.foo  stuff is mods for 4.0  dig_head fixes */
/*
*    Upper (Scale) limit is the points per inch (PPI) of the digitizer and
*    there is no point in setting thresh smaller then that.
*
*    Lower limit is a number i picked.
*    The smaller the thresh the more points kept.
*
*/

static    float    Scale;
static    float    Lower = .1;

static    double    default_thresh = THRESH_FUDGE ;            /*  1/20 of an inch  */

int set_thresh (void) 
{

/*DEBUG*/ debugf ("In set_thresh ()\n");
    if (Dig_Enabled)
	D_get_scale    (&Scale);

#ifdef foo
    if (head.map_thresh == 0.0)                /*     new digit file  */
    {
/*DEBUG*/ debugf ("set_thresh: calling calc_thresh (%f) [default_thresh]\n", default_thresh);
        calc_thresh ( default_thresh);
        return (0);
    }
#endif
    /*
    map_to_dig_thresh (head.map_thresh);
    */

/*DEBUG*/ CMap->head.digit_thresh = default_thresh;

/*DEBUG*/ debugf ("set_thresh: calling calc_thresh (%f) [digit_thresh] \n", CMap->head.digit_thresh);
    calc_thresh (CMap->head.digit_thresh);
    calc_snap_thresh (CMap->head.digit_thresh);
/*DEBUG*/ debugf ("SET_THRESH: prune %f  snap %f\n", CMap->prune_thresh, CMap->snap_thresh);
    return (0);

}        /*  set_thresh()   */



int 
reset_snap_thresh (struct Map_info *map)
{

    double    cur_digit, save_digit;
    double conversion;

    char    buf[80];


    Clear_info ();

    /*
    map_to_dig_thresh (map->snap_thresh);
    save_digit = head.digit_thresh;
    */
    save_digit = _map_to_dig_thresh (map->snap_thresh);
    
    conversion = dig_unit_conversion() ;
    /* avoid FPE */
    if (map->head.orig_scale == 0 || conversion == 0.0)
	return (0);

    while (1)
     {
    
        cur_digit = map->snap_thresh / (map->head.orig_scale * conversion);

        Write_info ( 1, "  Current SNAPPING threshold:");

	sprintf ( buf, "         Map : %f  %s. ", map->snap_thresh, 
	    G_database_unit_name (1));
        Write_info ( 2, buf);

        sprintf ( buf, "         Dig : %f  -  1 / %f of an inch. ",
            cur_digit, 1.0 /cur_digit);
        Write_info ( 3, buf);
    
    
        Write_info ( 4, "   Enter new Dig threshold or <RETURN> when finished: ");
    
        cur_digit = -1;
        Get_curses_text (buf);
        sscanf (buf, "%lf", &cur_digit);

        if (cur_digit < 0) {                  /*  just hit return  */
            if ( cur_digit == save_digit)    /*  did they change it  */
                return (0);
            else
                return (1);
        }

        if ( cur_digit < Scale  ||  cur_digit > Lower)
         {
            Write_info ( 2, "");
            sprintf ( buf, "    %f  is not a reasonable threshold.", cur_digit);
            Write_info ( 3, buf);

	    if (Dig_Enabled)
	    {
            sprintf ( buf, " Threshold should be between   %f  and  %f.", 
            Lower, Scale);
            Write_info ( 4, buf);
	    }

            sleep (5);

            continue;
         }


	/* make sure this gets reset */
	map->snap_thresh = 0.0;
        /*  calc_thresh changes all thresholds  */
        calc_snap_thresh (cur_digit);


     }        /*  while(1)  */

}        /*  reset_thresh()  */


int 
reset_thresh (struct Map_info *map)
{

    double    cur_digit, save_digit;

    char    buf[80];


/*DEBUG*/ debugf ("In RESET_THRESH ()\n");
    Clear_info ();

    save_digit = map->head.digit_thresh;

    /* avoid FPE */
    if (map->head.orig_scale == 0 || dig_unit_conversion()  == 0.0)
	return (0);
    
    while (1)
     {

    /*DEBUG*/ debugf ("reset_thresh: prune %f  scale %d  unit %f\n",
    /*DEBUG*/        map->prune_thresh , map->head.orig_scale , dig_unit_conversion());

	cur_digit = map->prune_thresh / (dig_unit_conversion() * map->head.orig_scale);
    /*DEBUG*/ debugf ("reset_thresh: cur_digit = %f\n", cur_digit);

	Write_info ( 1, "  Current thresholds:");

	sprintf ( buf, "         Map : %f  %s. ", 2.0 * map->prune_thresh, 
	    G_database_unit_name (1));
	Write_info ( 2, buf);

	sprintf ( buf, "         Dig : %f  -  1 / %f of an inch. ",
	    2.0 * cur_digit, 1.0 /(2.0 * cur_digit));
	Write_info ( 3, buf);


	Write_info ( 4, "   Enter new Dig threshold or <RETURN> when finished: ");

	cur_digit = -1;
	Get_curses_text (buf);
	sscanf (buf, "%lf", &cur_digit);

	if (cur_digit < 0) {                  /*  just hit return  */
	    if ( cur_digit == save_digit)    /*  did they change it  */
	        return (0);
	    else
	        return (1);
        }

	    if ( cur_digit < Scale  ||  cur_digit > Lower)
	     {
		Write_info ( 2, "");
		sprintf ( buf, "    %f  is not a reasonable threshold.", cur_digit);
		Write_info ( 3, buf);

		if (Dig_Enabled)
		{
		sprintf ( buf, " Threshold should be between   %f  and  %f.", 
		Lower, Scale);
		Write_info ( 4, buf);
		}

		sleep (5);

		continue;
	     }


	    /* make sure this gets reset */
	    map->prune_thresh = 0.0;
	    /*  calc_thresh changes all thresholds  */
	    calc_thresh (cur_digit);


     }        /*  while(1)  */

}        /*  reset_thresh()  */


int 
calc_snap_thresh (double new_thresh)
{
/*
    float scale;
    float dig_units;
    double X1, Y1, X2, Y2;
    double hypot();
*/
    double tmp_thresh;

/*
    head.digit_thresh = new_thresh ;
*/
/*DEBUG*/ debugf ("In CALC_SNAP(%f)\n", new_thresh);
    tmp_thresh = new_thresh * CMap->head.orig_scale * dig_unit_conversion ();

    /*
    if (CMap->snap_thresh == 0.0)
    */
	CMap->snap_thresh = tmp_thresh;
/*DEBUG*/ debugf ("CALC_THRESH: snap_thresh = %f\n", CMap->snap_thresh);

#ifdef foo
    D_get_scale(&scale) ;         /* Returns inches/per dig unit */
                                  /* Threshold in meters on ground */
    dig_units = (float)CMap->head.digit_thresh / scale;

    transform_a_into_b ((double) 0.0, (double) 0.0, &X1, &Y1);
    transform_a_into_b ((double) dig_units, (double) 0.0, &X2, &Y2);

    CMap->head.map_thresh = hypot(X2-X1, Y2-Y1);
    sample_thresh = CMap->head.map_thresh / 2.0;
#endif
    return 0;
}



int 
calc_thresh (double new_thresh)
{
/*
    float scale;
    float dig_units;
    double X1, Y1, X2, Y2;
    double hypot();
*/

/*DEBUG*/ debugf ("In CALC_THRESH(%f)\n", new_thresh);
    CMap->head.digit_thresh = new_thresh ;     /* Threshold in inches on map */
    CMap->head.map_thresh = new_thresh * dig_unit_conversion() * CMap->head.orig_scale;

    /*
    if (CMap->prune_thresh == 0.0)
    */
	CMap->prune_thresh = CMap->head.map_thresh / 2.;
/*DEBUG*/ debugf ("CALC_THRESH: prune = %f\n", CMap->prune_thresh);

#ifdef foo
    D_get_scale(&scale) ;         /* Returns inches/per dig unit */
                                  /* Threshold in meters on ground */
    dig_units = (float)CMap->head.digit_thresh / scale;

    transform_a_into_b ((double) 0.0, (double) 0.0, &X1, &Y1);
    transform_a_into_b ((double) dig_units, (double) 0.0, &X2, &Y2);

    CMap->head.map_thresh = hypot(X2-X1, Y2-Y1);
    sample_thresh = CMap->head.map_thresh / 2.0;
#endif
    return 0;
}

int map_to_dig_thresh (double map_thresh)
{
    float dig_units;

    CMap->head.digit_thresh = _map_to_dig_thresh (map_thresh);
    return 0;
}

double _map_to_dig_thresh (double map_thresh)
{
    float dig_units;
    /* double X1, Y1, X2, Y2; */
    double dig_thresh;


#ifdef hmmm
    if (Dig_Enabled)
    {

	transform_b_into_a ((double) 0.0, (double) 0.0, &X1, &Y1);
	transform_b_into_a ((double) map_thresh, (double) 0.0, &X2, &Y2);

	dig_units = hypot(X2-X1, Y2-Y1);

	dig_thresh =  Scale * dig_units;

    }
    else
#endif
    {

	dig_thresh = map_thresh / (dig_unit_conversion () * CMap->head.orig_scale);
    }
    return (dig_thresh);
}
