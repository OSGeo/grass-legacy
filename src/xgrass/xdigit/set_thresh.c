/*  @(#)set_thresh.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/*
**  Last, last  modified by  Terry Baker 10/1992
**  US Army Construction Engineering Research Lab
*/


#include "digit.h"

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


#include "dig_head.h"


/*
*    Upper (Scale) limit is the points per inch (PPI) of the digitizer and
*    there is no point in setting thresh smaller then that.
*
*    Lower limit is a number i picked.
*    The smaller the thresh the more points kept.
*
*/

double dig_unit_conversion ();

static    float    Scale;
static    float    Lower = .1;

static    double    default_thresh = THRESH_FUDGE ;            /*  1/20 of an inch  */

set_thresh() 
{

    if (Dig_Enabled)
	D_get_scale    (&Scale);


    CM->head.digit_thresh = default_thresh;

    calc_thresh (CM->head.digit_thresh);
    calc_snap_thresh (CM->head.digit_thresh);
    return (0);

}        /*  set_thresh()   */



reset_snap_thresh(w)
 Widget w;
{

    double    cur_digit, save_digit;
    struct Map_info *map;

    char    *tmp, buf[100];

    map = CM;

    save_digit = _map_to_dig_thresh (map->snap_thresh);
    

    tmp = XmTextGetString(w);
    sscanf (tmp, "%lf", &cur_digit);
     XtFree (tmp);


    if ( cur_digit < Scale  ||  cur_digit > Lower)
    {

	    if (Dig_Enabled)
                sprintf (buf, "%lf  is not a reasonable threshold.\nThreshold should be between   %lf  and  %lf.", cur_digit, 
		Lower, Scale);
	    else
                sprintf (buf, "%lf  is not a reasonable threshold.", cur_digit);
            make_monolog(1, buf);

     }
     else if ( cur_digit != save_digit)    /*  did they change it  */
    {
        /* make sure this gets reset */
	map->snap_thresh = 0.0;
        /*  calc_thresh changes all thresholds  */
        calc_snap_thresh (cur_digit);
    }
     show_digth();
     show_snapth();
}        /*  reset_thresh()  */


reset_thresh(w)
    Widget w;
{
    struct Map_info *map;
    double    cur_digit, save_digit;
    char    *tmp, buf[100];


    map = CM;

    save_digit = map->head.digit_thresh;
    

    tmp = XmTextGetString(w);
    sscanf (tmp, "%lf", &cur_digit);
    XtFree (tmp);


    if ( cur_digit < Scale  ||  cur_digit > Lower)
    {

	    if (Dig_Enabled)
                sprintf (buf, "%lf  is not a reasonable threshold.\nThreshold should be between   %lf  and  %lf.", cur_digit, Lower, Scale);
	    else
                sprintf (buf, "%lf  is not a reasonable threshold.", cur_digit);
            make_monolog(1, buf);

     }
     else if ( cur_digit != save_digit)    /*  did they change it  */
    {
	    /* make sure this gets reset */
	    map->prune_thresh = 0.0;
	    /*  calc_thresh changes all thresholds  */
	    calc_thresh (cur_digit);


     }      
     show_digth();
     show_snapth();
}        /*  reset_thresh()  */


calc_snap_thresh (new_thresh)
    double    new_thresh;
{

    CM->snap_thresh = new_thresh * CM->head.orig_scale * dig_unit_conversion ();
}



calc_thresh (new_thresh)
    double    new_thresh;
{

    CM->head.digit_thresh = new_thresh ;     /* Threshold in inches on map */
    CM->head.map_thresh = 
		new_thresh * dig_unit_conversion() * CM->head.orig_scale;
    CM->prune_thresh = CM->head.map_thresh / 2.;

}



map_to_dig_thresh (map_thresh)
    double map_thresh;
{
    float dig_units;
    double X1, Y1, X2, Y2;
    double hypot();


    CM->head.digit_thresh = _map_to_dig_thresh (map_thresh);
}

double
_map_to_dig_thresh (map_thresh)
    double map_thresh;
{

	return (map_thresh / (dig_unit_conversion () * CM->head.orig_scale));
}
