/****************************************************************
 *
 * scale (window, panel, margin, text)
 *
 *   reconfigures the window resolution, rows and cols
 *   based on the scale
 *
 *   window: window to be reconfigured.
 *           note: the window resolutions, rows, cols are ignored
 *
 *   panel:   number of pixels (max) in one panel including margins
 *   margin:  number of pixels in margins (outside map)
 *
 *   text:    scale text
 *
 * note: must call check_scale() first. errors here are fatal
 ***************************************************************/
#include <string.h>
#include "gis.h"
#include "Paintlib.h"
#include "local_proto.h"

#define METERS_TO_INCHES ((double)39.37)
#define MILES_TO_INCHES  ((double)5280*12)

static double do_scale (struct Cell_head *, int, int, char *);
#ifdef __GNUC_MINOR__
static int OOPS (void) __attribute__ ((__noreturn__));
#else
static int OOPS (void);
#endif

int scale (struct Cell_head *window, int panel, int margin, char *text)
{
    double inches;      /* horizontal inches of painted map */

    inches = do_scale (window, panel, margin, text);

/* compute the number of cols of output and effective ew resolution */
    window->cols = inches * Phres () + .5;
    window->ew_res = (window->east - window->west) / window->cols;

/* compute the number of rows of output and effective ns resolution */
    window->ns_res = window->ew_res * Phres() / Pvres() ;
    window->rows = (window->north - window->south) / window->ns_res ;

    return 0;
}

static double do_scale (struct Cell_head *window, int panel, int margin, char *text)
{
    char unit1[30];
    char unit2[30];
    char equals[30];
    char dummy[2];
    long n1,n2;
    double u1,u2 = 0.0L;

/*
* absolute horizontal width specification
*   x inches
*   x panels
* convert text to 1 : n
*/
    u1 = 0;
    *unit1 = 0;
    *dummy = 0;
    if (sscanf (text, "%lf %s %1s", &u1, unit1, dummy) == 2 && *dummy == 0)
    {
	u2 = -1;
	if (strncmp (unit1,"panel",5) == 0 && u1 > 0)
	    u2 = (panel-margin) * u1 / Phres () ;
	else if (strncmp (unit1,"inch",4) == 0 && u1 > 0)
	    u2 = u1 - margin/Phres() ;
	if (u2 > 0)
	{
	    sprintf (text, "1 : %.0f",
		METERS_TO_INCHES * distance(window->east,window->west) / u2);
	    return u2;
	}
    }

/*
* unitless ratio specification
*    n : m
*/
    *dummy = 0;
    n1 = n2 = 0;
    if (sscanf (text,"%ld : %ld%1s", &n1,&n2,dummy) == 2)
    {
	if (n1 <= 0 || n2 <= 0 || *dummy)
	    OOPS();
	sprintf (text, "%ld : %ld", n1, n2);
	return METERS_TO_INCHES * distance(window->east,window->west) * n1 / n2 ;
    }

/*
*
* ratio specification with unit conversions
*    x inches equals y miles
*    x inches equals y meters
*    x inches equals y kilometers
*/

    *unit1 = 0;
    *unit2 = 0;
    *equals = 0;
    n1 = n2 = 0;
    if (sscanf (text, "%ld %s %s %ld %s", &n1, unit1, equals, &n2, unit2) == 5)
    {
	if (n1 <= 0 || n2 <= 0)
	    OOPS();
	if (strcmp (equals, "=") != 0 && strncmp (equals,"equal",5) != 0)
	    OOPS();
/* unit1: inches */
	if (strncmp (unit1,"inch",4) == 0)
	    u1 = n1;
	else
	    OOPS();

/* unit2: meters, miles, kilometers */
	if (strncmp (unit2,"mile",4) == 0)
	{
	    u2 = MILES_TO_INCHES;
	    strcpy (unit2, "mile");
	}
	else if (strncmp (unit2,"meter",5) == 0)
	{
	    u2 = METERS_TO_INCHES ;
	    strcpy (unit2, "meter");
	}
	else if (strncmp (unit2, "kilometer", 9) == 0)
	{
	    u2 = METERS_TO_INCHES * 1000;
	    strcpy (unit2, "kilometer");
	}
	else
	    OOPS();
	u2 *= n2;

	strcpy (unit1, "inch");
	strcpy (equals, "equal");
	if (n1 == 1)
	    strcat (equals, "s");
	else
	    strcat (unit1,"es");

	if (n2 != 1)
	    strcat (unit2,"s");

	sprintf (text, "%ld %s %s %ld %s", n1, unit1, equals, n2, unit2);

	return METERS_TO_INCHES * distance(window->east,window->west) * u1 / u2 ;
    }
    OOPS();
}

static int OOPS (void)
{
    G_fatal_error ("Pmap: do_scale(): shouldn't happen");
    exit(1);
}

int unscaled (struct Cell_head *window, int rows, int cols)
{
    double ns, ew;

    ns = (window->ns_res * window->rows) / rows;
    ew = (window->ew_res * window->cols) / cols;

    if (ns > ew)
	ew = ns;
    else
	ns = ew;

    window->ns_res = ns;
    window->ew_res = ew;

    window->rows = (window->north - window->south) / ns;
    window->cols = (window->east  - window->west ) / ew;

    return 0;
}
