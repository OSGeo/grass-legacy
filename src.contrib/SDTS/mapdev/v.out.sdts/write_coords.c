/*
**
**  Written by David Stigberg Fall 92-Spring 93
**  US Army Construction Engineering Research Lab
**
*/

#include "Vect.h"
#include "defines.h"
#include "externs.h"
#include "stc123.h"
#include "globals2.h"
/*#include "sdts.h"*/

static int coors_printed = 0;

static unsigned char bit_y[4], bit_x[4];


write_LE_coords (fpout, num, Xptr, Yptr, zone, final)
    FILE *fpout;
    register int num;
    double *Xptr, *Yptr;
	int zone;
	int final;
{
    register int i;
	int option1 = 6;
	int option2 = 1;

    for (i = 0 ; i < num ; i++)
    {

		if ((i + 1) == num ) /*end of rec*/
		{
			if (final)
				option2 = 4;
			else
				option2 = 3;
		}

		if (!write_coords (fpout, Xptr[i], Yptr[i], zone, LEAD_ID, 
					  option1, option2, "SADR"))
					  return (0);
		option1 = 1;
    }

	return (1);
}

backup_and_write_coords (fpout, lead_id)
    FILE *fpout;
	int lead_id;
{
	 int bkstat;

/*backup final field of final record and rewrite with EOF option*/

     if (!bak123sfld (fpout, &bkstat)) 
     {
		put_err_mess ("Failed to backup 123 SADR subfield.", 0);
        return (0);
     } 

	 if (!wr123sfld (fpout, "SADR", lead_id, bit_y, 4, 4))
	 {
		put_err_mess ("Failed to re-write 123 SADR subfield after backing up.", 0);
        return (0);
	 }

	 return (1);
}



write_NA_coords (fpout, x, y, zone)
    FILE *fpout;
    double x, y;
	int zone;
{
	int option1 = 6; /*start of field*/
	int option2 = 5; /*end of field*/

	return (write_coords (fpout, x, y, zone, LEAD_ID_R, option1, option2, "SADR"));

}

write_NO_coords (fpout, x, y, zone, option2)
    FILE *fpout;
    double x, y;
	int zone;
	int option2;
{
	int option1 = 6; /*start of field*/

	return (write_coords (fpout, x, y, zone, LEAD_ID_R, option1, option2, "SADR"));

}

write_EP_coords (fpout, num, Xptr, Yptr, zone )
    FILE *fpout;
    register int num;
    double *Xptr, *Yptr;
	int zone;
{
	int option1 = 6; /*start of field*/
	int option2 = 5; /*end of field*/

	if (num != 2) {
		fprintf (stderr, "WARNING: DOT with %d sets of coords; should have 2\n", num); 
	}

	if (Xptr[0] != Xptr[1] || Yptr[0] != Yptr[1]) {
		 fprintf (stderr, "WARNING: DOT with non-identical XY pairs\n");
	}

	return (write_coords (fpout, Xptr[0], Yptr[0], zone, LEAD_ID_R, 
				  option1, option2, "SADR"));


}

write_coords (fpout, x, y, zone, lead_id, option1, option2, tag)
    FILE *fpout;
	double x, y;
	int zone;
	int option1, option2;
	char *tag;
{

#ifndef NO_CONVERSION

	int int_x, int_y;

    if (!U_to_ll)
	{
		fprintf (stderr, "whoops. can't process non-utm yet\n");
		return;
	}

	/*convert from UTM to LAT-LON*/
	utm_to_ll (x, y, &int_y, &int_x, zone);

    /*convert from int lat-lon to bitfields*/
	conv_bitfld ((int) int_x, bit_x);
	conv_bitfld ((int) int_y, bit_y);
#else
	/*apply export scaling factors*/
    x = ( x * Sfax_out); 
    y = ( y * Sfay_out); 

	conv_bitfld ((int) x, bit_x);
	conv_bitfld ((int) y, bit_y);

#endif /* NO_CONVERSION */

	if (!wr123sfld (fpout, tag, lead_id, bit_x, 4, option1))
	{
	   put_err_mess ("Failed to write 123 SADR field.", 0);
	   return (0);
	}
	if (!wr123sfld (fpout, tag, lead_id, bit_y, 4, option2))
	{
	   put_err_mess ("Failed to write 123 SADR field.", 0);
	   return (0);
	}

	/*
	wr123sfld (fpout, "SADR", lead_id, bit_x, 4, option1);
	wr123sfld (fpout, "SADR", lead_id, bit_y, 4, option2);
	*/

	coors_printed++;

	return (1);

}

conv_bitfld (i, b)
   int i;
   unsigned char *b;
{
    b[0] = (i >> 24) & 255; 
    b[1] = (i >> 16) & 255; 
    b[2] = (i >> 8) & 255; 
    b[3] = i & 255; 
}

