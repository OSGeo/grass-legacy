
#define FUDGE 30.
reset_color(number, red, grn, blu)
	int number ;
	unsigned char red, grn, blu ;
{
	int INDEX ;
	int _norm() ;

/* silently ignore numbers greater than max color */
	if (number > get_num_colors())
		return ;

/* normalize the RGB values each to the range FUDGE to 256
 * as the MASSCOMP shows low intensities much too weak
 */
	red = (unsigned char)((256. - FUDGE) * (float)red / 256. + FUDGE) ;
	grn = (unsigned char)((256. - FUDGE) * (float)grn / 256. + FUDGE) ;
	blu = (unsigned char)((256. - FUDGE) * (float)blu / 256. + FUDGE) ;

	INDEX = 
		(_norm((float)red/256.) << 20) +
		(_norm((float)grn/256.) << 12) +
		(_norm((float)blu/256.) <<  4) ;

	mgicm(number, INDEX) ;
	return 0 ;
}

_norm(intensity)
	float intensity ;
{
	return ( (unsigned int)(intensity * 15 +.5) & 0x00ff) ;
}
