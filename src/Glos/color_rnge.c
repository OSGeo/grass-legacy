/****************************************************************/
/*								*/
/*	decide_color_range.c	in	~/src/Glos		*/
/*								*/
/*	This function calculates the factor that should be	*/
/*	used to multiply every visible point's inclination	*/
/*	so that the full color range could be utilized. It 	*/
/*	takes as input the max inclination and returns the	*/
/*	color factor.						*/
/*								*/
/****************************************************************/


double decide_color_range(max_inclination,COLOR_SHIFT,COLOR_MAX)

        double max_inclination,COLOR_SHIFT,COLOR_MAX;
{
        int i;
 
        i = max_inclination + 0.99;
        return((COLOR_MAX - COLOR_SHIFT)/i);
}

/************* END OF FUNCTION "DECIDE_COLOR_FACTOR" ************/

