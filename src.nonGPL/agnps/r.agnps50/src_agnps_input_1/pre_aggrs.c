





/*       ===========================================================

         This function was created to circumvate the problem that
         the system would not reset the WIND setting until the 
         code exit, even the G_put_window function was used in 
         Srinivasan's code, because a child process cannot change
         the environmental variables of its parent. 
        
                   August, 1995
                       
                   Zhian Li,   Micheal A. Foster
                   Department of Entomology
                   501 ASI Building
                   The Pennsylvania State University 
                   University Park, Pa 16802 

         ===========================================================
*/
         

#include "agnps_input.h"
         
main()
{

	int	i, j, nrows, ncols, ct, land_use;
	int	clay1, silt1, sand1;
        int     user_fert_lvl;
	FILE	*fopen(), *fs;
        FILE    *fdlot, *nfdlot;
        FILE    *temp;
	float	rules_ch_side_slope();
	float	rules_man_n();
	float	rules_sur_cond();
	char	buf[512];
	double	L, S, LS;
	double 	m; /* exponential factor in LS factor equation */
	double	theta; /* angle of slope converted from % of slope */

	struct Categories landuse_cats, mgt_practice_cats;
	struct Categories hy_cond_cats;
	struct Categories machinery_cats, K_fac_cats, C_fac_cats;
	struct Categories clay_cats, sand_cats, channel_slope_cats;

        /* Added by Zhian Li July 1995 */
        /*******************************/
        int     cell_id;
        int     divcell;   /* Cell division number             */
                           /* This option should be added later*/ 
        int     rdivcell;  /* Receiving cell division number   */
                           /* This option should be added later*/ 
        char    line[80];
        int     pest;      /* Pesticide type                   */
        int     kflag;     /* Hydrograph calc. method flag     */
        int     it,it1;
        float   t;
        char    method[5]; /* Input method for channel and     */
                           /* fertilizer in specific cells     */
                           /*                                  */
                           /*    Map  = Map click location     */
                           /*    D    = Direct Input for a     */
                           /*           specific cell.         */
        
        char   cmd[80];    /* Auxiliary variable               */
        int    fert_lvl;   /* Auxiliary variable               */

      /* For nonfeedlot point sources parameters     */
    struct a1 {
        float    wdisch;        /* Water discharge rate; Cubic Feet/second */
        float    tn;            /* Total Nitrogen discharged from this non */
                                /* feed lot point source(ppm).             */
        float    tp;            /* Total Phosphorus discgared from this non*/
                                /* feed lot point source(ppm).             */
        float    tcod;          /* Total COD discarged from this non feed  */
                                /* lot point source(ppm).                  */
        int      ptsloc;        /* Location of the point source in the cell*/
                                /*       1 = top                           */
                                /*       0 = bottom                        */
        };
    struct a1 nfdltpts[9];
      
      /* End of nonfeedlot point source parameter block*/



      /* For feedlot point source parameters         */
    struct a2 {
        float    fdarea;        /* Feed lot area in acres                  */
        int      fcs;           /* Feed lot curve number                   */
        float    rfarea;        /* Feed lot roofed area in acres           */
        int      fln;           /* Feed lot nitrogen concentration in ppm  */
        int      flp;           /* Feed lot Phosphorus concentration in ppm*/
        int      flcod;         /* Feed lot COD  concentration in ppm      */
        /* for the tributary area (area 2 of feedlot)*/
        float    tb1a;          /* Area of subarea 1 of the tributary area */
        int      tb1cnt;        /* Curve number of subarea 1 of the        */
                                /*        tributary area.                  */               
        float    tb2a;          /* Area of subarea 2 of the tributary area */
        int      tb2cnt;        /* Curve number of subarea 2 of the        */
                                /*        tributary area.                  */               
        float    tb3a;          /* Area of subarea 3 of the tributary area */
        int      tb3cnt;        /* Curve number of subarea 3 of the        */
                                /*        tributary area.                  */               
        float    tb4a;          /* Area of subarea 4 of the tributary area */
        int      tb4cnt;        /* Curve number of subarea 4 of the        */
                                /*        tributary area.                  */               
        float    tb5a;          /* Area of subarea 5 of the tributary area */
        int      tb5cnt;        /* Curve number of subarea 5 of the        */
                                /*        tributary area.                  */               
        float    tb6a;          /* Area of subarea 6 of the tributary area */
        int      tb6cnt;        /* Curve number of subarea 6 of the        */
                                /*        tributary area.                  */               

       /* For the areas adjacent to the feedlot    */
        float    ad1a;          /* Area of subarea 1 adjacent to feedlot   */
        int      ad1cnt;        /* Curve number of subarea 1 adjacent to   */
                                /*        the feedlot.                     */

        float    ad2a;          /* Area of subarea 2 adjacent to feedlot   */
        int      ad2cnt;        /* Curve number of subarea 2 adjacent to   */
                                /*        the feedlot.                     */

        float    ad3a;          /* Area of subarea 3 adjacent to feedlot   */
        int      ad3cnt;        /* Curve number of subarea 3 adjacent to   */
                                /*        the feedlot.                     */

        float    ad4a;          /* Area of subarea 4 adjacent to feedlot   */
        int      ad4cnt;        /* Curve number of subarea 4 adjacent to   */
                                /*        the feedlot.                     */

        float    ad5a;          /* Area of subarea 5 adjacent to feedlot   */
        int      ad5cnt;        /* Curve number of subarea 5 adjacent to   */
                                /*        the feedlot.                     */

        float    ad6a;          /* Area of subarea 6 adjacent to feedlot   */
        int      ad6cnt;        /* Curve number of subarea 6 adjacent to   */
                                /*        the feedlot.                     */


        /* for the 3 buffer areas*/
        float    bf1slp;        /* Slope for buffer area 1.                */
        float    bf1scnt;       /* Buffer area surface constant            */
        int      bf1fll;        /* Flow length for buffer area 1           */
        float    bf2slp;        /* Slope for buffer area 2.                */
        float    bf2scnt;       /* Buffer area surface constant            */
        int      bf2fll;        /* Flow length for buffer area 2           */
        float    bf3slp;        /* Slope for buffer area 3.                */
        float    bf3scnt;       /* Buffer area surface constant            */
        int      bf3fll;        /* Flow length for buffer area 3           */
        /* for the animal entries*/
        int      nanimal1;      /* Number of animals of type 1             */
        float    animcod1;      /* animal COD factor for animal type 1     */
        float    animp1;        /* animal Phosphorus for animal type 1     */
        float    animn1;        /* animal Nitrogen for animal type 1       */
        int      nanimal2;      /* Number of animals of type 2             */
        float    animcod2;      /* animal COD factor for animal type 2     */
        float    animp2;        /* animal Phosphorus for animal type 2     */
        float    animn2;        /* animal Nitrogen for animal type 2       */
        int      nanimal3;      /* Number of animals of type 3             */
        float    animcod3;      /* animal COD factor for animal type 3     */
        float    animp3;        /* animal Phosphorus for animal type 3     */
        float    animn3;        /* animal Nitrogen for animal type 3       */
        }; 
      struct a2 fdltpts[9];

      /* End of feedlot point source parameter block */

      /* Define the channel parameters               */
      /*                                             */
      /* Note:                                       */
      /*                                             */
      /*   In the present version of this interface, */
      /*   it is assumed that there is no definitive */
      /*   channel in all cells. Therefore, the input*/ 
      /*   for channels in all of the cells are the  */
      /*   same.  Further modification to this       */
      /*   interface will allow the user to input the*/
      /*   parameters for each channel each cell. The*/
      /*   easiest way to do this is to turn the     */
      /*   following definitions into structure and  */
      /*   make a structure for each cell.           */ 
      /*                                             */
      /*               Zhian Li                      */  

        

      /*  Note:                                      */
      /*                                             */
      /*      The values for the following variables */ 
      /*    are for cases there is  no definitive    */
      /*    channel in the cells.  These parameters  */
      /*    will be available in the input interface */
      /*    in the future.                           */

        chnw       = 0.0;
        chnwco     = 3.425;
        chwexp     = 0.3151;
        chdepth    = 0.0;
        chdepco    = 0.4537;
        chdepexp   = 0.2192;
        chlenth    = 0.0;
        chlco      = 153.0;
        chlexp     = 0.6;
        chmanf     = 0.103;
        agdecay    = 1;
        ndecay     = 0;
        pdecay     = 0;
        coddecay   = 0;
        clyscour   = 1;
        sltscour   = 1;
        saggsur    = 1;
        laggsur    = 1;
        sandsur    = 1;


      /* For soil information                        */
        basen      = 0.0010;
        basep      = 0.0005;
        poren      = 5.0;
        porep      = 2.0;
        extractn   = 0.05;
        extractp   = 0.025;
        leachn     = 0.25;
        leachp     = 0.25;
        soilorg    = 20;

     /* For fertilizer information                   */ 
        fert1n     = 50;
        fert1p     = 20;
        fert2n     = 100;
        fert2p     = 40;
        fert3n     = 200;
        fert3p     = 80;
        fert4n     = 0;
        fert4p     = 0;
        fert_nfac  = 60;
        fert_pfac  = 60;
        user_fert_lvl = 4;

        watershd_input();

	G_gisinit("agnps_input");

	this_mapset = G_mapset();

	get_input_map_names();

	cell_num_id();
}
