
        /*---------------------------------------------------------*
         *               AGNPS/GRASS Interface Project             *
         *  Developed in the Agriculture Engineering Department    *
         *                at Purdue University                     *
         *                        by                               *
         *         Raghavan Srinivasan and Bernard Engel           *
         *                                                         *
         *   (c)Copyright, 1992 Purdue Research Foundation, West   *
         *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
         *   permission is granted, this material shall not be     *
         *   copied, reproduced or coded for reproduction by any   *
         *   electrical, mechanical or chemical processes,  or     *
         *   combinations thereof, now known or later developed.   *
         *---------------------------------------------------------*/

#include "agnps_input.h"


/*      July, 1991  Agricultural Engineering, Purdue University
        Raghavan Srinivasan (srin@ecn.purdue.edu)

        main()

        This is the main program to acquire the needed inputs from the user
        and process them for the format needed by the AGNPS model.
        Once the map names are gathered the processes are started to
        manipulate them and temp maps are created. Here by masking the
        wshd outline the data are gathered and put into AGNPS format.
*/

/*       ===========================================================

         This function has been modified significantly in order 
         to make it compatable with AGNPS 5.0.   New input items
         have been added for the items not in the old AGNPS input
         file.  Modifications have also made in function 
         get_wshd_input.c to allow new input items.   

                   June, 1995

                   Zhian Li,   Micheal A. Foster
                   Department of Entomology
                   501 ASI Building
                   The Pennsylvania State University 
                   University Park, Pa 16802 

         ===========================================================
*/

/*       ===========================================================
         Corrections were necessary to use the agnps_input_2 with
         LinuX. The import of K-factor and C-factor was not done in
         the correct way: The conversion of the category
         (string), containing the K-factor resp. the C-factor, into
         the float-format was incorrect (error-message: "K-factor not
         between 0-1"). So I made a correction of these conversion 
         commands.
         Other Changes: The K-factor was tested twice of it's correct
         value, the second test is changed now to the test of the 
         C-factor value (makes more sense...).

                  December 1996
                  Markus Neteler
                  Department of Geography
                  Institute of Physical Geography and Landscape-ecology
                  30167 Hannover, Germany
                  email: neteler@geog.uni-hannover.de
         ===========================================================
*/

/* New names for the modules:  Markus Neteler 15. March 1998

       renamed
         agnps_input -> r.agnps50.input
         agnps_view  -> r.agnps50.view
         run_agnps50 -> r.agnps50.run
             
*/
/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

/* maximum # of scenarios for pesticide data */
#define MAX_SCENARIO_NUM 5

/* holds data for a preplant scenario */
typedef struct
 { float days_since_app;
   float app_rate;
   int   app_eff;
   float inc_depth;
   int   inc_eff;
   float s_residue;
   float s_residue_half_life;
   float solubility;
   float carbon_sorption;
 } preplant;

/* holds data for a preemergence sceario */
typedef struct
 { float days_since_app;
   float app_rate;
   int   app_eff;
   float s_residue;
   float s_residue_half_life;
   float solubility;
   float carbon_sorption;
 } preemergence;

/* holds data for a postemergence scenario */
typedef struct
 { float days_since_app;
   float app_rate;
   int   app_eff;
   int   canopy_cover;
   float f_residue;
   float s_residue;
   float f_wash_threshold;
   int   f_wash_fraction;
   float f_residue_half_life;
   float s_residue_half_life;
   float solubility;
   float carbon_sorption;
 } postemergence;

/* holds information which may be either preplant, preemergence, or
   postemergence
*/
typedef union
 { preplant      pre_p;
   preemergence  pre_e;
   postemergence post_e;
 } pest_data;

/* holds data for one scenario

   fields:
      used	: a boolean value indicating whether this particular scenario
                  was found in the input mapset
      herbicide : the type of herbicide used;
                  1 = atrazine, 2 = alachlor, 3 = metalachlor
      apply_time: 1 = preplant, 2 = preemergence, 3 = postemergence
      data      : will hold data for preplant, preemergence, or postemergence,
                  depending on the value of the apply_time field
*/
typedef struct
 { int       used;
   int       herbicide;
   int       apply_time;
   pest_data data;
 } pest_scenario;

/* holds data on specific pesticides */
typedef struct
 { float res_half_life;   /* soil residue half life */
   float solubility;	  /* solubility in water */
   float carbon_sorption; /* organic carbon sorption */
 } pest_specific_data;

/* function prototypes */
void get_scs_tr55_defaults  ();
int  run_output_viewer      ();
int  do_another_storm       ();
void get_new_storm_data     ();
void get_user_pest_data     (pest_scenario *scenarios);
void get_pest_scenario_data (pest_scenario *sc, int sc_num);
void get_pre_p_data         (pest_scenario *sc, int sc_num);
void get_pre_e_data         (pest_scenario *sc, int sc_num);
void get_post_e_data        (pest_scenario *sc, int sc_num);
void to_upper_case          (char *s);
void make_name_string       (int which_pest, char *s);
void make_menu_name_string  (int which_pest, char *s);
char ch_to_upper            (char c);
int  is_whitespace          (char c);
int  approx_compare         (const char *s1, const char *s2);
void create_output_file     (int user_fert_lvl,
                             pest_scenario *pest_scenario_array,
                             int kflag);
/* added in for EI value calculation 7/96 */  
float EI_calc(char [], double, double);


/************************************************************/
/* start of section containing data for specific pesticides */
/************************************************************/

/* NOTE: All you have to do to add a new pesticide is add 1 to the current
         value of NUM_PEST, add the name of the pesticide to PEST_NAMES,
         add the pesticide's trade name to PEST_TRADE_NAMES, and add values
         for soil residue half life, water solubility, and organic carbon
         sorption to PEST_SPEC_DATA.  Of course, if the number of pesticides
         gets large, changes will need to be made to the display format used
         in the get_user_pest_data subroutine so that all of the pesticide
         names fit neatly on the screen.
*/

#define NUM_PEST 6
const char *PEST_NAMES[NUM_PEST] =
 { "Atrazine",
   "Alachlor",
   "Metolachlor",
   "Pendimethalin",
   "Diuron",
   "Simazine"
 };

const char *PEST_TRADE_NAMES[NUM_PEST] =
 { "Atratol",
   "Lasso",
   "Dual",
   "Prowl",
   "Karmex",
   "Aquazine or Princep"
 };

const pest_specific_data PEST_SPEC_DATA[NUM_PEST] = 
 { { 60.0, 33.0,  100.0  },
   { 15.0, 240.0, 170.0  },
   { 90.0, 530.0, 200.0  },
   { 90.0, 0.275, 5000.0 },
   { 90.0, 42.0,  480.0  },
   { 60.0, 6.2,   130.0  }
 };

/**********************************************************/
/* end of section containing data for specific pesticides */
/**********************************************************/

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

/* For nonfeedlot point sources parameters     */
struct a1
 { float wdisch; /* Water discharge rate; Cubic Feet/second */
   float tn;     /* Total Nitrogen discharged from this non */
                 /* feed lot point source(ppm).             */
   float tp;     /* Total Phosphorus discgared from this non*/
                 /* feed lot point source(ppm).             */
   float tcod;   /* Total COD discarged from this non feed  */
                 /* lot point source(ppm).                  */
   int   ptsloc; /* Location of the point source in the cell*/
                 /*       1 = top                           */
                 /*       0 = bottom                        */
 };

/* For feedlot point source parameters         */
struct a2
 { float    fdarea;        /* Feed lot area in acres                  */
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

main()
{

        int	i, j, nrows, ncols, ct, land_use;
        int	clay1, silt1, sand1;
        int     user_fert_lvl;
        /* FILE    *temp; line commented out by Dave Peterson, March 1996 */
        float	rules_ch_side_slope();
        float	rules_man_n();
        float	rules_sur_cond();
        char	buf[512], command[80];
        double	L, S, LS;
        double 	m; /* exponential factor in LS factor equation */
        double	theta; /* angle of slope converted from % of slope */

        /* added by Dave Peterson, February 1996 ...
           each item in the array represents a scenario which may be present
           in the map files; item 0 is not used, since 0 indicates no
           pesticides
        */
        pest_scenario pest_scenario_array[MAX_SCENARIO_NUM + 1];
        char          name_str[81];

        /* added by Dave Peterson, February 1996 ...
           variables will hold values to be written to output file as
           pesticide data
        */

        struct Categories landuse_cats, mgt_practice_cats;
        struct Categories hy_cond_cats;
        struct Categories machinery_cats, K_fac_cats, C_fac_cats;

        /* added by Dave Peterson, February 1996 ... */
        struct Categories pesticide_cats, nutrient_cats;

        struct Categories clay_cats, sand_cats, channel_slope_cats;

        /* Added by Zhian Li July 1995 */
        /*******************************/
        int     kflag;     /* Hydrograph calc. method flag     */
        char    method[5]; /* Input method for channel and     */
                           /* fertilizer in specific cells     */
                           /*                                  */
                           /*    Map  = Map click location     */
                           /*    D    = Direct Input for a     */
                           /*           specific cell.         */

        char   cmd[80];    /* Auxiliary variable               */

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

        chnw       = 0.2;
        chnwco     = 3.425;
        chwexp     = 0.3151;
        chdepth    = 0.1;
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




        /* line below commented out by Dave Peterson, March 1996 */
        /* temp = fopen("temp.out","w"); */

/*
        system("$GISBASE/etc/agnps50/pre_aggrs_input");
*/

        get_wshd_input();
        
        G_gisinit("r.agnps50.input");

        this_mapset = G_mapset();

        get_input_map_names();

        cell_num_id();

        slope_aspect();

        drain_num();

/*      This block of code was commented out by Dave Peterson, March 1996
        for(i=1; i<=tot_cells;i++)
         fprintf(temp,"After slope: i and cel[i].cell_num %i %i\n",i,cel[i].cell_num);


         for(i=1; i<=tot_cells;i++)
          fprintf(temp,"After DRAIN_num: i and cel[i].cell_num %i %i\n",i,cel[i].cell_num);
*/

/* check for any error in the aspect maps */
        chkdata();

       /* Get default channel data for the*/
       /* cells which have no definitive  */
       /* channels or are waterways.      */


           /* channel_dat(); commented out by Dave Peterson, April 1996 */

/*         This block of code was commented out by Dave Peterson, March 1996
           for(i=1; i<=tot_cells;i++)
            fprintf(temp,"After CHECK Data: i and cel[i].cell_num %i %i\n",i,cel[i].cell_num);
*/

        CN_hy_cond();

/* open the cell files created for reading (slope, aspect, rcell) */

        op_cel_fls();

/* get the category names and cell title */

        if (G_read_cats (machinery->p, machinery->mapset, &machinery_cats) < 0){
                clean_up();
                exit(-1);
                }

                /*
        if (channel_slope->flag == YES)
         { if (G_read_cats (channel_slope->p, channel_slope->mapset, &channel_slope_cats) < 0)
            clean_up();
           exit(-1);
         }
                */

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

        if (G_read_cats (nutrient->p, nutrient->mapset, &nutrient_cats) < 0)
         { clean_up();
           exit(-1);
         }

        if (pesticide -> p != NULL)
         { if (G_read_cats (pesticide -> p , pesticide -> mapset, 
                            &pesticide_cats) < 0)
            { clean_up();
              exit(-1);
            }
         }

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

        if (G_read_cats (landuse->p, landuse->mapset, &landuse_cats) < 0){
                clean_up();
                exit(-1);
                }
        if (G_read_cats (mgt_practice->p, mgt_practice->mapset, &mgt_practice_cats) < 0){
                clean_up();
                exit(-1);
                }
        if (G_read_cats (hy_cond->p, this_mapset, &hy_cond_cats) < 0){
                clean_up();
                exit(-1);
                }
        if (G_read_cats (C_fac->p, C_fac->mapset, &C_fac_cats) < 0){
                clean_up();
                exit(-1);
                }
        if (G_read_cats (K_fac->p, K_fac->mapset, &K_fac_cats) < 0){
                clean_up();
                exit(-1);
                }
        if (G_read_cats (sand->p, sand->mapset, &sand_cats) < 0){
                clean_up();
                exit(-1);
                }
        if (G_read_cats (clay->p, clay->mapset, &clay_cats) < 0){
                clean_up();
                exit(-1);
                }


        G_get_set_window (&orig_window);
        nrows = orig_window.rows;
        ncols = orig_window.cols;

/*      No cell division is allowed in this version.  However, 
        the cell sub division can be added in the future if
        necessary.
                                Zhian Li                     */

        /* changed by Dave Peterson, March 1996: approx_compare used instead
           of strncmp */
        if( (approx_compare(pkflcal,"AGNPS")) == 0) {
            hydro_mdl = 1;
            }
           else 
            {
            hydro_mdl = 0;
            }

        /* changed by Dave Peterson, March 1996: approx_compare used instead
           of strncmp */
        if( (approx_compare(gmorp,"Yes") == 0) ||
            (approx_compare(gmorp, "y") == 0)     )
            {
            geom_mod = 1;
            }
           else
            {
            geom_mod = 0;
            }

        /* changed by Dave Peterson, March 1996: approx_compare used instead
           of strncmp */
        if( (approx_compare(hydrosf,"K Coef") == 0) ||
            (approx_compare(hydrosf,"K Coef.") == 0) )
            {
            kflag = 1;
            }
           else
            {
            kflag = 0;
            }

/*      Define the storm type                       

        1.  Storm Type                   stmtp;
        2.  Storm Energy Intensity       ei;
        3.  Duration of storm            stmtm;
        4.  Storm Rainfall in inches     rainfall; 
        5.  Rainfall Nitrogen in ppm     stmnppm; 
*/         
        if( ei >= 0.0001) {
           stmtm = 0.0;
           }
        /* This block of code was commented out by Dave Peterson, March 1996
           for(i=1; i<=tot_cells;i++)
            fprintf(temp,"The values of i and cel[i].cell_num %i %i\n",i,cel[i].cell_num);
        */

/*      Begin process input data for each cell  */ 

        fprintf (stderr,"Create input for each cell\n");
        ct = 1;
        for(i = 0; i < nrows; i++) {

        G_get_map_row(wshd->fd,wshd->rbuf,i);
        G_get_map_row(temp_slope_map->fd,temp_slope_map->rbuf,i);
        G_get_map_row(temp_dir_map->fd,temp_dir_map->rbuf,i);
        G_get_map_row(temp_drain_map->fd,temp_drain_map->rbuf,i);
        G_get_map_row(nutrient->fd,nutrient->rbuf,i);

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

        if (pesticide -> p != NULL)
         G_get_map_row(pesticide -> fd, pesticide -> rbuf, i);

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

        G_get_map_row(machinery->fd,machinery->rbuf,i);
        if (channel_slope->flag == YES)
         G_get_map_row(channel_slope->fd,channel_slope->rbuf,i);
        G_get_map_row(landuse->fd,landuse->rbuf,i);
        G_get_map_row(mgt_practice->fd,mgt_practice->rbuf,i);
        G_get_map_row(K_fac->fd,K_fac->rbuf,i);
        G_get_map_row(C_fac->fd,C_fac->rbuf,i);
        G_get_map_row(sand->fd,sand->rbuf,i);
        G_get_map_row(clay->fd,clay->rbuf,i);
        G_get_map_row(hy_cond->fd,hy_cond->rbuf,i);
        G_get_map_row(temp_cn_map->fd,temp_cn_map->rbuf,i);

           for(j=0;j < ncols;j++) {
                if(wshd->rbuf[j] > 0){
                  land_use = assign_landuse(G_tolcase(G_get_cat(landuse->rbuf[j], &landuse_cats)));
                  
/* check for landuse for slope, also check for 0 slope */
                  if(land_use == water || land_use == marsh)
                     cel[ct].ovl_slope = 0.0;
                  else if (temp_slope_map->rbuf[j] == 0)
                     cel[ct].ovl_slope = 0.5;
                  else
                     cel[ct].ovl_slope = (float) temp_slope_map->rbuf[j]/10.0;

                  cel[ct].CN = temp_cn_map->rbuf[j];

/* assume an uniform land shape since after smoothing it would be uniform */
                  cel[ct].slp_shpe_fact = 1;

/* LS factor is estimated using I.D. Moore paper */
                  theta = (double)(atan((double)(cel[ct].ovl_slope/100.0)));
                  L = pow(((double)((cel[ct].acc+1)*grid_res)/22.13), 0.4); /* L = accumulation_cells * grid_res/22.13 to the power 0.4 */
                  S = pow((sin(theta)/0.0896),1.3); /* convert pecentage of slope to degrees and take sin of this then divide by 0.0896 and raise to the power 1.3 */
                  LS = L*S; /* LS factor */
/* convert from LS factor to field slope length using 537 manual */
                  if(cel[ct].ovl_slope < 1.0) m = 0.2;
                  else if(cel[ct].ovl_slope < 3.0) m = 0.3;
                  else if(cel[ct].ovl_slope < 4.5) m = 0.4;
                  else if(cel[ct].ovl_slope < 5.0) m = 0.5;

                  cel[ct].fld_slp_len = (int) (pow((LS/((double)((65.41*sin(theta)*sin(theta))+4.56*sin(theta)+0.065))),(1/m))*72.6);

                  if(cel[ct].fld_slp_len > (int) (grid_res*3.28)) cel[ct].fld_slp_len = (int) (grid_res*3.28);

/* After the discussion with Dr. Engel, the slope length factor normally will
not vary more than 400 feet, so limit of 400 feet is imposed if the estimated
slope length exceeds */
                  if(cel[ct].fld_slp_len > 400) cel[ct].fld_slp_len = 400;
                  if(cel[ct].fld_slp_len == 0) cel[ct].fld_slp_len = 1;

/******************************************************************/
/* start of code section modified by Dave Peterson, February 1996 */
/******************************************************************/

/* old code ... */
/*                cel[ct].fert_level = nutrient->rbuf[j]; */

/* new code ... */

                  cel[ct].fert_level = (int) atoi(G_get_cat(nutrient -> rbuf[j],
                                                  &nutrient_cats));

/****************************************************************/
/* end of code section modified by Dave Peterson, February 1996 */
/****************************************************************/

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

                  if (pesticide -> p != NULL)
                   cel[ct].pest_level = (int) atoi(G_get_cat(pesticide -> rbuf[j],
                                                   &pesticide_cats));

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

                  cel[ct].aspect = temp_dir_map->rbuf[j];

/* assume the P-factor as 1.0 which is the worse case and for row crops */
                  cel[ct].P_fac = 1.0;

/* get USLE C and K factors from respective maps */     

/**************************************************************/
/* begin of code section: conversion of K-factor, corrected by*/ 
/* Markus Neteler December 1996                               */
/* This correction was necessary for LinuX                    */
/**************************************************************/

/* ***************  Original code :***************************/
/*     cel[ct].K_fac = (float) atof(G_get_cat(K_fac->rbuf[j],&K_fac_cats));*/
/* ***************  End of original code *********************/ 

 sscanf(G_get_cat(K_fac->rbuf[j],&K_fac_cats),"%f",&(cel[ct].K_fac));

/* end of code section: conversion of K-factor ****************/


/* check for USLE K value between 0-1 else report error and exit */
                if(cel[ct].K_fac < 0.0 || cel[ct].K_fac >1.0){
                     sprintf(buf, "USLE K factor for soil category number [%d] not in allowable range of 0-1\n",K_fac->rbuf[j]);
                     G_fatal_error(buf);
                     clean_up();
                     exit(1);
                     }


/**************************************************************/
/* begin of code section: conversion of C-factor, corrected by*/ 
/* Markus Neteler December 1996                               */
/* This correction was necessary for LinuX                    */
/**************************************************************/
/* ***************  Original code :****************************/
/*                cel[ct].C_fac = (float) atof(G_get_cat(C_fac->rbuf[j],&C_fac_cats)); */
/* ***************  End of original code *********************/ 

 sscanf(G_get_cat(C_fac->rbuf[j],&C_fac_cats),"%f",&(cel[ct].C_fac));

/* end of code section: conversion of C-factor ****************/


/***************************************************************/
/* begin of code section: check of C-factor-value, corrected by*/ 
/* Markus Neteler December 1996                                */
/* This check of value should be done with C-factor, not again */
/* with the K-factor!                                          */
 /**************************************************************/
/* ***************  Original code :****************************/
/* check for USLE K value between 0-1 else report error and exit */
/*                if(cel[ct].K_fac < 0.0 || cel[ct].K_fac >1.0){ */
/*                        sprintf(buf, "USLE K factor for soil category number [%d] not in allowable range of 0-1\n",K_fac->rbuf[j]);           */
/*                        G_fatal_error(buf);                 */
/*                        clean_up();                         */
/*                        exit(1);                            */
/*                        }                                   */
/* ***************  End of original code **********************/ 
/* check for USLE C value between 0-1 else report error and exit */
                if(cel[ct].C_fac < 0.0 || cel[ct].C_fac >1.0){
                        sprintf(buf, "USLE C factor for soil category number [%d] not in allowable range of 0-1\n",C_fac->rbuf[j]);
                        G_fatal_error(buf);
                        clean_up();
                        exit(1);
                        }
/* end of code section: check of C-factor-value  ****************/



/* assume pt_src, gully_src, impd_fac and chl_ind are not exist, so assign 0 */


/* Add the point source input option             */
/* by commenting out the the next line.          */
/*                           Zhian Li, June, 1995*/
/*
                  cel[ct].pt_src = 0;
*/
                  cel[ct].gully_src = 0;
                  cel[ct].impd_fac = 0;

/* get clay, sand, silt values */
                  clay1 = atoi(G_get_cat(clay->rbuf[j],&clay_cats));
                  sand1 = atoi(G_get_cat(sand->rbuf[j],&sand_cats));
                  silt1 = 100 - (clay1+sand1);

/* assume 50% of overland slope as channel slope if the channel
   slope map is not exsist */
/*
                  if (channel_slope->flag == YES){
                        cel[ct].chl_slope = (float) channel_slope->rbuf[j]/10;
                        if(cel[ct].chl_slope > 0.0) cel[ct].chl_indicator = 1;
                        if (cel[ct].chl_slope == 0.0) cel[ct].ch_side_slope = 0.0;
                        else cel[ct].ch_side_slope = rules_ch_side_slope(clay1, silt1, sand1, land_use);
                        }
                else {
                        cel[ct].chl_slope = (float) 0.5 * cel[ct].ovl_slope;
                        cel[ct].chl_indicator = 0;
                        cel[ct].ch_side_slope = rules_ch_side_slope(clay1, silt1,sand1, land_use);
                        }
*/

/* This section of the code determines the type of channel */
/* in a cell.  However, the definition of channel type has */
/* changed in the new AGNPS model.  Therefore, the channel */
/* indicator must be changed accordingly.  The following   */
/* section of code serves this purpose to replace the old  */
/* code.                      Zhian Li,  July, 1995        */

                  cel[ct].chl_indicator = 1;
                  if (channel_slope->flag == YES){
                        cel[ct].chl_slope = (float) channel_slope->rbuf[j]/10;
                        if(cel[ct].chl_slope > 0.0) cel[ct].chl_indicator = 2;
                        if (cel[ct].chl_slope == 0.0) cel[ct].ch_side_slope = 0.0;
                        else cel[ct].ch_side_slope = rules_ch_side_slope(clay1, silt1, sand1, land_use);
                        }
                else {
                        cel[ct].chl_slope = (float) 0.5 * cel[ct].ovl_slope;
                        cel[ct].chl_indicator = 1;
                        cel[ct].ch_side_slope = rules_ch_side_slope(clay1, silt1, sand1, land_use);
                        }

/*  End of modification.                                   */
/*                                                         */

                  cel[ct].COD_fac = rules_cod(land_use);
                  cel[ct].incor_level = rules_fert_aval(G_tolcase(G_get_cat(machinery->rbuf[j], &machinery_cats)),land_use);
                  cel[ct].man_n = rules_man_n(land_use);
                  cel[ct].texture = rules_soil_texture(clay1,silt1,sand1);
                  cel[ct].sur_const = rules_sur_cond(land_use, G_tolcase(G_get_cat(mgt_practice->rbuf[j], &mgt_practice_cats)),G_tolcase(G_get_cat(hy_cond->rbuf[j], &hy_cond_cats)));
                  ct++;
                  }
              }
         }


         G_close_cell(temp_drain_map->fd);
         G_close_cell(temp_slope_map->fd);
         G_close_cell(temp_dir_map->fd);
         G_close_cell(nutrient->fd);

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

         if (pesticide -> p != NULL)
          G_close_cell(pesticide -> fd);

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

         if (channel_slope->flag == YES)
                G_close_cell(channel_slope->fd);
         G_close_cell(landuse->fd);
         G_close_cell(mgt_practice->fd);
         G_close_cell(machinery->fd);
         G_close_cell(K_fac->fd);
         G_close_cell(C_fac->fd);
         G_close_cell(hy_cond->fd);
         G_close_cell(wshd->fd);

/************************************************************/
/* start of code section added by Dave Peterson, April 1996 */
/************************************************************/

           if (hydro_mdl == 0 && geom_mod == 0)
            get_scs_tr55_defaults();

           strcpy(yesno, "y");
           V_clear();
           V_line(2, "Do you want to enter feedlot, channel, or nutrient");
           V_line(3, "information specific to selected cells? (y/n)");
           V_line(4, " (if no, the program will build an input");
           V_line(5, " file based solely on the maps)");

           V_ques(yesno,'s',3,55,3);
           V_intrpt_ok();
           if(!V_call()) exit(1);
           V_clear();

/**********************************************************/
/* end of code section added by Dave Peterson, April 1996 */
/**********************************************************/

           if( (approx_compare(yesno,"y") == 0) ||
               (approx_compare(yesno,"yes") == 0)  )
            { tptsrc = 0;
              mfdlt  = 0;
              mnfdlt = 0;
              strcpy(yesno, "n");
              V_clear();
              V_line(2, "Point Source or Feedlot");
              V_line(3, "=======================");
              V_line(5, "Any point sources or feedlots in the watershed? (y/n)");

              V_ques(yesno,'s',5,60,3);
              V_intrpt_ok();
              if(!V_call()) exit(1);
              V_clear();
              ptsrc_flag=0; /*Initialize point source flag. MAF Sept. 3, 1995*/
              if( (approx_compare(yesno,"y") == 0) ||
                  (approx_compare(yesno,"yes") == 0)  )
               { ptsrc_flag=1; /*MAF Sept. 3, 1995*/
                 /*  system("rm fdlot.dat");  commented out 7/96  */
                 /*  system("rm nfdlot.dat"); for display improvements */
                 sprintf(cmd,"$GISBASE/etc/agnps50/display_cell_map.sh %i\n",grid_res);
                 if(grid_res > 0)
                  system(cmd);
                 else
                  { fprintf (stderr,"ERROR: The map resolution must be greater than 0\n");
                    fprintf (stderr,"       Your current set is %i \n",grid_res);
                    exit(0);
                  }
                 fprintf(stderr,"Please resize the GRASS monitor\n");
                 fprintf(stderr,"so that each cell can be clearly identified\n");
                 fprintf(stderr,"hit return when done\n");
                 getchar();
                 system(cmd);
                 ptsrc_input();
               }
/******************************/
/** End additions by Zhian Li */

                         /* These variables will be defined */
                         /* for each base cell in the future*/
                         /* version of this grass/agnps.    */
                         /*                                 */
                         /*         Zhian Li                */ 


      /*  Prepare input for channel data                    */
      /*  The user will have two input methods              */

      /*          1.  Input from a selected Map location    */
      /*                                                    */
      /*          2.  Direct input for a specific cell      */
      /*                                                    */
              strcpy(method,"M");

              V_clear();
              V_line(2,  "Any channel data input? (y/n) ");
              V_line(4,  "Input method? (M/D)              ");
              V_line(5,  "    M = click on map for location   ");
              V_line(6,  "        (select M only if graphics monitor is running)");
              V_line(7,  "    D = enter the cell number directly");
              V_line(9,  "  Note:");
              V_line(10, "     Channel data must be entered for all cells");
              V_line(11, "     except those with no definitive channel or");
              V_line(12, "     those that are water.");

              V_ques(yesno,'s',2,60,3);
              V_ques(method,'s',4,60,3);
              V_intrpt_ok();
              if(!V_call()) exit(1);
              V_clear();

              if( (approx_compare(yesno,"y") == 0) ||
                  (approx_compare(yesno,"yes") == 0)  )
               { if( approx_compare(method,"m") == 0)
                  { sprintf(cmd,"$GISBASE/etc/agnps50/display_cell_map.sh %i\n",grid_res);
                    if(grid_res > 0)
                     system(cmd);
                    else
                     { fprintf (stderr,"ERROR: The map resolution must be greater than 0\n");
                       fprintf (stderr,"       Your current set is %i \n",grid_res);
                       exit(0);
                     }
                    fprintf(stderr,"Please resize the GRASS monitor\n");
                    fprintf(stderr,"so that each cell can be clearly identified\n");
                    fprintf(stderr,"hit return when done\n");
                    getchar();
                    system(cmd);
                    mchl_input();
                  }
                 else dchl_input();
               }

      /*  Prepare input for fertilizer data                 */
      /*  The user will have two input methods              */

      /*          1.  Input from a selected Map location    */
      /*                                                    */
      /*          2.  Direct input for a specific cell      */
      /*                                                    */

              V_clear();
              V_line(2,  "Would you like to edit fertilizer input? (y/n) ");
              V_line(4,  "Input method? (M/D)              ");
              V_line(5,  "    M = click on map for location   ");
              V_line(6,  "        (select M only if graphics monitor is running)");
              V_line(7,  "    D = enter the cell number directly");
              V_line(9,  "  Note:");
              V_line(10, "     Fertilizer levels entered here will write over");
              V_line(11, "     the data extracted from the GIS maps.");
              V_ques(yesno,'s',2,60,3);
              V_ques(method,'s',4,60,3);
              V_intrpt_ok();
              if(!V_call()) exit(1);
              V_clear();

              if( (approx_compare(yesno,"y") == 0) ||
                  (approx_compare(yesno,"yes") == 0)  )
               { if (approx_compare(method,"m") == 0)
                  { sprintf(cmd,"$GISBASE/etc/agnps50/display_cell_map.sh %i\n",grid_res);
                    if(grid_res > 0)
                     system(cmd);
                    else
                     { fprintf (stderr,"ERROR: The map resolution must be greater than 0\n");
                       fprintf (stderr,"       Your current set is %i \n",grid_res);
                       exit(0);
                     }
                    fprintf(stderr,"Please resize the Grass monitor\n");
                    fprintf(stderr,"so that each cell can be clearly identified\n");
                    fprintf(stderr,"hit return when done\n");
                    getchar();
                    system(cmd);
                    mfert_input();
                  }
                 else dfert_input();
               }
            }

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

         if (pesticide -> p != NULL)
          get_user_pest_data(pest_scenario_array);

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

         /* NOTE: the contents of this subroutine were previously all part of */
         /*       the main() function; subroutine created by Dave Peterson,   */
         /*       March 1996                                                  */
         create_output_file(user_fert_lvl, pest_scenario_array, kflag);

         /* fclose(temp); commented out by Dave Peterson, March 1996 */

         strcpy(yesno, "n");
         V_clear();
         V_line(2,"    Do you want to make an AGNPS model run? (y/n)");
         V_ques(yesno,'s',2,60,3);
         V_intrpt_ok();
         if(!V_call()) exit(1);
         V_clear();

        if ((approx_compare(yesno, "yes") == 0) ||
            (approx_compare(yesno, "y") == 0)      )
         { strcpy(yesno, "n");
           V_clear();
           V_line(2,"Do you want to create a GIS formatted output file? (y/n)");
           V_ques(yesno,'s',2,60,3);
           V_intrpt_ok();
           if(!V_call()) exit(1);
           V_clear();

           for (; ; )
            { strcpy(name_str, "r.agnps50.run ");
              strcat(name_str, in_fl_name);
              strcat(name_str, ((approx_compare(yesno, "yes") == 0) ||
                                (approx_compare(yesno, "y") == 0)      ) ?
                               " 1 " : " 0 ");
              strcat(name_str, "1 0");

              fprintf (stderr,"Running AGNPS model on file %s ...", in_fl_name);
              if (system(name_str))
               { fprintf (stderr,
"\nError running AGNPS model: process returned nonzero exit code.\n");
                 break;
               }
              else
               { fprintf (stderr,"\nAGNPS model finished running, press ENTER:");
                 getchar();
               }

              if (run_output_viewer())
               system("r.agnps50.view");

              if (!do_another_storm()) break;

              get_new_storm_data();
              create_output_file(user_fert_lvl, pest_scenario_array, kflag);
            }
         }

        clean_up();

        system("rm cell_id.out");
        fprintf (stderr,"Done\n");

        /* start of code section added by Dave Peterson, April 1996 */
        sprintf(command, "rm -f %s", landuse->p);
        system(command); /* remove temp landuse map created in subroutine */
                         /* CN_hy_cond                                    */
        /* end of code section added by Dave Peterson, April 1996 */

/*
        system("$GISBASE/etc/agnps50/close_grass_window.sh");
        system("$GISBASE/etc/agnps50/remove_tmp_file.sh");
*/
}

void get_scs_tr55_defaults()
 { V_clear();
   V_line(2, "Defaults for SCS-TR55 Nongeomorphic Option (for no definitive channel)");
   V_line(4, "Channel width at cell outlet (ft.)");
   V_line(5, "Channel depth at cell outlet (ft.)");
   V_line(7, "These defaults will be used for all cells except those");
   V_line(8, "for which data is entered in the channel input section");
   V_ques(&chnw, 'f', 4, 40, 8);
   V_ques(&chdepth, 'f', 5, 40, 8);
   V_intrpt_ok();
   if(!V_call()) exit(1);
   V_clear();
 }

/* see if user wants to run the output viewer */
int run_output_viewer()
 { char answer[4];

   strcpy(answer, "n");

   V_clear();
   V_line(2,
   "Would you like to view the output file using the output viewer? (y/n)");
   V_line(4, 
   "Note: After viewing output, you can return and simulate additional storms.");
   V_ques(answer, 's', 2, 71, 3);
   V_intrpt_ok();
   if(!V_call()) exit(1);
   V_clear();

   return ((approx_compare(answer, "yes") == 0) ||
           (approx_compare(answer, "y") == 0)      );
 }

/* see if user wants to simulate another storm; return 1 for yes or 0 for no */
int do_another_storm()
 { char answer[4];

   strcpy(answer, "n");

   V_clear();
   V_line(2,"Would you like to simulate another storm? (y/n)");
   V_ques(answer, 's', 2, 60, 3);
   V_intrpt_ok();
   if(!V_call()) exit(1);
   V_clear();

   return ((approx_compare(answer, "yes") == 0) ||
           (approx_compare(answer, "y") == 0)      );   
 }

/* get data for new storm from the user */
void get_new_storm_data()
 { in_fl_name[0] = 0;

   V_clear();
   V_line(1, "Data for New Storm:");
   V_line(3, "1. New filename (15 chars max.)");
   V_line(4, "2. Rainfall amount in inches"); 
   V_line(5, "3. Energy intensity value in English units");
   V_line(6, "4. Storm type (e.g. I, II, III)");
   V_line(7, "5. Storm duration (hours)"); 

   V_ques(in_fl_name, 's', 3, 45, 15);
   V_ques(&rainfall, 'f', 4, 45, 5);
   V_ques(&ei, 'f', 5, 45, 5);
   V_ques(stmtp, 's', 6, 45, 3);
   V_ques(&stmtm, 'f', 7, 45, 5);
   if(!V_call()) exit(1);
 
 /*section added in to calculate the EI value if user inputs 0,
   added 7/96 */
   if(ei==0) {   
      ei=EI_calc(stmtp,rainfall,stmtm);
     }
 }

void create_output_file(int user_fert_lvl, pest_scenario *pest_scenario_array,
                        int kflag)
 { FILE          *fs, *fdlot, *nfdlot;
   int           i, j, cell_id;
   char          name_str[81], line[81];
   preplant	 *p1;
   preemergence  *p2;
   postemergence *p3;
   int           pest;     /* Pesticide type */
   int           divcell;  /* Cell division number             */
                           /* This option should be added later*/ 
   int           rdivcell; /* Receiving cell division number   */
                           /* This option should be added later*/ 
   int           fert_lvl; /* Auxiliary variable               */
   int           value1, value4, value5, value9, value14;
   float         value2, value3, value6, value7, value8, value10, value11,
                 value12, value13, value15;
   struct a1     nfdltpts[9];
   struct a2     fdltpts[9];

   fprintf (stderr,"Creating AGNPS input file %s ...\n", in_fl_name);

   if ((fs = fopen (in_fl_name,"w")) == NULL)
    { fprintf (stderr,"Input file for AGNPS model can't open\n");
      clean_up();
      exit();
    }

/* write project description and other wshd parameters in the data file */	

/*      Additions by Zhian Li July 1995 

        The input format has been significantly changed in 
        version 5.0 of AGNPS.   Therefore, the fprintf statements
        in the following section have been dramatically changed.


            1.  A new line for version identification has been added;

            2.  A new line for run flags has been added;

            3.  A new line of Watershed name has been added;

            4.  Fertilizer input option has been added for
                  fertilizer entries;
*/  

/*      The old format    
   fprintf(fs,"%s\n%4.1f%4d%6.1f%6.1f%6d\n",wshd_des,cell_area,tot_cells,rainfall,ei,1);
*/

   divcell  = 0;
   rdivcell = 0;

/*      New Format    */
   fprintf(fs,"AGNPS SCS-TR55 format 5.00x\n");
   fprintf(fs, " %7i %7i %7i %7i %7i %7i %7i %7i\n", 0, 1, 0, 0, 0, 0, 0, 0);
   fprintf(fs,"%s\n",wshd_name);
   fprintf(fs,"%s\n",wshd_des);

/*      The following varaibles are printed in the next two lines of code 
        The order of the variables and their definitions are given as the
        following:

        1.  Cell size;
        2.  Number of base cells
        3.  Number of total cells  
        4.  Method of peak flow calculation  0 = TR55
                                             1 = CREAMS
        5.  Geomorphic Calculation flag      0 = no
                                             1 = yes 
        6.  Hydrograph calculation method    0 = use prepeak fraction
                                             1 = use k-coef. read from 
                                                 the next item
        7.  Value of k coefficient  
*/

   fprintf(fs,"%16.2f%8i%8i%8i",cell_area,tot_cells,tot_cells,hydro_mdl);
   fprintf(fs,"%8i%8i%8.2f\n",geom_mod,kflag,kfact);
   fprintf(fs,"%16s%8.2f%8.1f%8.2f%8.2f\n",stmtp,ei,stmtm,rainfall,stmnppm);

   for(i=1; i <= tot_cells; i++)
    { /* commented out by Dave Peterson, March 1996
      fprintf(temp,"The values of i and cel[i].cell_num %i %i\n",i,cel[i].cell_num);
      */
/*   
*********The following section of code prepare 
*********the old AGNPS input file

      fprintf(fs,"%7d%7d%4d%5.1f%2d%4d%5.1f%5.1f%5.3f%4.2f%4.2f%5.2f%4.2f%2d%2d%2d%4d%2d%4d%4d%3d%2d\n",
              cel[i].cell_num*1000,cel[i].rcell_num*1000,cel[i].CN,
              cel[i].ovl_slope,cel[i].slp_shpe_fact, cel[i].fld_slp_len,
              cel[i].chl_slope,cel[i].ch_side_slope, cel[i].man_n,
              cel[i].K_fac,cel[i].C_fac, cel[i].P_fac,
              cel[i].sur_const,cel[i].aspect, cel[i].texture,
              cel[i].fert_level,cel[i].incor_level, cel[i].pt_src,
              cel[i].gully_src,cel[i].COD_fac, cel[i].impd_fac,
              cel[i].chl_indicator);

*********End of comment
*/

      /*  The following section of code was created */
      /*  by Zhian Li for AGNPS 5.0 input format..  */

      /*  Open point source files and read in the point */
      /*  sources for each cell                         */

      tptsrc = 0;
      mfdlt  = 0;
      mnfdlt = 0;
/*
      cel[1].chl_indicator=2;
*/

/*   
      CAN'T USE THIS APPROACH ANYMORE SINCE yesno FUNCTION WAS JUST ASSIGNED
      BY FERTILIZER INPUT REQUEST.  MIKE FOSTER SEPT. 3, 1995 
      if((strncmp(yesno,"Y",1)) == NULL ||    
         (strncmp(yesno,"y",1)) == NULL)
*/
         /* INSTEAD, CHECK THE PTSRC_FLAG */

      if (ptsrc_flag)
       { /* fprintf (stderr,"check the point sources\n"); */

         if((fdlot = fopen("fdlot.dat","r")) == NULL)
          { fprintf(stderr,"Cannot open feedlot data file\n");
            fprintf(stderr,"for read in preparing cell input\n");
            exit(0);
          }

         if((nfdlot = fopen("nfdlot.dat","r")) == NULL)
          { fprintf(stderr,"Cannot open nfeedlot data file\n");
            fprintf(stderr,"for read in preparing cell input\n");
            exit(0);
          }

         /*  For nonfeedlot point source         */
         j = 0;
         for(; ; )
          { if((fgets(line,80,nfdlot)) == NULL)
             { rewind(nfdlot);
               break;
             }
            sscanf(line,"%i\n",&cell_id);
            fgets(line,80,nfdlot);
            if(cell_id == i )
             { sscanf(line,"%f%f%f%f%i\n",
                      &nfdltpts[j].wdisch,&nfdltpts[j].tn,&nfdltpts[j].tp,
                      &nfdltpts[j].tcod,&nfdltpts[j].ptsloc);
               mnfdlt = mnfdlt + 1;
               j++;
             }
            if(mnfdlt >= 9 )
             { rewind(nfdlot);
               break;
             }
          }

         /*  For Feedlot point source            */

         tptsrc = mnfdlt;

         j = 0;
         mfdlt = 0;
         for(; ; )
          { if (fgets(line,80,fdlot) == NULL)
             { rewind(fdlot);
               break;
             }
            sscanf(line,"%i \n",&cell_id);
/*
            fprintf (stderr,"The cell ID of FeedLot %i\n",cell_id);
*/
            if(cell_id == i )
             { fgets(line,80,fdlot);
               sscanf(line,"%f%i%f%i%i%i\n",
                      &fdltpts[j].fdarea,&fdltpts[j].fcs,&fdltpts[j].rfarea,
                      &fdltpts[j].fln,&fdltpts[j].flp,&fdltpts[j].flcod);
               /* Tributary and adjacent areas      */
               fgets(line,80,fdlot);
               sscanf(line,"%f%i%f%i\n",&fdltpts[j].tb1a,
                      &fdltpts[j].tb1cnt,&fdltpts[j].ad1a,&fdltpts[j].ad1cnt);
               fgets(line,80,fdlot);
               sscanf(line,"%f%i%f%i\n",&fdltpts[j].tb2a,
                      &fdltpts[j].tb2cnt,&fdltpts[j].ad2a,&fdltpts[j].ad2cnt);
               fgets(line,80,fdlot);
               sscanf(line,"%f%i%f%i\n",&fdltpts[j].tb3a,
                      &fdltpts[j].tb3cnt,&fdltpts[j].ad3a,&fdltpts[j].ad3cnt);
               fgets(line,80,fdlot);
               sscanf(line,"%f%i%f%i\n",&fdltpts[j].tb4a,
                      &fdltpts[j].tb4cnt,&fdltpts[j].ad4a,&fdltpts[j].ad4cnt);
               fgets(line,80,fdlot);
               sscanf(line,"%f%i%f%i\n",&fdltpts[j].tb5a,
                      &fdltpts[j].tb5cnt,&fdltpts[j].ad5a,&fdltpts[j].ad5cnt);
               fgets(line,80,fdlot);
               sscanf(line,"%f%i%f%i\n",&fdltpts[j].tb6a,
                      &fdltpts[j].tb6cnt,&fdltpts[j].ad6a,&fdltpts[j].ad6cnt);

               /* Buffer areas                     */
               fgets(line,80,fdlot);
               sscanf(line,"%f%f%i\n",
                      &fdltpts[j].bf1slp,&fdltpts[j].bf1scnt,&fdltpts[j].bf1fll);
               fgets(line,80,fdlot);
               sscanf(line,"%f%f%i\n",
                      &fdltpts[j].bf2slp,&fdltpts[j].bf2scnt,&fdltpts[j].bf2fll);
               fgets(line,80,fdlot);
               sscanf(line,"%f%f%i\n",
                      &fdltpts[j].bf3slp,&fdltpts[j].bf3scnt,&fdltpts[j].bf3fll);
               /* Animal descriptions              */
               fgets(line,80,fdlot);
               sscanf(line,"%i%f%f%f\n",&fdltpts[j].nanimal1,
                      &fdltpts[j].animcod1,&fdltpts[j].animp1,&fdltpts[j].animn1);
               fgets(line,80,fdlot);
               sscanf(line,"%i%f%f%f\n",&fdltpts[j].nanimal2,
                      &fdltpts[j].animcod2,&fdltpts[j].animp2,&fdltpts[j].animn2);
               fgets(line,80,fdlot);
               sscanf(line,"%i%f%f%f\n",&fdltpts[j].nanimal3,
                      &fdltpts[j].animcod3,&fdltpts[j].animp3,&fdltpts[j].animn3);
               j++;
               mfdlt = mfdlt + 1;
               tptsrc = tptsrc + 1;
             }
            else
             { fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               /* for the tributary and adjacent areas   */
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
               fgets(line,80,fdlot);
             }
            if(tptsrc >= 9 )
             { rewind(fdlot);
               break;
             }
          }

         fclose(fdlot);
         fclose(nfdlot);
       }

      /*    Test test  */
      /* commented out by Dave Peterson, March 1996
      fprintf(temp,"The values of i and cel[i].cell_num %i %i\n",i,cel[i].cell_num);
      */
      fprintf(fs,"%8i%8i%8i%8i%8i%8i%8.1f%8i\n",
              cel[i].cell_num,divcell,cel[i].rcell_num,rdivcell,
              cel[i].aspect,cel[i].CN,cel[i].ovl_slope,
              cel[i].slp_shpe_fact);
      fprintf(fs,"%16i%8.3f%8.2f%8.4f%8.2f%8.2f%8i\n",
              cel[i].fld_slp_len,cel[i].man_n,cel[i].K_fac,cel[i].C_fac,
              cel[i].P_fac,cel[i].sur_const,cel[i].COD_fac); 
      cel[i].impd_fac = 0;

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

      pest = (pesticide -> p == NULL) ? 0 : cel[i].pest_level;

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

      switch (cel[i].fert_level)
       { case 4 : fprintf(fs,"%16i%8i%8i%8i%8i%8i%8i\n",
                  cel[i].texture,user_fert_lvl,pest,tptsrc,
                  cel[i].gully_src,cel[i].impd_fac,cel[i].chl_indicator);
                  break;
         case 5 : fert_lvl = 4;
                  fprintf(fs,"%16i%8i%8i%8i%8i%8i%8i\n",
                          cel[i].texture,fert_lvl,pest,tptsrc,
                          cel[i].gully_src,cel[i].impd_fac,cel[i].chl_indicator);
                  break;
         default: fprintf(fs,"%16i%8i%8i%8i%8i%8i%8i\n",
                          cel[i].texture,cel[i].fert_level,pest,tptsrc,
                          cel[i].gully_src,cel[i].impd_fac,cel[i].chl_indicator);
                  break;
       }

      if(cel[i].texture > 0 )
       { fprintf(fs,"Soil:   ");
         fprintf(fs,"%8.4f%8.4f%8.2f%8.2f\n",basen,basep,poren,porep);
         fprintf(fs,"%16.3f%8.3f%8.3f%8.3f%8i\n",extractn,extractp,
                 leachn,leachp,soilorg);
       }

      /* For fertilizer information */
      if (cel[i].fert_level >= 1 && cel[i].fert_level <= 5)
       fprintf(fs, "Fert:   ");
      switch (cel[i].fert_level)
       { case 1: fprintf(fs,"%8i%8i%8i%8i\n",fert1n,fert1p, cel[i].incor_level,
                         cel[i].incor_level);
                 break;
         case 2: fprintf(fs,"%8i%8i%8i%8i\n",fert2n,fert2p,
                         cel[i].incor_level,cel[i].incor_level);
                 break;
         case 3: fprintf(fs,"%8i%8i%8i%8i\n",fert3n,fert3p,
                         cel[i].incor_level,cel[i].incor_level);
                 break;
         case 4: /* Handle user input fertilizer information */
                 fcel_id = i;
                 fert_lvl_input();
                 fprintf(fs,"%8i%8i%8i%8i\n",fert4n,fert4p,fert_nfac,fert_pfac);
                 break;
         case 5: /* commented out by Dave Peterson, March 1996
                 fprintf(temp,"Fert at i=%d\n",i);
                 */
                 fprintf(fs,"%8i%8i%8i%8i\n",cel[i].fert4n, cel[i].fert4p,
                         cel[i].fert_nfac,cel[i].fert_pfac);
                 break;
       }

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

      /* write pesticide data to output file ... */

      if (pest != 0)
       { fprintf(fs, "Pest:   ");
         make_name_string(pest_scenario_array[pest].herbicide, name_str);
         fprintf(fs, name_str);

         value1 = pest_scenario_array[pest].apply_time;
         switch (value1)
          { case 1: p1 = &(pest_scenario_array[pest].data.pre_p);
                    value2  = p1 -> days_since_app;
                    value3  = p1 -> app_rate;
                    value4  = p1 -> app_eff;
                    value5  = 0;
                    value6  = p1 -> s_residue;
                    value7  = p1 -> s_residue_half_life;
                    value8  = p1 -> inc_depth;
                    value9  = p1 -> inc_eff;
                    value10 = p1 -> solubility;
                    value11 = p1 -> carbon_sorption;
                    value12 = 0.0;
                    value13 = 0.0;
                    value14 = 0;
                    value15 = 0.0;
                    break;
            case 2: p2 = &(pest_scenario_array[pest].data.pre_e);
                    value2  = p2 -> days_since_app;
                    value3  = p2 -> app_rate;
                    value4  = p2 -> app_eff;
                    value5  = 0;
                    value6  = p2 -> s_residue;
                    value7  = p2 -> s_residue_half_life;
                    value8  = 0.0;
                    value9  = 0;
                    value10 = p2 -> solubility;
                    value11 = p2 -> carbon_sorption;
                    value12 = 0.0;
                    value13 = 0.0;
                    value14 = 0;
                    value15 = 0.0;
                    break;
            case 3: p3 = &(pest_scenario_array[pest].data.post_e);
                    value2  = p3 -> days_since_app;
                    value3  = p3 -> app_rate;
                    value4  = p3 -> app_eff;
                    value5  = p3 -> canopy_cover;
                    value6  = p3 -> s_residue;
                    value7  = p3 -> s_residue_half_life;
                    value8  = 0.0;
                    value9  = 0;
                    value10 = p3 -> solubility;
                    value11 = p3 -> carbon_sorption;
                    value12 = p3 -> f_residue;
                    value13 = p3 -> f_wash_threshold;
                    value14 = p3 -> f_wash_fraction;
                    value15 = p3 -> f_residue_half_life;
                    break;
          }
         fprintf(fs, "%16d %7.1f %7.2f %7d %7d\n", value1, value2, value3,
                 value4, value5);
         fprintf(fs, "%16.2f %7.1f %7.2f %7d %11.3f %11.3f\n", value6,
                 value7, value8, value9, value10, value11);
         fprintf(fs, "%16.2f %7.2f %7d %7.1f\n", value12, value13, value14,
                 value15);
       }

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/

      /* commented out by Dave Peterson, March 1996
      fprintf(temp,"tptsrc at cell %d=%d\n",i,tptsrc);
      */  
      if(tptsrc > 0)
       { for(j=0; j<=mnfdlt-1; j++)
          { fprintf(fs,"NonFdlt:"); 
            fprintf(fs,"%8.3f%8.2f%8.2f%8.2f%8i\n",
                    nfdltpts[j].wdisch,nfdltpts[j].tn,nfdltpts[j].tp,
                    nfdltpts[j].tcod,nfdltpts[j].ptsloc);
          }
         for(j=0; j<= mfdlt-1; j++)
          { fprintf(fs,"Feedlot:");
            fprintf(fs,"%8.2f%8i%8.2f%8i%8i%8i\n",
                    fdltpts[j].fdarea,fdltpts[j].fcs,fdltpts[j].rfarea,
                    fdltpts[j].fln,fdltpts[j].flp,fdltpts[j].flcod);
            fprintf(fs,"%16i%8.1f%8.1f%8.1f%8.1f%8.1f%8.1f\n",
                    1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

            /* Definition parameters of the tributary areas */
            /* and adjacent areas of a feedlot              */
            fprintf(fs,"%16.2f%8i%8.2f%8i\n",fdltpts[j].tb1a,
                    fdltpts[j].tb1cnt,fdltpts[j].ad1a,fdltpts[j].ad1cnt);
            fprintf(fs,"%16.2f%8i%8.2f%8i\n",fdltpts[j].tb2a,
                    fdltpts[j].tb2cnt,fdltpts[j].ad2a,fdltpts[j].ad2cnt);
            fprintf(fs,"%16.2f%8i%8.2f%8i\n",fdltpts[j].tb3a,
                    fdltpts[j].tb3cnt,fdltpts[j].ad3a,fdltpts[j].ad3cnt);
            fprintf(fs,"%16.2f%8i%8.2f%8i\n",fdltpts[j].tb4a,
                    fdltpts[j].tb4cnt,fdltpts[j].ad4a,fdltpts[j].ad4cnt);
            fprintf(fs,"%16.2f%8i%8.2f%8i\n",fdltpts[j].tb5a,
                    fdltpts[j].tb5cnt,fdltpts[j].ad5a,fdltpts[j].ad5cnt);
            fprintf(fs,"%16.2f%8i%8.2f%8i\n",fdltpts[j].tb6a,
                    fdltpts[j].tb6cnt,fdltpts[j].ad6a,fdltpts[j].ad6cnt);

            /* Definition parameters of the buffer areas    */
            fprintf(fs,"%16.2f%8.2f%8i\n",
                    fdltpts[j].bf1slp,fdltpts[j].bf1scnt,fdltpts[j].bf1fll);
            fprintf(fs,"%16.2f%8.2f%8i\n",
                    fdltpts[j].bf2slp,fdltpts[j].bf2scnt,fdltpts[j].bf2fll);
            fprintf(fs,"%16.2f%8.2f%8i\n",
                    fdltpts[j].bf3slp,fdltpts[j].bf3scnt,fdltpts[j].bf3fll);
            fprintf(fs,"%16i%8.2f%8.2f%8.2f\n",fdltpts[j].nanimal1,
                    fdltpts[j].animcod1,fdltpts[j].animp1,fdltpts[j].animn1);
            fprintf(fs,"%16i%8.2f%8.2f%8.2f\n",fdltpts[j].nanimal2,
                    fdltpts[j].animcod2,fdltpts[j].animp2,fdltpts[j].animn2);
            fprintf(fs,"%16i%8.2f%8.2f%8.2f\n",fdltpts[j].nanimal3,
                    fdltpts[j].animcod3,fdltpts[j].animp3,fdltpts[j].animn3);
          }
       }

      if(cel[i].chl_indicator <= 1 )
       { fprintf(fs,"Channel:");
         fprintf(fs,"%8.2f%8.4f%8.4f%8.2f%8.4f%8.4f\n",
                 chnw,chnwco,chwexp,chdepth,chdepco,chdepexp);
         cel[i].chl_slope = 0.0;
         cel[i].ch_side_slope = 0.0;
         fprintf(fs,"%16.2f%8.3f%8.4f%8.2f%8.2f\n",
                 chlenth,chlco,chlexp,cel[i].chl_slope,
                 cel[i].ch_side_slope);
         fprintf(fs,"%16.3f%8i%8i%8i%8i\n",
                 chmanf,agdecay,ndecay,pdecay,coddecay);
         fprintf(fs,"%16i%8i%8i%8i%8i\n",
                 clyscour,sltscour,saggsur,laggsur,sandsur);
       }
      else
       { chlw     = cel[i].chlw;
         chlwc    = cel[i].chlwc;
         chlwe    = cel[i].chlwe;
         chld     = cel[i].chld;
         chldc    = cel[i].chldc;
         chlde    = cel[i].chlde;
         chllnth  = cel[i].chllnth;
         chllnthc = cel[i].chllnthc;
         chllnthe = cel[i].chllnthe;
         chlm     = cel[i].chlm;

         fprintf(fs,"Channel:");
         fprintf(fs,"%8.2f%8.4f%8.4f%8.2f%8.4f%8.4f\n",
                 chlw,chlwc,chlwe,chld,chldc,chlde);
         fprintf(fs,"%16.2f%8.3f%8.4f%8.2f%8.2f\n",
                 chllnth,chllnthc,chllnthe,cel[i].chl_slope,
                 cel[i].ch_side_slope);
         fprintf(fs,"%16.3f%8i%8i%8i%8i\n",
                 chlm,agdecay,ndecay,pdecay,coddecay);
         fprintf(fs,"%16i%8i%8i%8i%8i\n",
                 clyscour,sltscour,saggsur,laggsur,sandsur);
       }
    }

   fclose(fs);

   fprintf (stderr,"Done creating AGNPS input file %s, press ENTER:", in_fl_name);
   getchar(); 
 }

/***************************************************************/
/* start of code section added by Dave Peterson, February 1996 */
/***************************************************************/

void get_user_pest_data(pest_scenario *scenarios)

/* prompt user for pesticide data for each scenario found in map */

 { int  i, level, herb;
   char r[50], s[50], t[2], names[NUM_PEST][50];

   t[0] = 0;

   sprintf(s, "Pesticide data entry");
   sprintf(r, "Enter a herbicide choice [1 - %d]: ", NUM_PEST);

   for (i = 1; i <= NUM_PEST; i++)
    make_menu_name_string(i, names[i - 1]);

   for (; ; )
    { V_clear();
      V_line(3,  "Pesticide input data");
      V_line(5,  "====================== ");
      V_line(7,  "  Herbicides:");
      for (i = 1; i <= NUM_PEST; i++)
       V_line(i + 7, names[i - 1]);
      V_line(i + 7, r);

      V_ques(t, 's', 14, 35, 1);

      if(!V_call()) exit(1);

      if (t[0] >= '1' && t[0] <= NUM_PEST + '0')
       break;

      V_clear();
      fprintf (stderr,"Your choice for herbicide must be a number between 1 and %d.\n\n",NUM_PEST);
      fprintf (stderr,"Please press ENTER and try again: ");
      getchar();
    }

   herb = t[0] - '0';

   /* initialize the "used" field of each scenario to 0 */
   for (i = 1; i <= MAX_SCENARIO_NUM; i++)
    scenarios[i].used = 0;

   /* find out which scenarios exist on the map */
   for (i = 1; i <= tot_cells; i++)
    scenarios[cel[i].pest_level].used = 1;

   /* get pesticide data for each scenario found on map */
   for (i = 1; i <= MAX_SCENARIO_NUM; i++)
    if (scenarios[i].used)
     { scenarios[i].herbicide = herb;
       get_pest_scenario_data(&(scenarios[i]), i);
     }
 }

void get_pest_scenario_data(pest_scenario *sc, int sc_num)

 /* get pesticide data for a particular scenario */

 { char s[50], t[75], u[2];

   sprintf(s, "Pesticide data entry");
   sprintf(t, "(Scenario %i, Herbicide = %s [%s]):", sc_num,
           PEST_NAMES[(sc -> herbicide) - 1],
           PEST_TRADE_NAMES[(sc -> herbicide) - 1]);
   u[0] = 0;

   /* prompt user for herbicide and application time */
   for (; ; )
    { V_clear();
      V_line(5,  s);
      V_line(6,  t);
      V_line(8,  "Application timing:");
      V_line(9,  "   1.  Preplant");
      V_line(10, "   2.  Preemergence");
      V_line(11, "   3.  Postemergence");
      V_line(12, "Enter a choice for application timing [1 - 3]:  ");

      V_ques(u, 's', 12, 47, 1);

      if(!V_call()) exit(1);

      if (u[0] >= '1' && u[0] <= '3')
       break;

      V_clear();
      fprintf (stderr,"Your choice for application time must be a number between\n");
      fprintf (stderr,"1 and 3.\n\n");
      fprintf (stderr,"Please press ENTER and try again: ");
      getchar();
    }

   sc -> apply_time = u[0] - '0';

   /* get additional data, depending on whether user selected preplant,
      preemergence, or postemergence
   */
   switch (sc -> apply_time)
    { case 1: get_pre_p_data  (sc, sc_num); break;
      case 2: get_pre_e_data  (sc, sc_num); break;
      case 3: get_post_e_data (sc, sc_num); break;
    };
 }

void get_pre_p_data(pest_scenario *sc, int sc_num)

 /* get data for preplant */

 { char  s[50], t[75];
   float f1, f2, f3, f4, f5, f6, f7;
   int   i1, i2;

   sprintf(s, "Preplant data entry");
   sprintf(t, "(Scenario %i, Herbicide = %s [%s]): ", sc_num,
           PEST_NAMES[(sc -> herbicide) - 1],
           PEST_TRADE_NAMES[(sc -> herbicide) - 1]);
   f1 = 0.0;
   f2 = 0.0;
   i1 = 75;
   f3 = 0.0;
   i2 = 0;
   f4 = 0.0;
   f5 = PEST_SPEC_DATA[(sc -> herbicide) - 1].res_half_life;
   f6 = PEST_SPEC_DATA[(sc -> herbicide) - 1].solubility;
   f7 = PEST_SPEC_DATA[(sc -> herbicide) - 1].carbon_sorption;

   V_clear();
   V_line(5,  s);
   V_line(6,  t);
   V_line(8,  "  Time since application (days):");
   V_line(9,  "  Application rate (lbs./acre):");
   V_line(10, "  Application efficiency (%):");
   V_line(11, "  Incorporation depth (in.):");
   V_line(12, "  Incorporation efficiency (%):");
   V_line(13, "  Initial soil residue (lbs./acre):");
   V_line(14, "  Soil residue half-life (days):");
   V_line(15, "  Solubility in water (ppm):");
   V_line(16, "  Organic carbon sorption (KOC):");
   V_ques(&f1, 'f', 8,  35, 8);
   V_ques(&f2, 'f', 9,  35, 8);
   V_ques(&i1, 'i', 10,  35, 8);
   V_ques(&f3, 'f', 11, 35, 8);
   V_ques(&i2, 'i', 12, 35, 8);
   V_ques(&f4, 'f', 13, 35, 8);
   V_ques(&f5, 'f', 14, 35, 8);
   V_ques(&f6, 'f', 15, 35, 8);
   V_ques(&f7, 'f', 16, 35, 8);
   if(!V_call()) exit(1);

   (sc -> data).pre_p.days_since_app      = f1;
   (sc -> data).pre_p.app_rate            = f2;
   (sc -> data).pre_p.app_eff             = i1;
   (sc -> data).pre_p.inc_depth           = f3;
   (sc -> data).pre_p.inc_eff             = i2;
   (sc -> data).pre_p.s_residue           = f4;
   (sc -> data).pre_p.s_residue_half_life = f5;
   (sc -> data).pre_p.solubility          = f6;
   (sc -> data).pre_p.carbon_sorption     = f7;
 }

void get_pre_e_data(pest_scenario *sc, int sc_num)

 /* get preemergence data */

 { char  s[50], t[75];
   float f1, f2, f3, f4, f5, f6;
   int   i1;

   sprintf(s, "Preemergence data entry");
   sprintf(t, "(Scenario %i, Herbicide = %s [%s]): ", sc_num,
           PEST_NAMES[(sc -> herbicide) - 1],
           PEST_TRADE_NAMES[(sc -> herbicide) - 1]);
   f1 = 0.0;
   f2 = 0.0;
   i1 = 75;
   f3 = 0.0;
   f4 = PEST_SPEC_DATA[(sc -> herbicide) - 1].res_half_life;
   f5 = PEST_SPEC_DATA[(sc -> herbicide) - 1].solubility;
   f6 = PEST_SPEC_DATA[(sc -> herbicide) - 1].carbon_sorption;

   V_clear();
   V_line(5,  s);
   V_line(6,  t);
   V_line(8,  "  Time since application (days):");
   V_line(9,  "  Application rate (lbs./acre):");
   V_line(10, "  Application efficiency (%):");
   V_line(11, "  Initial soil residue (lbs./acre):");
   V_line(12, "  Soil residue half-life (days):");
   V_line(13, "  Solubility in water (ppm):");
   V_line(14, "  Organic carbon sorption (KOC):");
   V_ques(&f1, 'f', 8,  35, 8);
   V_ques(&f2, 'f', 9,  35, 8);
   V_ques(&i1, 'i', 10,  35, 8);
   V_ques(&f3, 'f', 11, 35, 8);
   V_ques(&f4, 'f', 12, 35, 8);
   V_ques(&f5, 'f', 13, 35, 8);
   V_ques(&f6, 'f', 14, 35, 8);
   if(!V_call()) exit(1);

   (sc -> data).pre_e.days_since_app      = f1;
   (sc -> data).pre_e.app_rate            = f2;
   (sc -> data).pre_e.app_eff             = i1;
   (sc -> data).pre_e.s_residue           = f3;
   (sc -> data).pre_e.s_residue_half_life = f4;
   (sc -> data).pre_e.solubility          = f5;
   (sc -> data).pre_e.carbon_sorption     = f6;
 }

void get_post_e_data(pest_scenario *sc, int sc_num)

 /* get postemergence data */

 { char s[50], t[75];
   float f1, f2, f3, f4, f5, f6, f7, f8, f9;
   int   i1, i2, i3;

   sprintf(s, "Postemergence Data Entry");
   sprintf(t, "(Scenario %i, Herbicide = %s [%s]): ", sc_num,
           PEST_NAMES[(sc -> herbicide) - 1],
           PEST_TRADE_NAMES[(sc -> herbicide) - 1]);
   f1 = 0.0;
   f2 = 0.0;
   i1 = 75;
   i2 = 20;
   f3 = 0.0;
   f4 = 0.0;
   f5 = 0.1;
   i3 = 40;
   f6 = 3.0;
   f7 = PEST_SPEC_DATA[(sc -> herbicide) - 1].res_half_life;
   f8 = PEST_SPEC_DATA[(sc -> herbicide) - 1].solubility;
   f9 = PEST_SPEC_DATA[(sc -> herbicide) - 1].carbon_sorption;

   V_clear();
   V_line(5,  s);
   V_line(6,  t);
   V_line(8,  "  Time since application (days):");
   V_line(9,  "  Application rate (lbs./acre):");
   V_line(10, "  Application efficiency (%):");
   V_line(11, "  Canopy cover (%):");
   V_line(12, "  Initial foliar residue (lbs./acre):");
   V_line(13, "  Initial soil residue (lbs./acre):");
   V_line(14, "  Foliar washoff threshold (in.):");
   V_line(15, "  Foliar washoff fraction (%):");
   V_line(16, "  Foliar residue half-life (days):");
   V_line(17, "  Soil residue half-life (days):");
   V_line(18, "  Solubility in water (ppm):");
   V_line(19, "  Organic carbon sorption (KOC):");
   V_ques(&f1, 'f', 8,  37, 8);
   V_ques(&f2, 'f', 9,  37, 8);
   V_ques(&i1, 'i', 10,  37, 8);
   V_ques(&i2, 'i', 11, 37, 8);
   V_ques(&f3, 'f', 12, 37, 8);
   V_ques(&f4, 'f', 13, 37, 8);
   V_ques(&f5, 'f', 14, 37, 8);
   V_ques(&i3, 'i', 15, 37, 8);
   V_ques(&f6, 'f', 16, 37, 8);
   V_ques(&f7, 'f', 17, 37, 8);
   V_ques(&f8, 'f', 18, 37, 8);
   V_ques(&f9, 'f', 19, 37, 8);
   if(!V_call()) exit(1);

   (sc -> data).post_e.days_since_app      = f1;
   (sc -> data).post_e.app_rate            = f2;
   (sc -> data).post_e.app_eff             = i1;
   (sc -> data).post_e.canopy_cover        = i2;
   (sc -> data).post_e.f_residue           = f3;
   (sc -> data).post_e.s_residue           = f4;
   (sc -> data).post_e.f_wash_threshold    = f5;
   (sc -> data).post_e.f_wash_fraction     = i3;
   (sc -> data).post_e.f_residue_half_life = f6;
   (sc -> data).post_e.s_residue_half_life = f7;
   (sc -> data).post_e.solubility          = f8;
   (sc -> data).post_e.carbon_sorption     = f9;
 }

void to_upper_case(char *s)
/* convert any lower case characters in string s to upper case */
 { int  i;
   char c;

   for (i = 0; (c = s[i]) != 0; i++)
    if (c >= 'a' && c <= 'z')
     { c += 'A' - 'a';
       s[i] = c;
     }
 }

void make_name_string(int which_pest, char *s)
/* make string with pesticide name and trade name for writing to output file */
 { int  i, j;
   char s1[25], s2[25];

   strcpy(s1, PEST_NAMES[which_pest - 1]);
   strcpy(s2, PEST_TRADE_NAMES[which_pest - 1]);
   to_upper_case(s1);
   to_upper_case(s2);
   for (i = 0; s1[i] && i < 25; i++)
    s[i] = s1[i];
   for (; i < 30; i++)
    s[i] = ' ';
   for (j = 0; s2[j] && j < 25; i++, j++)
    s[i] = s2[j];
   s[i++] = '\n';
   s[i] = 0;
 }

void make_menu_name_string(int which_pest, char *s)
/* make string with pesticide name and trade name for display in menu */
 { char t[5];

   sprintf(t, "%2d", which_pest);
   strcpy(s, "   ");
   strcat(s, t);
   strcat(s, "  ");
   strcat(s, PEST_NAMES[which_pest - 1]);
   strcat(s, " (");
   strcat(s, PEST_TRADE_NAMES[which_pest - 1]);
   strcat(s, ")");
 }

char ch_to_upper(char c)
/* return char c converted to upper case */
 { return (c >= 'a' && c <= 'z') ? c + ('A' - 'a') : c; }

int is_whitespace(char c)
/* return 1 if char c is whitespace (tab or space); else return 0 */
 { return (c == ' ' || c == '\t'); }

int approx_compare(const char *s1, const char *s2)

 /* return 0 if strings equal or 1 if unequal; not case sensitive;
    ignore leading and trailing whitespace
 */

 { int i1, i2;

   /* scan past any leading whitespace ... */
   for (i1 = 0; is_whitespace(s1[i1]); i1++);
   for (i2 = 0; is_whitespace(s2[i2]); i2++);

   /* compare strings; stop on first nonmatching char, or when end of either */
   /* string is reached                                                      */
   while (s1[i1] && s2[i2] && ch_to_upper(s1[i1]) == ch_to_upper(s2[i2]))
    { i1++; i2++; }

   /* strings equal (s1[i1] and s2[i2] are both terminating NULL chars), so */
   /* return 0                                                              */
   if (s1[i1] == s2[i2]) return 0;

   /* if current char in either string is neither whitespace nor NULL */
   /* terminator then strings must be unequal, so return 1            */
   if (!is_whitespace(s1[i1]) && s1[i1]) return 1;
   if (!is_whitespace(s2[i2]) && s2[i2]) return 1;

   /* scan past any trailing whitespace ... */
   while (s1[i1] == ' ' || s1[i1] == '\t') i1++;
   while (s2[i2] == ' ' || s2[i2] == '\t') i2++;

   /* if terminating NULL char immediately follows trailing whitespace in each */
   /* string, then strings are equal, so return 0                              */
   if (s1[i1] == 0 && s2[i2] == 0) return 0;

   /* else strings must be unequal, so return 1 */
   return 1;
 }

/*************************************************************/
/* end of code section added by Dave Peterson, February 1996 */
/*************************************************************/
