
/*   This GRASS AGNPS interface is used for user to */
/*   input fertilizer application rate and the      */
/*   fertilizer availability factors in a format    */
/*   compatible with the latest version of          */ 
/*   AGNPS-- AGNPS5.0(including AGNPS4.03).         */
/*                                                  */
/*   This function is used to allow the user to     */
/*   enter channel data for TR-55/Nongeomorphic     */
/*   AGNPS calculation.                             */
/*                                                  */
/*                        Zhian  Li                 */
/*                                                  */
/*                        Penn State University     */
/*                                                  */
/*                        August, 1995              */ 

#include "agnps_input.h"

dchannel00_dat(int cell_number)
{

        char	buf[1024];
        char    buf1[15];
        char    str[50];


        chlw       = 0.0;
        chlwc      = 3.425;
        chlwe      = 0.3151;
        chld       = 0.0;
        chldc      = 0.4537;
        chlde      = 0.2192;
        chllnthc   = 153.0;
        chllnthe   = 0.6;
        chlm       = 0.103;

        if (cell_number == 0)
         { chllnth    = 0.0;
           chlslope   = 0.0;    /* added by Dave Peterson, March 1996 */
           chlsslop   = 10.0;   /* added by Dave Peterson, March 1996 */
           chl_type   = 2;
           chl_cel_id = 0;
         }
        else
         { chllnth    = cel[cell_number].chllnth;
           chlslope   = cel[cell_number].chl_slope;
           chlsslop   = cel[cell_number].ch_side_slope;
           chl_type   = cel[cell_number].chl_indicator;
           chl_cel_id = cell_number;
         }

          strcpy(str, "Channel Information ");
          if (geom_mod) strcat(str, "(Geomorphic Option)");
          else          strcat(str, "(Nongeomorphic Option)");

        V_clear();
          V_line(1,  str); 
          V_line(3,  str);
          V_line(4,  "==========================================");
          V_line(5,  "Channel type (1-8):");
          V_line(6,  "   1.  Non definitive channel:");
          V_line(7,  "   2.  Drainage ditch");
          V_line(8,  "   3.  Road ditch");
          V_line(9,  "   4.  Grass waterway");
          V_line(10, "   5.  Ephemeral stream");
          V_line(11, "   6.  Intermittent stream");
          V_line(12, "   7.  Perennial stream");
          V_line(13, "   8.  User defined channel");
/*******************************************************/
/* start of code modified by Dave Peterson, March 1996 */
/*******************************************************/

/* code changed so user gets prompted for channel slope, channel
   side slope, channel length, and Manning's coefficient instead of
   channel width, channel depth, channel length, channel slope, and
   Manning factor
*/
          V_line(15, "Channel characteristics:");
          V_line(16, "   1.  Channel slope (%)"); 
          V_line(17, "   2.  Channel side slope (%)");
          V_line(18, "   3.  Channel length (ft.)");
          V_line(19, "   4.  Channel Manning coefficient");

          V_ques(&chl_cel_id,'i',1,40,5);
          V_ques(&chl_type,'i',5,40,5);
          V_ques(&chlslope,'f',16,40,5);
          V_ques(&chlsslop,'f',17,40,5);
          V_ques(&chllnth,'f',18,40,5);
          V_ques(&chlm,'f',19,40,5);
/*************************************************************/
/* end of code section modified by Dave Peterson, March 1996 */
/*************************************************************/

          V_intrpt_ok();
          if(!V_call()) exit(1);




}
