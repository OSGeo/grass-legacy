
/*   This GRASS AGNPS interface is used for user to */
/*   input fertilizer application rate and the      */
/*   fertilizer availability factors in a format    */
/*   compatible with the latest version of          */ 
/*   AGNPS-- AGNPS5.0(including AGNPS4.03).         */
/*                                                  */
/*   This function is used to allow the user to     */
/*   enter channel data for TR-55/geomorphic        */
/*   AGNPS calculation.                             */
/*                                                  */
/*                        Zhian  Li                 */
/*                                                  */
/*                        Penn State University     */
/*                                                  */
/*                        August, 1995              */ 

#include "agnps_input.h"

dchannel01_dat(int cell_number)
{

        char	buf[1024];
        char    buf1[15];
        char    str[50];


        chlw       = 0.0;
        chld       = 0.0;
        chllnth    = 0.0;
        chllnthc   = 153.0;
        chllnthe   = 0.6;
        chlm       = 0.103;
        chlwc      = 3.425;
        chlwe      = 0.3151;
        chldc      = 0.4537;
        chlde      = 0.2192;

        if (cell_number == 0)
         { chllnthc   = 153.0;
           chllnthe   = 0.6;
           chlm       = 0.103;
           chl_type   = 2;
           chlslope   = 0;
           chl_cel_id = 0;
         }
        else
         { chllnthc   = cel[cell_number].chllnthc;
           chllnthe   = cel[cell_number].chllnthe;
           chlm       = cel[cell_number].chlm;
           chl_type   = cel[cell_number].chl_indicator;
           chlslope   = cel[cell_number].chl_slope;
           chl_cel_id = cell_number;
         }

          strcpy(str, "Channel Information ");
          if (geom_mod) strcat(str, "(Geomorphic Option)");
          else          strcat(str, "(Nongeomorphic Option)");

        V_clear();

          V_line(1,  "Cell number ---------->");
          V_line(3,  str);
          V_line(4,  "Channel type (1-8):");
          V_line(5,  "   1.  No definitive channel");
          V_line(6,  "   2.  Drainage ditch");
          V_line(7,  "   3.  Road ditch");
          V_line(8,  "   4.  Grass waterway");
          V_line(9,  "   5.  Ephemeral stream");
          V_line(10, "   6.  Intermittent stream");
          V_line(11, "   7.  Perennial stream");
          V_line(12, "   8.  User defined channel");
          V_line(13, "Channel characteristics:");
          V_line(14, "   1.  Channel slope (%)");
          V_line(15, "   2.  Channel length coefficient");
          V_line(16, "   3.  Channel length exponent");
          V_line(17, "   4.  Geomorphic depth coefficient");
          V_line(18, "   5.  Geomorphic depth exponent");
          V_line(19, "   6.  Geomorphic width coefficient");
          V_line(20, "   7.  Geomorphic width exponent");
          V_line(21, "   8.  Channel Manning coefficient");                        

          V_ques(&chl_cel_id,'i',1,40,5);
          V_ques(&chl_type,'i',4,40,5);
          V_ques(&chlslope,'f',14,40,5);
          V_ques(&chllnthc,'f',15,40,5);
          V_ques(&chllnthe,'f',16,40,5);
          V_ques(&chldc,'f',17,40,5);
          V_ques(&chlde,'f',18,40,5);
          V_ques(&chlwc,'f',19,40,5);
          V_ques(&chlwe,'f',20,40,5);
          V_ques(&chlm,'f',21,40,5);

          V_intrpt_ok();
          if(!V_call()) exit(1);




}
