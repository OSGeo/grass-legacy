
/*   This GRASS AGNPS interface is used for user to */
/*   input fertilizer application rate and the      */
/*   fertilizer availability factors in a format    */
/*   compatible with the latest version of          */ 
/*   AGNPS-- AGNPS5.0(including AGNPS4.03).         */
/*                                                  */
/*                        Zhian  Li                 */
/*                                                  */
/*                        Penn State University     */
/*                                                  */
/*                        August, 1995              */ 

#include "agnps_input.h"

dfert_lvl_input()
{

        char	buf[1024];
        char    buf1[15];


          fcel_id   = 0;
          fert4n    = 0;
          fert4p    = 0;
          fert_nfac = 0;
          fert_pfac = 0;

        V_clear();
          V_line(1,  "Cell number --------------------->"); 
          V_line(3,  "Fertilizer Application Information");
          V_line(4,  "==================================");
          V_line(7,  "1.  Nitrogen application rate (lb/acre)");
          V_line(9,  "2.  Phosphorus application rate (lb/acre)"); 
          V_line(11, "3.  Nitrogen availability factor (%)");
          V_line(13, "4.  Phosphorus availability factor (%)");

          V_ques(&fcel_id,'i',1,45,5);
          V_ques(&fert4n,'i',7,45,5);
          V_ques(&fert4p,'i',9,45,5);
          V_ques(&fert_nfac,'i',11,45,5);
          V_ques(&fert_pfac,'i',13,45,5);

          V_intrpt_ok();
          if(!V_call()) exit(1);


}
