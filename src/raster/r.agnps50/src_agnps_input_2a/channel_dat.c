
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

channel_dat()
{

        char	buf[1024];
        char    buf1[15];




        V_clear();
          V_line(1,  "General Channel Information   ");
          V_line(3,  "==============================");
          V_line(5,  "For waterway and no definitive channel");
          V_line(9,  "1.  Channel width"); 
          V_line(10, "2.  Geomorphic width coefficient");
          V_line(11, "3.  Geomorphic width exponent");
          V_line(12, "4.  Channel depth");
          V_line(13, "5.  Geomorphic depth coefficient");
          V_line(14, "6.  Geomorphic depth exponent");
          V_line(15, "7.  Channel length");
          V_line(16, "8.  Channel length coefficient");
          V_line(17, "9.  Channel length exponent");
          V_line(18, "10. Channel Manning coefficient");                        

          V_ques(&chnw,'f',9,40,5);
          V_ques(&chnwco,'f',10,40,5);
          V_ques(&chwexp,'f',11,40,5);
          V_ques(&chdepth,'f',12,40,5);
          V_ques(&chdepco,'f',13,40,5);
          V_ques(&chdepexp,'f',14,40,5);
          V_ques(&chlenth,'f',15,40,5);
          V_ques(&chlco,'f',16,40,5);
          V_ques(&chlexp,'f',17,40,5);
          V_ques(&chmanf,'f',18,40,5);

          V_intrpt_ok();
          if(!V_call()) exit(1);
          V_clear();


}
