
/*   This GRASS AGNPS interface is used for user to */
/*   input point sources, i.e. FEEDLOT or NONFEEDLOT*/
/*   in form compatible with the latest version of  */ 
/*   AGNPS-- AGNPS version.                         */
/*                                                  */
/*                        Zhian  Li                 */
/*                                                  */
/*                        Penn State University     */
/*                                                  */
/*                        July, 1995                */ 

#include "agnps_input.h"

get_pts_dat()
{

        char	buf[1024];
        char    buf1[15];




        V_clear();
        /* V_line(1,"	Type of Point Source (Feedlot/NonFeedLot)?");*/
        V_line(2, "Is the point source a feedlot (F) or not (N)? (F/N)");
        V_ques(buf1,'s',2,53,10);
        if(!V_call()) exit(1);
        V_clear();
        if(approx_compare(buf1, "Nonfeedlot") == 0 ||
           approx_compare(buf1, "N") == 0) {
          pntsrctp = -1;
          wdisch   = 0.0;
          tn       = 0.0;
          tp       = 0.0;
          tcod     = 0.0;
          ptsloc   = 0;
          V_line(3,  "Nonfeedlot Point Source Input Data");
          V_line(4,  "==================================");
          V_line(6,  "1.  Water discharge rate (0-100. cf/s)");
          V_line(8,  "2.  Total nitrogen concentration (0-100 ppm)"); 
          V_line(10, "3.  Total phosphorus concentration (0-100 ppm)");
          V_line(12, "4.  Total COD concentration (0-1000.0 ppm)");
          /* V_line(15,"    5.  Point Source Loaction");*/

          V_ques(&wdisch,'f',6,50,15);
          V_ques(&tn,'f',8,50,5);
          V_ques(&tp,'f',10,50,5);
          V_ques(&tcod,'f',12,50,5);
          /* V_ques(&ptsloc,'i',15,60,5); */

          V_intrpt_ok();
          if(!V_call()) exit(1);
          }



        V_clear();
        if(approx_compare(buf1,"Feedlot") == 0 ||
           approx_compare(buf1, "F") == 0) {
           pntsrctp = 1;
           fdarea = 0.3;
           fcs    = 98;
           rfarea = 0.0;
           fln = 300;
           flp = 85;
           flcod = 4500;  
           V_clear();
           V_line(1,  "Feedlot Point Source Input Data");
           V_line(2,  "===============================");
           V_line(4,  "1.  Feedlot area (acres)");
           V_line(6,  "2.  Feedlot curve number");
           V_line(8,  "3.  Feedlot roofed area (acres)");
           V_line(10, "4.  Feedlot runoff nitrogen (default: 300 ppm)");
           V_line(12, "5.  Feedlot runoff phosphorus (default: 85 ppm)");
           V_line(14, "6.  Feedlot runoff COD (default: 4500 ppm)");
           V_ques(&fdarea,'f',4,50,6);
           V_ques(&fcs,'i',6,50,6);
           V_ques(&rfarea,'f',8,50,6);
           V_ques(&fln,'i',10,50,6);
           V_ques(&flp,'i',12,50,6);
           V_ques(&flcod,'i',14,50,6);
           V_intrpt_ok();
           if(!V_call()) exit(1);




           V_clear();

           /* Tributary area description                       */
           tb1a   = 0.1;
           tb1cnt = 72;
           tb2a   = 0.0;
           tb2cnt = 0;
           tb3a   = 0.0;
           tb3cnt = 0;
           tb4a   = 0.0;
           tb4cnt = 0;
           tb5a   = 0.0;
           tb5cnt = 0;
           tb6a   = 0.0;
           tb6cnt = 0;

           V_line(1,  "Tributary Area (Area 2) of the Feedlot");
           V_line(2,  "(contributes runoff to feedlot)");
           V_line(3,  "======================================");
           V_line(5,  "1.  Subarea 1 (acres)");
           V_line(6,  "    Curve number of subarea 1");
           V_line(8,  "2.  Subarea 2 (acres)");
           V_line(9,  "    Curve number of subarea 2");
           V_line(11, "3.  Subarea 3 (acres)");
           V_line(12, "    Curve number of subarea 3");
           V_line(14, "4.  Subarea 4 (acres)");
           V_line(15, "    Curve number of subarea 4");
           V_line(17, "(Use only 1 subarea when tributary area is relatively uniform)");

         /* for tributary areas                     */
           V_ques(&tb1a,'f',5,35,6);
           V_ques(&tb1cnt,'i',6,35,6);
           V_ques(&tb2a,'f',8,35,6);
           V_ques(&tb2cnt,'i',9,35,6);
           V_ques(&tb3a,'f',11,35,6);
           V_ques(&tb3cnt,'i',12,35,6);
           V_ques(&tb4a,'f',14,35,6);
           V_ques(&tb4cnt,'i',15,35,6);
           V_intrpt_ok();
           if(!V_call()) exit(1);


           V_clear();

           /* Adjacent area description                       */
           ad1a   = 0.1;
           ad1cnt = 74;
           ad2a   = 0.0;
           ad2cnt = 0;
           ad3a   = 0.0;
           ad3cnt = 0;
           ad4a   = 0.0;
           ad4cnt = 0;
           ad5a   = 0.0;
           ad5cnt = 0;
           ad6a   = 0.0;
           ad6cnt = 0;

           V_line(1,  "Adjacent Area (Area 3) of Feedlot");
           V_line(2,  "(adds to drainage from feedlot, i.e. dilutes feedlot runoff)");
           V_line(3,  "============================================================");
           V_line(5,  "1.  Subarea 1 (acres)");
           V_line(6,  "    Curve number of subarea 1");
           V_line(8,  "2.  Subarea 2 (acres)");
           V_line(9,  "    Curve number of subarea 2");
           V_line(11, "3.  Subarea 3 (acres)");
           V_line(12, "    Curve number of subarea 3");
           V_line(14, "4.  Subarea 4 (acres)");
           V_line(15, "    Curve number of subarea 4");
           V_line(17, "(Use only 1 subarea when tributary area is relatively uniform)");

         /* for adjacent areas                     */
           V_ques(&ad1a,'f',5,35,6);
           V_ques(&ad1cnt,'i',6,35,6);
           V_ques(&ad2a,'f',8,35,6);
           V_ques(&ad2cnt,'i',9,35,6);
           V_ques(&ad3a,'f',11,35,6);
           V_ques(&ad3cnt,'i',12,35,6);
           V_ques(&ad4a,'f',14,35,6);
           V_ques(&ad4cnt,'i',15,35,6);
           V_intrpt_ok();
           if(!V_call()) exit(1);

           V_clear();
           /* Buffer area description                       */
           bf1slp  = 5;
           bf1scnt = 0.25;
           bf1fll  = 100;
           bf2slp  = 0;
           bf2scnt = 0.0;
           bf2fll  = 0;
           bf3slp  = 0;
           bf3scnt = 0.0;
           bf3fll  = 0;


           V_line(1,  "Buffer Area (Area 3 ) of the Feedlot");
           V_line(2,  "======================================");
           V_line(4,  "1.  Buffer subarea 1");
           V_line(5,  "    Slope (%)");
           V_line(6,  "    Length (ft.)");
           V_line(7,  "    Surface constant");
           V_line(9,  "2.  Buffer subarea 2");
           V_line(10, "    Slope (%)");
           V_line(11, "    Length (ft.)");
           V_line(12, "    Surface constant");
           V_line(14, "3.  Buffer subarea 3");
           V_line(15, "    Slope (%)");
           V_line(16, "    Length (ft.)");
           V_line(17, "    Surface constant");
           V_line(19, "(use only 1 subarea when buffer is relatively uniform");
           V_line(20, "in the direction of flow)");

         /* for 3 buffer areas                     */
           V_ques(&bf1slp,'f',5,25,6);
           V_ques(&bf1fll,'i',6,25,6);
           V_ques(&bf1scnt,'f',7,25,6);
           V_ques(&bf2slp,'f',10,25,6);
           V_ques(&bf2fll,'i',11,25,6);
           V_ques(&bf2scnt,'f',12,25,6);
           V_ques(&bf3slp,'f',15,25,6);
           V_ques(&bf3fll,'i',16,25,6);
           V_ques(&bf3scnt,'f',17,25,6);
           V_intrpt_ok();
           if(!V_call()) exit(1);

         /* Descriptions of animals                         */
         /* Only 3 different types of animals are allowed   */ 

           nanimal1  = 125;
           animcod1  = 1.96;
           animp1    = 0.92;
           animn1    = 1.68;
           nanimal2  = 0;
           animcod2  = 0.0;
           animp2    = 0.0;
           animn2    = 0.0;
           nanimal3  = 0;
           animcod3  = 0.0;
           animp3    = 0.0;
           animn3    = 0.0;
           V_clear();
           V_line(1,  "Animals in the Feedlot");
           V_line(2,  "======================");
           V_line(4,  "1.  Type 1 animals");
           V_line(5,  "    Number");
           V_line(6,  "    COD factor");
           V_line(7,  "    Phosphorus factor");
           V_line(8,  "    Nitrogen factor");
           V_line(10, "2.  Type 2 animals");
           V_line(11, "    Number");
           V_line(12, "    COD factor");
           V_line(13, "    Phosphorus factor");
           V_line(14, "    Nitrogen factor");
           V_line(16, "3.  Type 3 animals");
           V_line(17, "    Number");
           V_line(18, "    COD factor");
           V_line(19, "    Phosphorus factor");
           V_line(20, "    Nitrogen factor");

         /* Descriptions of animals                */
           V_ques(&nanimal1,'i',5,25,6);
           V_ques(&animcod1,'f',6,25,6);
           V_ques(&animp1,'f',7,25,6);
           V_ques(&animn1,'f',8,25,6);
           V_ques(&nanimal2,'i',11,25,6);
           V_ques(&animcod2,'f',12,25,6);
           V_ques(&animp2,'f',13,25,6);
           V_ques(&animn2,'f',14,25,6);
           V_ques(&nanimal3,'i',17,25,6);
           V_ques(&animcod3,'f',18,25,6);
           V_ques(&animp3,'f',19,25,6);
           V_ques(&animn3,'f',20,25,6);

           V_intrpt_ok();
           if(!V_call()) exit(1);
           }

}
