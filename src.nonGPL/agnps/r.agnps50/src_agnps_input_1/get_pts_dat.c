
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
        V_line(1,"	Point source type: Feedlot/Nonfeedlot (F/N)?");
        V_ques(buf1,'s',1,60,15);
        if(!V_call()) exit(1);
        V_clear();
        if(approx_compare(buf1, "Nonfeedlot") == 0) {
          pntsrctp = -1;
          wdisch   = 0.0;
          tn       = 0.0;
          tp       = 0.0;
          tcod     = 0.0;
          ptsloc   = 0;
          V_line(3,  "Nonfeedlot Point Source");
          V_line(4,  "=======================");
          V_line(7,  "1.  Water discharge rate (0-100. cf/s)");
          V_line(9,  "2.  Total nitrogen concentration (0-100. ppm)"); 
          V_line(11, "3.  Total phosphorus concentration (0-100. ppm)");
          V_line(13, "4.  Total COD concentration (0-1000.0 ppm)");
          /* V_line(15,"    5.  Point Source Loaction");*/

          V_ques(&wdisch,'f',7,50,15);
          V_ques(&tn,'f',9,50,5);
          V_ques(&tp,'f',11,50,5);
          V_ques(&tcod,'f',13,50,5);
          /* V_ques(&ptsloc,'i',15,60,5); */

          V_intrpt_ok();
          if(!V_call()) exit(1);
          }



        V_clear();
        if((strncmp(buf1,"FeedLot",1)) == 0) {
           pntsrctp = 1;
           fdarea = 0.3;
           fcs    = 98;
           rfarea = 0.0;
           fln = 300;
           flp = 85;
           flcod = 4500;  
           V_clear();
           V_line(1, "Feedlot Point Source Data");
           V_line(2, "=========================");
           V_line(4, "1.  Feedlot area (acres)");
           V_line(5, "2.  Feedlot curve number");
           V_line(6, "3.  Feedlot roofed area (acres)");
           V_line(7, "4.  Feedlot runoff nitrogen (default: 300 ppm)");
           V_line(8, "5.  Feedlot runoff phosphorus (default: 85 ppm)");
           V_line(9, "6.  Feedlot runoff COD ((default: 4500 ppm)");
           V_ques(&fdarea,'f',4,50,6);
           V_ques(&fcs,'i',5,50,6);
           V_ques(&rfarea,'f',6,50,6);
           V_ques(&fln,'i',7,50,6);
           V_ques(&flp,'i',8,50,6);
           V_ques(&flcod,'i',9,50,6);
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
           V_line(2,  "==============================");
           V_line(4,  "1.  Area of subarea 1 of tributary area");
           V_line(5,  "2.  Curve number of aubarea 1 of aributary area");
           V_line(6,  "3.  Area of subarea 2 of tributary area");
           V_line(7,  "4.  Curve number of subarea 2 of tributary area");
           V_line(8,  "5.  Area of subarea 3 of tributary area");
           V_line(9,  "6.  Curve number of subarea 3 of tributary area");
           V_line(10, "7.  Area of subarea 4 of tributary area");
           V_line(11, "8.  Curve number of subarea 4 of tributary area");
           V_line(12, "9.  Area of subarea 5 of tributary area");
           V_line(13, "10. Curve number of subarea 5 of tributary area");
           V_line(14, "11. Area of subarea 6 of tributary area");
           V_line(15, "12. Curve number of subarea 6 of tributary area");

         /* for tributary areas                     */
           V_ques(&tb1a,'f',4,50,6);
           V_ques(&tb1cnt,'i',5,50,6);
           V_ques(&tb2a,'f',6,50,6);
           V_ques(&tb2cnt,'i',7,50,6);
           V_ques(&tb3a,'f',8,50,6);
           V_ques(&tb3cnt,'i',9,50,6);
           V_ques(&tb4a,'f',10,50,6);
           V_ques(&tb4cnt,'i',11,50,6);
           V_ques(&tb5a,'f',12,50,6);
           V_ques(&tb5cnt,'i',13,50,6);
           V_ques(&tb6a,'f',14,50,6);
           V_ques(&tb6cnt,'i',15,50,6);
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

           V_line(1, "Area (Area 3) Adjacent to the Feedlot");
           V_line(2, "==============================");
           V_line(4, "1.  Area of adjacent  area 1 ");
           V_line(5, "2.  Curve number of adjacent area 1");
           V_line(6, "3.  Area of adjacent  area 2 ");
           V_line(7, "4.  Curve number of adjacent area 2");
           V_line(8, "5.  Area of adjacent  area 3 ");
           V_line(9, "6.  Curve number of adjacent area 3");
           V_line(10,"7.  Area of adjacent  area 4 ");
           V_line(11,"8.  Curve number of adjacent area 4");
           V_line(12,"9.  Area of adjacent  area 5 ");
           V_line(13,"10. Curve number of adjacent area 5");
           V_line(14,"11. Area of adjacent  area 6 ");
           V_line(15,"12. Curve number of adjacent area 6");

         /* for adjacent areas                     */
           V_ques(&ad1a,'f',4,50,6);
           V_ques(&ad1cnt,'i',5,50,6);
           V_ques(&ad2a,'f',6,50,6);
           V_ques(&ad2cnt,'i',7,50,6);
           V_ques(&ad3a,'f',8,50,6);
           V_ques(&ad3cnt,'i',9,50,6);
           V_ques(&ad4a,'f',10,50,6);
           V_ques(&ad4cnt,'i',11,50,6);
           V_ques(&ad5a,'f',12,50,6);
           V_ques(&ad5cnt,'i',13,50,6);
           V_ques(&ad6a,'f',14,50,6);
           V_ques(&ad6cnt,'i',15,50,6);
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
           V_line(3,  "1.  Slope of buffer area 1");
           V_line(4,  "2.  Buffer area 1 surface constant");
           V_line(5,  "3.  Flow length for buffer area 1 (feet)");
           V_line(6,  "4.  Slope of buffer area 2");
           V_line(7,  "5.  Buffer area 2 surface constant");
           V_line(8,  "6.  Flow length for buffer area 2 (feet)");
           V_line(9,  "7.  Slope of buffer area 3");
           V_line(10, "8.  Buffer area 3 surface constant");
           V_line(11, "9.  Flow length for buffer area 3 (feet)");

         /* for 3 buffer areas                     */
           V_ques(&bf1slp,'f',3,50,6);
           V_ques(&bf1scnt,'f',4,50,6);
           V_ques(&bf1fll,'i',5,50,6);
           V_ques(&bf2slp,'f',6,50,6);
           V_ques(&bf2scnt,'f',7,50,6);
           V_ques(&bf2fll,'i',8,50,6);
           V_ques(&bf3slp,'f',9,50,6);
           V_ques(&bf3scnt,'f',10,50,6);
           V_ques(&bf3fll,'i',11,50,6);
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
           V_line(3,  "1.  Number of animals in type 1");
           V_line(4,  "2.  The COD factor for animal type 1");
           V_line(5,  "3.  Phosphorus of animal type 1");
           V_line(6,  "4.  Nitrogen of animal type 1");
           V_line(7,  "5.  Number of animals in type 2");
           V_line(8,  "6.  The COD factor for animal type 2");
           V_line(9,  "7.  Phosphorus of animal type 2");
           V_line(10, "8.  Nitrogen of animal type 2");
           V_line(11, "9.  Number of animals in type 3");
           V_line(12, "10. The COD factor for animal type 3");
           V_line(13, "11. Phosphorus of animal type 3");
           V_line(14, "12. Nitrogen of animal type 3");

         /* Descriptions of animals                */
           V_ques(&nanimal1,'i',3,50,6);
           V_ques(&animcod1,'f',4,50,6);
           V_ques(&animp1,'f',5,50,6);
           V_ques(&animn1,'f',6,50,6);
           V_ques(&nanimal2,'i',7,50,6);
           V_ques(&animcod2,'f',8,50,6);
           V_ques(&animp2,'f',9,50,6);
           V_ques(&animn2,'f',10,50,6);
           V_ques(&nanimal3,'i',11,50,6);
           V_ques(&animcod3,'f',12,50,6);
           V_ques(&animp3,'f',13,50,6);
           V_ques(&animn3,'f',14,50,6);

           V_intrpt_ok();
           if(!V_call()) exit(1);
           }

}
