/*       ===========================================================
         psu_agnps_view.c has the sole purpose of running the GIS
         functions for displaying AGNPS output. 
         ===========================================================
*/


int main (int argc, char *argv[])
{ 
  char input_nps_filename[81];

  G_gisinit (argv[0]);
  Critical_area_analys(input_nps_filename);
  show_maps(input_nps_filename);
}
