/*       ===========================================================
         psu_agnps_view.c has the sole purpose of running the GIS
         functions for displaying AGNPS output. 
         ===========================================================
*/


main()
{ char input_nps_filename[81];

  G_gisinit("r.agnps50.view");
  Critical_area_analys(input_nps_filename);
  show_maps(input_nps_filename);
}
