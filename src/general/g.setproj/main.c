/* main.c    1.1   05/16/91  GRASS4.0
*    Created by : M.L.Holko , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*	      Provides a means of creating a new projection
*             information file
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             m.setproj set=mapset for output project info file
*                    proj=projection of the output project info file
*
*/

#include  "table.h" 
#include  "gis.h"
#define MAIN

main (argc,argv) 
  int argc; char *argv[]; 
{ 
    int Out_proj;
    int out_zone;
    int out_stat;
    int ret;
    int i;
    int stat; 
    char cmnd2[500], out_lon0[20], out_lat0[20];
    char proj_out[20], proj_name[50], set_name[20], file[1024], *value1, *a;
    char path[1024],epath[1024],buffa[1024],buffb[1024],answer[200],answer1[200];
    char answer2[200],buff[1024];
    char tmp_buff[20];

    struct Option *projopt;
    struct Key_Value *out_proj_keys, *in_unit_keys;
    double aa,e2;
    FILE *ls, *out1, *FPROJ, *pj;
    char *new_data, *key, *value, spheriod[50], *sph;
    int j, k, in_stat, ii, npr, sph_check;
    struct Cell_head cellhd;

    G_gisinit (argv[0]);

    init_table();
    init_unit_table();
    sprintf(set_name,"PERMANENT");
    G__file_name (path, "", PROJECTION_FILE, set_name);

	 /* get the output projection parameters, if existing */
/*Check for ownership here */
    stat = G__mapset_permissions(set_name);
    if (stat == 0) {
      G_fatal_error("PERMANENT: permission denied.");
    }

   if (access(path,0) == 0)
   {

        fprintf (stderr,  "\n\nWARNING!  A projection file '%s' \n   already exists for this location\n", path);
	fprintf (stderr, "\nOverriding this information implies that the old projection information\n");
	fprintf (stderr, "    was incorrect.  If you change the parameters, all existing data will be\n");
	fprintf (stderr, "    interpretted differently by the projection software.\n%c%c%c",7,7,7);
	fprintf (stderr, "    GRASS will not re-project your data automatically\n\n");
	
        if (G_yes ("Would you still like to change it", 0))
        {
	  /*   let the open down the road overwrite it.
          sprintf(buff,"rm -f %s",path);
          system(buff);
	  */
        }
        else 
	{
	    fprintf (stderr, "The projection information will not be updated\n");
	    leave (SP_NOCHANGE);
	}
     }

     G_get_default_window (&cellhd);
     if (-1 == G_set_window (&cellhd))
	 G_fatal_error ("set_window failed");

     if (G_get_set_window (&cellhd) == -1)
	 G_fatal_error ("get_set_window failed");

     Out_proj = cellhd.proj;

     switch (Out_proj) {
     case 0:	/* No projection/units */
	/* leap frog over code, and just make sure we remove the file */
	goto write_file;
	break;
	
     case PROJECTION_UTM:
       sprintf(proj_name,"%s", G__projection_name(PROJECTION_UTM));
       sprintf(proj_out,"utm");
       zone = G_zone();
       break;
     case PROJECTION_SP:
       sprintf(proj_name,"%s", G__projection_name(PROJECTION_SP));
       sprintf(proj_out,"stp");
       break; 
     case PROJECTION_LL:
       sprintf(proj_name,"%s",G__projection_name(PROJECTION_LL));
       sprintf(proj_out,"ll");
       break; 
     case PROJECTION_OTHER:
       while (1)
       {
	 if (G_ask_proj_name(proj_out,proj_name) < 0)
	 {
	   leave (SP_NOCHANGE);
	 }
	 Out_proj = 4;
	 proj_index = get_proj_index(proj_out);
	 if (proj_index == LL || proj_index == UTM)
	   fprintf (stderr, "%c\nProjection 99 does not support UTM or LL\n\n", 7);
	 else break;
       }
       break;
     default: 
       G_fatal_error ("Unknown projection\n");
   }

/*****************   GET spheriod  **************************/
   if (Out_proj !=PROJECTION_SP) {
     sph_check = G_ask_ellipse_name(spheriod);
     if (sph_check < 0) 
       leave (SP_NOCHANGE);

     if (sph_check == 2) {
       radius = prompt_num_double ("Enter radius for the sphere in meters", RADIUS_DEF, 1);
     }
   }
/*** END get spheriod  ***/
	  /* create the PROJ_INFO & PROJ_UNITS files, if required */



    {
      proj_index = get_proj_index(proj_out);
      if (proj_index <0) 
      {
	sprintf(buff,"Projection %s is not specified in the table", proj_out);
	G_fatal_error(buff);
      }

      switch (proj_index) {
	case LL:
	  break;

	case STP:
	  if (Out_proj == PROJECTION_SP) {
	    if(get_stp_code(G_zone(),buffb) == 0)
	    {
	      sprintf(buff,"Invalid State Plane Zone : %d",G_zone());
	      G_fatal_error(buff);
	    }
	  }
	  else {
	    get_stp_proj(buffb);
	  }


	  break;

	default:
	  if (sph_check != 2) {
	    G_strip(spheriod); 
	    if (G_get_ellipsoid_by_name(spheriod,&aa,&e2) == 0) 
	      G_fatal_error("invalid input ellipsoid");
	  }
	  break;
      }
    }


write_file:
    /*
    **  NOTE   the program will (hopeully) never exit abnormally
    **  after this point.  Thus we know the file will be completely
    **  written out once it is opened for write 
    */
    sprintf(buff,"rm -f %s",path);
    system(buff);

    if (Out_proj == 0)
	goto write_units;

    out_proj_keys = G_create_key_value();
    G_set_key_value ("name", proj_name, out_proj_keys);

    /*
    **   Include MISC parameters for PROJ_INFO
    */
    switch (proj_index) 
    { 
    case STP:

      for (i=0; i < strlen(buffb); i++) 
        if (buffb[i] == ' ') {buffb[i] = '\t';}
      sprintf(cmnd2,"%s\t\n",buffb);
      for (i = 0; i < strlen(cmnd2); i++) {
        j = k = 0;
        if (cmnd2[i] == '+') {
          while (cmnd2[++i] != '=') buffa[j++] = cmnd2[i];
          buffa[j] = 0;
          while (cmnd2[++i] != '\t' && cmnd2[i] != '\n' &&cmnd2[i] != 0) 
            buffb[k++] = cmnd2[i];
            buffb[k] = 0;
            G_set_key_value (buffa, buffb, out_proj_keys);
        }
      }
      break;
    case LL:

       G_set_key_value ("proj", "ll", out_proj_keys);
       G_set_key_value ("ellps", spheriod, out_proj_keys);
     break;

    default:  

      if (sph_check != 2)
      {
       G_set_key_value ("proj", proj_out, out_proj_keys);
       G_set_key_value ("ellps", spheriod, out_proj_keys);
       sprintf(tmp_buff,"%.10lf", aa);
       G_set_key_value ("a", tmp_buff, out_proj_keys);
       sprintf(tmp_buff,"%.10lf", e2);
       G_set_key_value ("es", tmp_buff, out_proj_keys);
      }
      else
      {

       G_set_key_value ("proj", proj_out, out_proj_keys);
       G_set_key_value ("ellps", "sphere", out_proj_keys);
       sprintf(tmp_buff,"%.10lf", radius);
       G_set_key_value ("a", tmp_buff, out_proj_keys);
       G_set_key_value ("es", "0.0", out_proj_keys);
      }
      for (i=0; i < NOPTIONS; i++) 
      {
	if (TABLE[proj_index][i].ask == 1) 
	{

	  if (i == SOUTH) 
	  {
	    sprintf (buff, "\nIs this %s ", DESC[i] );
	    if (!G_yes (buff, 1))
            G_set_key_value ("south", "defined", out_proj_keys);
	  }
	  else
	  { 

	    for(;;)
	    {
	     /* G_clear_screen();*/
	      if ((i==LAT0)||(i==LAT1)||(i==LAT2)||(i==LATTS))
		if (get_LL_stuff(1,i)) break;
	      if (i==LON0)
		if (get_LL_stuff(0,i)) break;
	      if (i==ZONE) 
		if(Out_proj==PROJECTION_UTM) 
		  break;
		else
		  if (get_zone()) break;
	      if (i==KFACT)
		if (get_KFACT()) break;
	      if (i==X0)
		if (get_x0()) break;
	    }  /* for */
	  }
	  switch (i) {
	    case LAT0:
	      sprintf(tmp_buff, "%.10lf", LLSTUFF[i]);
              G_set_key_value ("lat_0", tmp_buff, out_proj_keys);
	      break;
	    case LAT1:
	      sprintf(tmp_buff, "%.10lf", LLSTUFF[i]);
              G_set_key_value ("lat_1", tmp_buff, out_proj_keys);
	      strcat(cmnd2,buff);
	      break;
	    case LAT2:
	      sprintf(tmp_buff, "%.10lf", LLSTUFF[i]);
              G_set_key_value ("lat_2", tmp_buff, out_proj_keys);
	      break;
	    case LATTS:
	      sprintf(tmp_buff, "%.10lf", LLSTUFF[i]);
              G_set_key_value ("lat_ts", tmp_buff, out_proj_keys);
	      break;
	    case LON0:
	      sprintf(tmp_buff, "%.10lf", LLSTUFF[i]);
              G_set_key_value ("lon_0", tmp_buff, out_proj_keys);
	      break;
	    case ZONE:
	      if(Out_proj==1) 
	      {
	        sprintf(tmp_buff, "%d", G_zone());
                G_set_key_value ("zone", tmp_buff, out_proj_keys);
              }
	      else
	      {
	        sprintf(tmp_buff, "%d", zone);
                G_set_key_value ("zone", tmp_buff, out_proj_keys);
              }
	      break;
	    case KFACT:
	        sprintf(tmp_buff, "%.10lf", kfact);
                G_set_key_value ("k", tmp_buff, out_proj_keys);
	      break;
	    case X0:
	        sprintf(tmp_buff, "%.10lf", x_false);
                G_set_key_value ("x_0", tmp_buff, out_proj_keys);
	      break;
	    default: break;
	    }
	  } /* ask */ 
	}  /* for */

	break;
  }  /* switch index */


  /* create the PROJ_INFO & PROJ_UNITS files, if required */

  G_write_key_value_file (path, out_proj_keys, &out_stat);
  if (out_stat != 0)
  {
    sprintf(buffb,"Error writting PROJ_INFO file: %s\n", path);
    G_fatal_error(buffb) ;
  }
  G_free_key_value(out_proj_keys);

write_units:
  G__file_name (path, "", UNIT_FILE, set_name);

  /* if we got this far, the user
  ** already affirmed to write over old info
  ** so if units file is here, remove it.
  */
  if (access(path,0) == 0)
  {
    sprintf(buff,"rm -f %s",path);
    system(buff);
  }

  if (Out_proj == 0)
    leave (0);
    

  {
    in_unit_keys = G_create_key_value();

    switch (Out_proj) { 
    case PROJECTION_UTM: 
      G_set_key_value("unit", "meter", in_unit_keys);
      G_set_key_value("units", "meters", in_unit_keys);
      G_set_key_value("meters", "1.0", in_unit_keys);
      break;
    case PROJECTION_SP: 
      G_set_key_value("unit", "foot", in_unit_keys);
      G_set_key_value("units", "feet", in_unit_keys);
      G_set_key_value("meters", "0.30479999", in_unit_keys);
      break;
    case PROJECTION_LL: 
      G_set_key_value("unit", "degree", in_unit_keys);
      G_set_key_value("units", "degrees", in_unit_keys);
      G_set_key_value("meters", "1.0", in_unit_keys);
      break;
    default: 
      if (proj_index != LL) {
	/* G_clear_screen();*/
	fprintf(stderr, "Enter plural form of units [meters]:");
	G_gets(answer);
	if (strlen(answer) == 0)
	{
          G_set_key_value("unit", "meter", in_unit_keys);
          G_set_key_value("units", "meters", in_unit_keys);
          G_set_key_value("meters", "1.0", in_unit_keys);
        }
	else {
	  G_strip(answer);
	  npr = strlen(answer);
	  for (i = 0; i < NUNITS; i++) {
	    in_stat = min1(npr,strlen(UNITS[i].units));
	    if(strncmp(UNITS[i].units,answer,in_stat) == 0) {
	      unit_fact = UNITS[i].fact;
	      break;
	    }
	  }
	  if (i < NUNITS) {
#ifdef FOO
	    if (proj_index == STP && !strcmp (answer, "feet"))
	    {
	      fprintf (stderr, "%cPROJECTION 99 State Plane cannot be in FEET.  Use Projection type 2.\n", 7);
              sprintf(buff,"rm -f %s",path);
	      system(buff);	/* remove file */

	      leave (SP_FATAL);
	    }
#endif
	      
            G_set_key_value("unit", UNITS[i].unit, in_unit_keys);
            G_set_key_value("units", answer, in_unit_keys);
	    sprintf(buffb,"%.10lf", unit_fact);
            G_set_key_value("meters", buffb, in_unit_keys);
	  }
	  else {
	    while (1)
	    {
	      fprintf(stderr, "Enter singular for unit:");
	      G_gets(answer1);
	      G_strip (answer1);
	      if (strlen(answer1) > 0) 
		break;
	    }

	    while (1)
	    {
	      fprintf(stderr,
		 "Enter conversion factor from %s to meters:",answer);
	      G_gets(answer2);
	      G_strip (answer2);
	      if (!(strlen(answer2) == 0 || (1 != sscanf (answer2, "%lf", &unit_fact))))
		break;
	    }
            G_set_key_value("unit", answer1, in_unit_keys);
            G_set_key_value("units", answer, in_unit_keys);
	    sprintf(buffb,"%.10lf", unit_fact);
            G_set_key_value("meters", buffb, in_unit_keys);
	  }
	}
      }
      else 
      {
            G_set_key_value("unit", "degree", in_unit_keys);
            G_set_key_value("units", "degrees", in_unit_keys);
            G_set_key_value("meters", "1.0", in_unit_keys);
      }
    } /* switch */

    G_write_key_value_file (path, in_unit_keys, &out_stat);
    if(out_stat!=0)
    {
      sprintf(buffb,"Error writing into UNITS output file: %s\n", path) ;
      G_fatal_error(buffb) ;
    }
    G_free_key_value(in_unit_keys);
  } /* if */



  fprintf (stderr, "\nProjection information has been recorded for this location\n\n");
  leave (0);
}


int min1(a,b)
int a,b;
{
if (a>b) return b;
else return a;
}


leave (val)
int val;
{
exit (val);
}

