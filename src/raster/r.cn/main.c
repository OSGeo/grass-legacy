/* %W% %G% */

/*
 * January, 1991 Agricultural Engineering, Purdue University Raghavan
 * Srinivasan (srin@ecn.purdue.edu)
 * 
 * r.cn()
 * 
 * This program generates a Curve Number (CN) map based on the four input map
 * layers include, hydrological soil group, land- use, practice or
 * management, hydrological condition maps. This program follows the SCS
 * CN table.
 */

#include "cn.h"


void main (argc, argv)
  int argc;
  char *argv[];
{
  char buf[100], *title[100];

  int i, j;
  int row_id, col_id;
  int G_get_map_row_nomask ();
  int G_parser();
  int cell_open (), cell_open_new ();
  int rules (), hy_soil_group (), amc_conversion ();
  CELL *hy_soil_group_rbuf, *veg_cover_rbuf, *hy_cond_rbuf;
  CELL *land_use_rbuf, *cn_rbuf;
  int hy_soil_group_id, veg_cover_id, cn_id;
  int hy_cond_id, land_use_id;
  int amc;
  struct
  {
    struct Option *sg, *lu, *pr, *hc, *cn, *amc;
  } parm;

  G_gisinit (argv[0]);

  parm.sg = G_define_option ();
  parm.sg->key = "sg";
  parm.sg->type = TYPE_STRING;
  parm.sg->required = YES;
  parm.sg->description = "hydrological soil group map layer";

  parm.lu = G_define_option ();
  parm.lu->key = "lu";
  parm.lu->type = TYPE_STRING;
  parm.lu->required = YES;
  parm.lu->description = "land use map layer";

  parm.pr = G_define_option ();
  parm.pr->key = "pr";
  parm.pr->type = TYPE_STRING;
  parm.pr->required = YES;
  parm.pr->description = "vegetation cover map layer";

  parm.hc = G_define_option ();
  parm.hc->key = "hc";
  parm.hc->type = TYPE_STRING;
  parm.hc->required = YES;
  parm.hc->description = "hydrological condition map layer";

  parm.cn = G_define_option ();
  parm.cn->key = "cn";
  parm.cn->type = TYPE_STRING;
  parm.cn->required = YES;
  parm.cn->description = "curve number map layer";

  parm.amc = G_define_option ();
  parm.amc->key = "amc";
  parm.amc->type = TYPE_INTEGER;
  parm.amc->required = YES;
  parm.amc->description = "AMC condition number";
  /* parm.amc->answer = "2"; */
  parm.amc->options = "1-3";

  if (G_parser (argc, argv))
    exit (1);
  sscanf (parm.amc->answer, "%d", &amc);

  hy_soil_group_name = parm.sg->answer;
  land_use_name = parm.lu->answer;
  veg_cover_name = parm.pr->answer;
  hy_cond_name = parm.hc->answer;
  cn_name = parm.cn->answer;

  /* get the current mapset */
  this_mapset = G_mapset ();

  /*
   * if curver number output map exists in the mapset then print error
   * message and quit
   */

  cn_mapset = G_find_cell2 (cn_name, this_mapset);
  if (cn_mapset)
  {
    sprintf (buf, "curve number file [%s] already exists\n", cn_name);
    G_fatal_error (buf);
    exit (1);
  }

  /* find all the map in the mapset and get their mapset location */

  hy_soil_group_mapset = G_find_cell2 (hy_soil_group_name, "");
  if (!hy_soil_group_mapset)
  {
    sprintf (buf, "hydrological soil group file [%s] not found\n",
	     hy_soil_group_name);
    G_fatal_error (buf);
    exit (1);
  }

  land_use_mapset = G_find_cell2 (land_use_name, "");
  if (!land_use_mapset)
  {
    sprintf (buf, "landuse file [%s] not found\n", land_use_name);
    G_fatal_error (buf);
    exit (1);
  }

  veg_cover_mapset = G_find_cell2 (veg_cover_name, "");
  if (!veg_cover_mapset)
  {
    sprintf (buf, "vegetation cover file [%s] not found\n", veg_cover_name);
    G_fatal_error (buf);
    exit (1);
  }

  hy_cond_mapset = G_find_cell2 (hy_cond_name, "");
  if (!hy_cond_mapset)
  {
    sprintf (buf, "hydrological condition file [%s] not found\n", hy_cond_name);
    G_fatal_error (buf);
    exit (1);
  }

  /* get the window information  */
  G_get_set_window (&window);
  nrows = G_window_rows ();
  ncols = G_window_cols ();

  /* open the map and get their file id  */

  hy_soil_group_id = cell_open (hy_soil_group_name, hy_soil_group_mapset);
  veg_cover_id = cell_open (veg_cover_name, veg_cover_mapset);
  hy_cond_id = cell_open (hy_cond_name, hy_cond_mapset);
  cn_id = cell_open_new (cn_name);
  land_use_id = cell_open (land_use_name, land_use_mapset);


  /* get the category names and cell title */

  if (G_read_cats (hy_soil_group_name, hy_soil_group_mapset,
		   &hy_soil_group_cats) < 0)
    exit (-1);
  if (G_read_cats (land_use_name, land_use_mapset, &land_use_cats) < 0)
    exit (-1);
  if (G_read_cats (veg_cover_name, veg_cover_mapset, &veg_cover_cats) < 0)
    exit (-1);
  if (G_read_cats (hy_cond_name, hy_cond_mapset, &hy_cond_cats) < 0)
    exit (-1);


  /* assign curve number values for the array hy_soil_cover  */
  data ();

  /* allocate cell buf for all the map layers */
  hy_soil_group_rbuf = G_allocate_cell_buf ();
  veg_cover_rbuf = G_allocate_cell_buf ();
  hy_cond_rbuf = G_allocate_cell_buf ();
  land_use_rbuf = G_allocate_cell_buf ();
  cn_rbuf = G_allocate_cell_buf ();

  for (i = 0; i < window.rows; i++)
  {
    G_get_map_row (veg_cover_id, veg_cover_rbuf, i);
    G_get_map_row (hy_soil_group_id, hy_soil_group_rbuf, i);
    G_get_map_row (hy_cond_id, hy_cond_rbuf, i);
    G_get_map_row (land_use_id, land_use_rbuf, i);

    G_zero_cell_buf (cn_rbuf);

    for (j = 0; j < window.cols; j++)
    {
      if (hy_soil_group_rbuf[j] > 0)
      {
	/*
	 * check the rules for the practice, landuse and hydrological
	 * condition and get the row number from the CN table
	 */

	row_id = rules (veg_cover_rbuf[j], land_use_rbuf[j], hy_cond_rbuf[j]);

	/*
	 * check the hydrological soil group and get the column from the
	 * CN table
	 */

	col_id = hy_soil_group (hy_soil_group_rbuf[j]);

	/* if AMC is 2 then assign the corresponding CN from the table */
	if (amc == 2)
	  cn_rbuf[j] = hy_soil_cover[row_id][col_id];
	/* else convert the CN to the selected AMC condition */
	else
	  cn_rbuf[j] = amc_conversion (hy_soil_cover[row_id][col_id], amc);
      }
    }
    G_put_map_row (cn_id, cn_rbuf);
  }

  G_close_cell (hy_soil_group_id);
  G_close_cell (veg_cover_id);
  G_close_cell (land_use_id);
  G_close_cell (hy_cond_id);
  G_close_cell (cn_id);

  /* write the appropriate CN title in the curver number map generated */
  if (amc == 1)
    G_strcpy (title, "Curve Number Map for AMC I");
  if (amc == 2)
    G_strcpy (title, "Curve Number Map for AMC II");
  if (amc == 3)
    G_strcpy (title, "Curve Number Map for AMC III");

  G_put_cell_title (cn_name, title);
  exit(0);
}
