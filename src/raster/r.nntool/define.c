#include "globals.h"

int cl, ask;
static int use=1;
extern int pc, cnt, *store; /* moved from within define_region() */
extern struct Cell_head cellhd; /* moved from within draw_region() */


define_region() 
{
int done(), draw_region(), display_region(), erase_site(); 
int analyze_sig(), mainm();
char tmp[10];
extern char group[10];

	static Objects objects[] =
	{
	 INFO("Train:",&use),
	 MENU(" Mark ", draw_region,&use),
         MENU(" Analyze ", analyze_sig,&use),
	 MENU(" Display ", display_region,&use),
	 MENU(" Erase ", erase_site,&use),
	 MENU(" Done ", done, &use),
         MENU(" Main ", mainm, &use),
	 {0}
	};
 
	static Objects object2[] = 
	{
	INFO("Train:",&use),
	MENU(" Mark ",draw_region,&use),
	MENU(" Display ", display_region,&use),
	MENU(" Erase ", erase_site,&use),
	MENU(" Done ", done, &use),
	MENU(" Main ", mainm, &use),
	{0}
	};
	
	Curses_prompt_gets("What class is this ? ",tmp);
	cl = atoi(tmp);
	use_mouse_msg();
	if(store[cl] == 0)
		{ store[cl] = cl; cnt=0; pc = 1; }
	ask = cl;

        if(group[0] == 'Y' || group[0] == 'y')
	  Input_pointer (objects);
        else
          Input_pointer (object2);

	return(0);
}

mainm()
{
return(-1);
}

done()
{
int ctmp;
extern nxz, nyz;
char tmp[10];

  mask();
/*  Curses_prompt_gets("Return to the previous menu [0/1]? ",tmp);
  ctmp = atoi(tmp);
  use_mouse_msg();
  if(ctmp) {
    nxz=nyz=0;
    return(-1);
  }
  else return(0); */

  return(0);
}

draw_region()
{
FILE *fd;
int nxi, nyi, button, radx, rady, x=0, y=1, edg, MORE, radbut,class, rad;
int i=0, px, py, endx, endy, firstx, firsty, t_x, t_y, r_x, r_y;
int tnyi, tnxi, tradx, trady;
extern int nxz, nyz, area[25][50], no_tr[3][20][20];
extern int Noedg[25][100], *Nopc, nxm[20][100], nym[20][100];
extern struct coord polygon[25][50][50]; 
extern int *CNT, *ftr, nrows, rm[20][100];
float radius;
double D_d_to_u_row(), D_d_to_u_col(), D_d_to_a_col();
double D_get_d_west(), D_get_d_south(), D_d_to_a_row();
CELL *pcellm, *G_allocate_cell_buf();
struct Cell_head Twindow;
char tmpstr[50], temp[20];
extern long **MAP[10];
double tmp_n, tmp_e, D_d_to_a_col(), D_d_to_a_row();


label:{
	Curses_prompt_gets("(point=0) (circular=1) (polygon=2) (Quit=-1) ",
				temp);
	area[ask][cnt] = atoi(temp);
        if(area[ask][cnt] == -1) return;

	use_mouse_msg();
 
        nxi = (int)(D_get_d_west());
        nyi = (int)(D_get_d_south());
        Mouse_pointer(&nxi,&nyi,&button);

	if(In_view(VIEW_MAP1, nxi, nyi))
		Region.view = VIEW_MAP1;
	else if(In_view(VIEW_MAP1_ZOOM, nxi, nyi))
		Region.view = VIEW_MAP1_ZOOM;

	if (area[ask][cnt] == 0) { /* Point */
          Menu_msg("Left=Select    Right = Done");
          while(button != 3) {
            tnxi = view_to_col(Region.view, nxi);
            tnyi = view_to_row(Region.view, nyi);
	    tmp_n = row_to_northing(&(Region.view->cell.head), tnyi, 0.5);
	    tmp_e = col_to_easting(&(Region.view->cell.head), tnxi, 0.5);
            nxm[ask][no_tr[area[ask][cnt]][ask][cnt]+i] =
		easting_to_col(&cellhd, tmp_e);
            nym[ask][no_tr[area[ask][cnt]][ask][cnt]+i] =
		northing_to_row(&cellhd, tmp_n);

	    add_point(nxi, nyi);
            Mouse_pointer(&nxi,&nyi,&button);
            i++;
          }
          no_tr[area[ask][cnt]][ask][cnt] = i;
        }
	else if(area[ask][cnt] == 1) { /* Circular */
          Menu_msg("Left = Select   Any = Radius    Right = Done");
          while(button != 3) {
            Mouse_line_anchored(nxi,nyi,&radx,&rady,&radbut);
 
            tradx = view_to_col(Region.view, radx);	/* + nxz; */
	    tmp_e = col_to_easting(&(Region.view->cell.head), tradx, 0.5);
	    r_x = easting_to_col(&cellhd, tmp_e);

            trady = view_to_row(Region.view, rady);	/* + nyz; */
	    tmp_n = row_to_northing(&(Region.view->cell.head), trady, 0.5);
	    r_y = northing_to_row(&cellhd, tmp_n);

	    tnxi = view_to_col(Region.view, nxi);
	    tmp_e = col_to_easting(&(Region.view->cell.head), tnxi, 0.5);
	    t_x = easting_to_col(&cellhd, tmp_e);
            nxm[ask][no_tr[area[ask][cnt]][ask][cnt]+i] = t_x;

	    tnyi = view_to_row(Region.view, nyi);
	    tmp_n = row_to_northing(&(Region.view->cell.head), tnyi, 0.5);
	    t_y = northing_to_row(&cellhd, tmp_n);
            nym[ask][no_tr[area[ask][cnt]][ask][cnt]+i] = t_y;

            rm[ask][no_tr[area[ask][cnt]][ask][cnt]+i] =
                (int)(sqrt(pow((r_x-t_x)*1.,2.)+pow((r_y - t_y)*1.,2.)));

	    /* rough cut of a circle! */
	    rad = (int)(sqrt(pow((radx-nxi)*1.,2.)+pow((rady-nyi)*1.,2.)));

	    Region.npoints = 0;
	    add_point(nxi,nyi+rad); add_point(nxi+rad,nyi); 
	    add_point(nxi,nyi-rad); add_point(nxi-rad,nyi);
	    R_cont_abs(nxi,nyi+rad);

	    Region.area.completed = 1;
	    Region.area.define = 1;
	    save_region();

            Mouse_pointer(&nxi,&nyi,&button);
            i++;
          }
          no_tr[area[ask][cnt]][ask][cnt] = i;
	}
	else if(area[ask][cnt] == 2) { /* polygon */
          Menu_msg("(Left=Select) (Middle=End Polygon) (Right = Done)");
          edg = 1;  Noedg[ask][pc] = edg;

	  Region.npoints = 0;
	  Region.area.define = 1;

          while(button != 3) {
            Mouse_line_anchored(nxi,nyi,&endx,&endy,&button);
            R_move_abs(nxi,nyi);
            R_cont_abs(endx,endy);
            if(edg == 1) {
              firstx = nxi;     firsty = nyi; /* The 1st location */
            }
            px = view_to_col(Region.view, nxi);
	    tmp_e = col_to_easting(&Region.view->cell.head, px, 0.5);
	    px = easting_to_col(&cellhd, tmp_e);
            py = view_to_row(Region.view, nyi);
	    tmp_n = row_to_northing(&Region.view->cell.head, py, 0.5);
	    py = northing_to_row(&cellhd, tmp_n);

            polygon[ask][pc][edg].x1 = px; 	/* + nxz; */
            polygon[ask][pc][edg].y1 = py;	/*  + nyz; */
	    add_point(nxi, nyi);
 
            px = view_to_col(Region.view, endx);
            tmp_e = col_to_easting(&Region.view->cell.head, px, 0.5);
            px = easting_to_col(&cellhd, tmp_e);
            py = view_to_row(Region.view, endy);
	    tmp_n = row_to_northing(&Region.view->cell.head, py, 0.5);
            py = northing_to_row(&cellhd, tmp_n);
            polygon[ask][pc][edg].x2 = px; 	/* + nxz; */
            polygon[ask][pc][edg].y2 = py;	/*  + nyz; */
            ++edg;
 
            nxi = endx;         nyi = endy;
	    Region.area.define=0;	
            if(button == 2) {
	      add_point(endx, endy);
              R_move_abs(nxi,nyi);
              R_cont_abs(firstx,firsty);

	      py = view_to_row(Region.view, endy);
	      tmp_n = row_to_northing(&Region.view->cell.head, py, 0.5);
              py = northing_to_row(&cellhd,tmp_n);
	      px = view_to_col(Region.view, endx);
              tmp_e = col_to_easting(&Region.view->cell.head, px, 0.5);
              px = easting_to_col(&cellhd, tmp_e);
 
              polygon[ask][pc][edg].x1 = px;	/* + nxz; */
              polygon[ask][pc][edg].y1 = py;	/* + nyz; */
              polygon[ask][pc][edg].x2 = polygon[ask][pc][1].x1;
              polygon[ask][pc][edg].y2 = polygon[ask][pc][1].y1;

	      Region.area.completed = 1;
	      save_region();

              Noedg[ask][pc] = edg; Nopc[ask]++;
              pc++;
              Mouse_pointer(&nxi,&nyi,&button);
              edg = 1;
	      if(In_view(VIEW_MAP1, nxi, nyi))
                Region.view = VIEW_MAP1;
              else if(In_view(VIEW_MAP1_ZOOM, nxi, nyi))
                Region.view = VIEW_MAP1_ZOOM;

	      Region.area.define = 1;
	      Region.npoints = 0;
	      SA++;  /* increment polygon counter */
            }
          }
        }

/*	sprintf(tmpstr,"You want any other type areas for class %d [0/1] ",ask);
	Curses_prompt_gets(tmpstr, temp);
	MORE = atoi(temp); */

/*        if(MORE == YES) {
          cnt++;
          CNT[ask] = cnt;
          goto label;
        } */
/*        else { */

	  i = 0;
          cnt++;
          CNT[ask] = cnt;
	  use_mouse_msg();
	  Menu_msg("");
	  return(0);

/*        } */
/*	return(0); */
      }
}

display_region()
{
int class;
char tmpstr[25];

	Curses_prompt_gets("Which class do you want displayed ? ",tmpstr);
    	class = atoi(tmpstr);
	sprintf(tmpstr,"Displaying class %d",class);
    	Menu_msg(tmpstr);
    	sprintf(tmpstr,"train.cls%d",class);
    	mydraw(VIEW_MAP1,OVER_LAY,tmpstr);
	Menu_msg("");
	return(0);
}

erase_site()
{
int mapx, mapy, i, numx, numy, dy, ftemp;
int succeed, dx, nx, ny, query, button, px, py;
extern int Noclass, nrows, ncols;
char tmpstr[20], sfile[10], *mapset;
double tmp_e, tmp_n;
extern long **MAP[10];

	Curses_prompt_gets("Which class does this area belong to ?",tmpstr);
	cl = atoi(tmpstr);	
	if(cl < 1 || cl > Noclass) {
		Menu_msg("No such class!");
		return(0);
/*		define_region(); */
	}
	sprintf(sfile,"train.cls%d",cl);
	mapset = G_find_cell(sfile,"");
	use_mouse_msg();

la:     Menu_msg("Press at the center of the area you want deleted");
        Mouse_pointer(&nx,&ny,&button);

	if(In_view(VIEW_MAP1, nx, ny))
		Region.view = VIEW_MAP1;
	else if(In_view(VIEW_MAP1_ZOOM, nx, ny))
		Region.view = VIEW_MAP1_ZOOM;

        px = view_to_col(Region.view,nx);
	tmp_e = col_to_easting(&(Region.view->cell.head), px, 0.5);
	mapx = easting_to_col(&cellhd, tmp_e);

        py = view_to_row(Region.view,ny);
	tmp_n = row_to_northing(&(Region.view->cell.head), py, 0.5);
	mapy = northing_to_row(&cellhd, tmp_n);

        numx = numy = 0;
        while(1) {
                numx++; numy++;
                succeed = 0;
                for(dy=-numy;dy <= numy && mapy+dy < nrows;dy++)
                        for(dx=-numx;dx <= numx && mapx+dx < ncols;dx++)
                                if(MAP[cl][mapy+dy][mapx+dx] != 0) {
                                        succeed++;
                                        MAP[cl][mapy+dy][mapx+dx] = 0;
                                }
                if(succeed == 0)
                        break;
        }

/*        Curses_prompt_gets("Delete any more for this class [Y/N]? ",tmpstr);
        if(tmpstr[0] == 'Y' || tmpstr[0] == 'y')
                goto la;
        else { */

	G_set_window(&cellhd);
	ftemp = G_open_cell_new(sfile,mapset);
        for(i=0;i < nrows;i++)
		G_put_map_row(ftemp,MAP[cl][i]);
	G_close_cell(ftemp);
	return(0);

/*	define_region(); */
/*        }  */
}

mydraw(view,overlay,name)
	View *view;
	int overlay;
	char name[];
{
  int fd;
  int left, top;
  int nncols, nnrows;
  int row;
  CELL *cell;
  struct Colors colr;
  int repeat;
  char msg[100];


  if (!view->cell.configured) return 0;
  if(G_read_colors (name, view->cell.mapset, &colr) < 0)
    return 0;

  if (overlay == OVER_WRITE)
    display_title (view);

  D_set_colors (&colr);
 
  G_set_window (&view->cell.head);
  nnrows = G_window_rows();
  nncols = G_window_cols();
 
  left = view->cell.left;
  top = view->cell.top;
 
  R_standard_color (BLUE);
  Outline_box (top, top+nnrows-1, left, left+nncols-1);
 
  {char *getenv(); if (getenv("NO_DRAW")){G_free_colors(&colr); return 1;} }
 
  fd = G_open_cell_old (name, view->cell.mapset);
  if (fd < 0)
    {
      G_free_colors (&colr);
      return 0;
    }
  cell = G_allocate_cell_buf();
    
 
  sprintf (msg, "Plotting %s ...", name);
  Menu_msg(msg);
 
  D_set_overlay_mode(!overlay);
  for (row = 0; row < nnrows; row += repeat)
    {
      R_move_abs (left, top+row);
      if(G_get_map_row_nomask(fd, cell, row) < 0)
        break;
      repeat = G_row_repeat_nomask (fd, row);
      D_raster (cell, nncols, repeat, &colr);
    }
  G_close_cell (fd);
  free (cell);
  G_free_colors (&colr);
 
  return row==nnrows;
}
