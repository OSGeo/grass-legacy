
#include <keyframe.c>

float OWsize, Vmax[3], Vmin[3], Vmid[3]; /*ortho window size and view bounds*/

gkv_get_viewbounds(view, nv)
Viewnode *view;
int nv;
{
int i;
Viewnode *v;

    Vmax[X] = Vmax[Y] = Vmax[Z] = -10000.;    
    Vmin[X] = Vmin[Y] = Vmin[Z] = 10000.;    

    for(i=0; i<nv; i++){
	v = &view[i];
	Vmax[X] = v->fields[KF_FROMX] > Vmax[X]? v->fields[KF_FROMX] : Vmax[X];
	Vmax[Y] = v->fields[KF_FROMY] > Vmax[Y]? v->fields[KF_FROMY] : Vmax[Y];
	Vmax[Z] = v->fields[KF_FROMZ] > Vmax[Z]? v->fields[KF_FROMZ] : Vmax[Z];
	Vmin[X] = v->fields[KF_FROMX] < Vmin[X]? v->fields[KF_FROMX] : Vmin[X];
	Vmin[Y] = v->fields[KF_FROMY] < Vmin[Y]? v->fields[KF_FROMY] : Vmin[Y];
	Vmin[Z] = v->fields[KF_FROMZ] < Vmin[Z]? v->fields[KF_FROMZ] : Vmin[Z];
    }
    OWsize = .7 * distance(Vmin, Vmax);

    if(!OWsize){
	Vmax[X] = X_Max;
	Vmax[Y] = Y_Max;
	Vmax[Z] = Z_Max;
	Vmin[X] = X_Min;
	Vmin[Y] = Y_Min;
	Vmin[Z] = Z_Min;
	OWsize = .7 * distance(Vmin, Vmax);
    }

    Vmid[X] = (Vmax[X] + Vmin[X]) / 2.;
    Vmid[Y] = (Vmax[Y] + Vmin[Y]) / 2.;
    Vmid[Z] = (Vmax[Z] + Vmin[Z]) / 2.;

}

gkv_do_ortho_displays()
{
Screencoord x1;
float asp;

	x1 = right/3;   
	asp = right/top * 4./3.;
	/* right and top of grid display window never changes, only bottom */

	gsd_viewport(0, x1, 0, top/4);    
	loadmatrix(ID_matrix);
	ortho(-OWsize*asp,OWsize*asp,-OWsize,OWsize, 100, 9000);
	/* top */
	lookat(Vmid[X],Vmid[Y], 5000, Vmid[X],Vmid[Y],Vmid[Z], 0);
	gsd_color_func(BGcolor);
	clear();
	zwritemask (0);
	gsd_color_func(~(BGcolor | 0xFF0000));
	cmov(Vmin[X], Vmid[Y] + OWsize * .8, Vmid[Z]);
	charstr("Top View");
	zwritemask (0xffffffff);
	k_show_path();

	gsd_viewport(x1, 2*x1, 0, top/4);    
	loadmatrix(ID_matrix);
	/* south */
	lookat(Vmid[X], -5000, Vmid[Z], Vmid[X], Vmid[Y], Vmid[Z], 0);
	gsd_color_func(BGcolor);
	clear();
	zwritemask (0);
	gsd_color_func(~(BGcolor | 0xFF0000));
	cmov(Vmin[X], Vmid[Y], Vmid[Z] + OWsize * .8);
	charstr("South View");
	zwritemask (0xffffffff);
	k_show_path();

	gsd_viewport(2*x1, right , 0, top/4);    
	loadmatrix(ID_matrix);
	/* east */
	lookat(5000, Vmid[Y], Vmid[Z], Vmid[X], Vmid[Y], Vmid[Z], 900);
	gsd_color_func(BGcolor);
	clear();
	zwritemask (0);
	gsd_color_func(~(BGcolor | 0xFF0000));
	cmov(Vmid[X], Vmin[Y], Vmid[Z] + OWsize * .8);
	charstr("East View");
	zwritemask (0xffffffff);
	k_show_path();

	gsd_viewport(0, right , top/4, top);    
	update_projection ();
	_do_fast_display();
}

void
gkv_do_viewports()
{
    if(!Views){
	Ashowkpath->val = 0;
	pnl_fixact(Ashowkpath);
	return;
    }
    if(Ashowkpath->val){
	get_viewbounds(Views, Viewsteps);
	do_ortho_displays();	
    }
    else{
	gsd_viewport(0, right, 0 , top);
	update_projection ();
	do_fast_display();
    }
}
