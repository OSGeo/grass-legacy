/* %W% %G% */
#include "usgs.h"

get_hdr()
{
	int		i;
	float dummy;
	float angle;

	buffer = buf_start;
	if(!(filestat = get_buf())) return(0);

	for(i = 0; i < 50; i++) name[i] = buffer[i];
	name[50] = 0;

	buffer += 144;
	buffer += get_int(&DEM);
	buffer += get_int(&pattern);
	buffer += get_int(&ref_sys);
	buffer += get_int(&ref_zone);

	for (i = 0; i < 15; i++)
	    buffer+=get_dfloat (&dummy);

	buffer += get_int(&xy_unit);
	buffer += get_int(&z_unit);
	buffer += get_int(&sides);

	for(i = 0; i < sides; i++){
		buffer += get_dfloat(&east[i]);
		buffer += get_dfloat(&north[i]);
	}

	buffer += get_dfloat(&min_elev);
	buffer += get_dfloat(&max_elev);

	buffer += get_dfloat (&angle);

/* now skip over accuracy code */
	while (*buffer++ == ' ')
		;


	buffer += get_efloat(&x_res);
	buffer += get_efloat(&y_res);
	buffer += get_efloat(&z_res);

	if(!x_res) x_res = y_res;
	if(!y_res) y_res = x_res;

	buffer += get_int(&P_rows);
	buffer += get_int(&P_cols);

	file_north= 0;
	file_south= 9999999.;
	file_east= 0;
	file_west= 9999999;

	for(i = 0; i < sides; i++){
		if(north[i] > file_north) file_north = north[i];
		if(north[i] < file_south) file_south = north[i];
		if(east[i]  > file_east)  file_east  = east[i];
		if(east[i]  < file_west)  file_west  = east[i];
	}

	return(1);
}

hdr_list(file)
FILE *file;
{
	int		i;

	for(i = 0 ; i < 80; i++) fprintf(file,"-");
	fprintf(file,"\n\n"); fflush (file);
	fprintf(file,"%s\n",name);
	fprintf(file,"\n");
	fprintf(file,"min elevation: %f  max elevation: %f\n",min_elev,max_elev);
	fprintf(file,"\n");
	fprintf(file,"ns_res: %f  ew_res: %f\n",x_res,y_res);
	fprintf(file,"# of columns in file =   %d\n",P_cols);
	fprintf(file,"\n");
}

window_list(file)
FILE *file;
{
	int		i;

	fprintf(file,"Current Window Settings-----------------------------\n");
	fprintf(file,"rows:       %d\n",cellhd.rows);
	fprintf(file,"cols:       %d\n",cellhd.cols);
	fprintf(file,"north:      %lf\n",cellhd.north);
	fprintf(file,"south:      %lf\n",cellhd.south);
	fprintf(file,"east:       %lf\n",cellhd.east);
	fprintf(file,"west:       %lf\n",cellhd.west);
	fprintf(file,"ns_res:     %lf\n",cellhd.ns_res);
	fprintf(file,"ew_res:     %lf\n",cellhd.ew_res);
	fprintf(file,"\n");
}
