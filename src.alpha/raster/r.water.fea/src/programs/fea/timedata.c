#include "gis.h"
#include "fea.h"
#include <math.h>
#define RAISE 5.0/3.0

static double tot_length = 0.0;
static void
calc_length()
{
	int i,j,temp1,temp2;
		for(i=0;i<max -1;i++){
			temp1 = nin[i] -1;
			temp2 = nout[i] -1;

			le[i] = sqrt(((easting[temp1]-easting[temp2])*(easting[temp1]-easting[temp2]))+((northing[temp1]-northing[temp2])*(northing[temp1]-northing[temp2])));
		}
		for(i=0;i<max -1;i++)
			tot_length += le[i];
	/* Include starting node length ie. half length */
		for(i=0;i<nsn;i++)
			for(j=0; j< max-1;j++)
				if(sn[i] == nin[j]){
					tot_length += le[j]/2;
				}
}
void
timedata()
{
	int i,bn,temp,akchar;
	double ch_length,ch_manning;
	double *slope;
	void get_cell_values();
	void const_value();
	FILE *fptd ; /* fptd =>> file pointer to timedata */

	fptd = (FILE *)G_fopen_old(element,"timedata",G_mapset());
	if(fptd == NULL){
		fprintf(stderr,"timedata cannot be opened.\n");
		exit(8);
	}

	slope = (double *) malloc(max * sizeof(double));
	if(slope == NULL){
		fprintf(stderr,"Insufficient memory\n");
		exit(1);
	}

	fscanf(fptd,"%*s %d",&duration);
	fscanf(fptd,"%*s %lf",&rain_max);
	fscanf(fptd,"%*s %d",&delta_t);
	fscanf(fptd,"%*s %d",&monit_time);
	fscanf(fptd,"%*s %s",timefile);
	fscanf(fptd,"%*s %d",&infilmap);

	if(infilmap == 1){
		fscanf(fptd,"%*s %s",manningsmap);
		fscanf(fptd,"%*s %s",sat_cond_map);
		fscanf(fptd,"%*s %s",cap_suc_map);
		fscanf(fptd,"%*s %s",porosity_map);
	}
	/* Obtaining the correct channel values */
	while ((akchar = getc(fptd)) != EOF)
	if(akchar == 'C'){	
	   	fscanf(fptd,"%d",&bn);	
		if(bn == basin_no){
			fscanf(fptd,"%lf%lf%lf%lf",&ch_length,&nslope,&base,&ch_manning);
			break;
		}
	}

	/* MODE 3 requires file - to be dealt in the next version */
	/* Extracting data for slopes */
	get_cell_values(easting,northing,max,slope,slopemap);
	for(i=0;i<max;i++){
		if(slope[i] <= 0.0)
			slope[i] = 5.0;  /* default, woolhiser(1975) */
		slope[i] /= 100.0;
	}

	if(infilmap == 1){
		get_cell_values(easting,northing,max,manning,manningsmap);
		for(i=0; i<max; i++){
			if(manning[i] <= 0.0)
				manning[i] = 35; /*default*/
			manning[i] = manning[i]/1000.0;
		}
	}
	else{
		fprintf(stderr,"[Please enter basin overland flow parameters now.]\n");
		const_value("Mannings roughness coefficient",max,manning,0.8,0.01);
	}

	/* change mannings values for the stream nodes */
		for(i=0;i < no_stream; i++){
			temp = stream_node[i] - 1;
			manning[temp] = ch_manning;
		}
	/* Modifying the mannings array for overland flow */
	for(i=0;i< max;i++)
		manning[i] = manning[i]/sqrt((double)slope[i]);
	calc_length();
	width = (area - (base*ch_length))/(tot_length - ch_length);
	free(slope);
	fclose(fptd);
}
