#include "netcdf.h"
#include "gis.h"
#include <stdio.h>
#define VERBOSE 1
#define NOT_VERBOSE 0

typedef struct HRAP {
	int x, y;
} HRAP;

typedef struct LL {
	float lat, lon;
} LL;

char mapname[512];
char title[512];
double true_lat, true_lon;
int verbose=NOT_VERBOSE;

void
get_netcdf_info(ncdfile, title, precip_id)
int ncdfile;
char *title;
int *precip_id;
{
	char *buffer;
	int id;
	nc_type vr_type;
	int vr_len;

	*precip_id = ncvarid(ncdfile,"amountofprecip");

	ncattinq(ncdfile, *precip_id, "long_name", &vr_type, &vr_len);
	buffer = (char *) malloc((vr_len + 1) *nctypelen(vr_type));
	ncattget(ncdfile, *precip_id, "long_name", (void *) buffer);
	buffer[vr_len] = '\0';
	sprintf(title,"%s, ", buffer);
	free(buffer);

	ncattinq(ncdfile, *precip_id, "units", &vr_type, &vr_len);
	buffer = (char *) malloc((vr_len+ 1) *nctypelen(vr_type));
	ncattget(ncdfile, *precip_id, "units", (void *) buffer);
	buffer[vr_len] = '\0';
	strcat(title, buffer);
	strcat(title,", ");
	free(buffer);

	ncattinq(ncdfile, *precip_id, "dateofdata", &vr_type, &vr_len);
	buffer = (char *) malloc((vr_len + 1) *nctypelen(vr_type));
	ncattget(ncdfile, *precip_id, "dateofdata", (void *) buffer);
	buffer[vr_len] = '\0';
	strcat(title, buffer);
	free(buffer);

	G_strip(title);

}

main(argc, argv)
int 	argc;
char	*argv[];
{
	
	int ncdfile;
	int precip_id;
	int akchar;
	nc_type precip_type;
	char filename[512];
	extern void process_short();
	extern void map_initialize();
	extern void process_long();
	extern void process_double();
	
	struct{
		struct Flag *v;
	} flag;

	struct{
		struct Option *input;
		struct Option *output;
	}option;

	option.input = G_define_option();
	option.input->key = "input";
	option.input->type = TYPE_STRING;
	option.input->required = YES;
	option.input->description = "Name of the netcdf file to be converted";

	option.output = G_define_option();
	option.output->key = "output";
	option.output->type = TYPE_STRING;
	option.output->required = NO;
	option.output->gisprompt = "new,cell,raster";
	option.output->description = "Name of the resultant raster map";

	flag.v = G_define_flag();
	flag.v->key = 'v';
	flag.v->description = "Verbose";

	G_gisinit(argv[0]);
	if(G_parser(argc,argv))
		exit(-1);

	if(G_projection() != PROJECTION_LL){
	   fprintf(stderr,"%s projection is not supported for %s.\n",
	   G_database_projection_name(G_projection()), G_program_name());
	   exit(1);
   }

	if(flag.v->answer)
		verbose=VERBOSE;

	strcpy(filename, option.input->answer);

	if(option.output->answer)
		strcpy(mapname, option.output->answer);
	else
		strcpy(mapname, option.input->answer);

	ncdfile	=	ncopen(filename, NC_NOWRITE);
	if(ncdfile == -1){
		fprintf(stderr, "File opening error");
		exit(1);
	}

	get_netcdf_info(ncdfile, title, &precip_id);

	ncvarinq(ncdfile, precip_id, (char *)0, &precip_type, (int *) 0, (int *) 0, (int *) 0);

	/* Now the netcdf file will be read one row at a time in the loop 
	This array of values will be sent to prodce the map */
		
	switch((int) precip_type){

		case NC_SHORT:
			process_short(ncdfile, precip_id, precip_type);
			break;
		case NC_LONG:
			process_long(ncdfile, precip_id, precip_type);
			break;
		case NC_FLOAT:
			process_long(ncdfile, precip_id, precip_type);
			break;
		case NC_DOUBLE:
			process_double(ncdfile, precip_id, precip_type);
			break;
	}
	ncclose(ncdfile);
}
