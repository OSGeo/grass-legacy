#include "header.h"
digit_header()
{
	int zone ;
	int anint ;
	int degrees, minutes ;
	double ll_e, ll_w, ll_n, ll_s ;
	double u_e, u_w, u_n, u_s ;

	printf("ORGANIZATION: DMA\n") ;
	printf("DIGIT DATE:   %s\n", DSI.compilation_date) ;
	printf("DIGIT NAME:   none\n") ;
	printf("MAP NAME:     %s\n", DSI.data_set_id) ;
	printf("MAP DATE:     unknown\n") ;
	printf("MAP SCALE:    unknown\n") ;
	printf("OTHER INFO:                              \n") ;
	sscanf(DSP.longitude_of_SW_corner,"%d", &anint) ;
		degrees = anint / 1000000 ; minutes = (anint - 1000000 * degrees) / 10000;
		ll_w = 3600. * degrees + 60. * minutes ;
	sscanf(DSP.longitude_of_NE_corner,"%d", &anint) ;
		degrees = anint / 1000000 ; minutes = (anint - 1000000 * degrees) / 10000;
		ll_e = 3600. * degrees + 60. * minutes ;
	sscanf(DSP.latitude_of_SW_corner, "%d", &anint) ;
		degrees = anint / 1000000 ; minutes = (anint - 1000000 * degrees) / 10000;
		ll_s = 3600. * degrees + 60. * minutes ;
	sscanf(DSP.latitude_of_NE_corner, "%d", &anint) ;
		degrees = anint / 1000000 ; minutes = (anint - 1000000 * degrees) / 10000;
		ll_n = 3600. * degrees + 60. * minutes ;
	zone = 0 ;
	CC_ll2u (ll_s, ll_w, &u_w, &u_s, &zone) ;
	CC_ll2u (ll_n, ll_e, &u_e, &u_n, &zone) ;
	printf("ZONE:         %d\n", zone) ;
	printf("WEST EDGE:    %.2lf\n", u_w) ;
	printf("EAST EDGE:    %.2lf\n", u_e) ;
	printf("SOUTH EDGE:   %.2lf\n", u_s) ;
	printf("NORTH EDGE:   %.2lf\n", u_n) ;
	sscanf(DSP.horizontal_resolution_units, "%d", &anint) ;
	printf("MAP THERSH:   %d\n", anint) ;
	printf("VERTI:             \n", anint) ;
}
