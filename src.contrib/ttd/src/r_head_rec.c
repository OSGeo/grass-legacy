#include <stdio.h>
#include "header.h"

read_header_record(rec)
	char *rec ;
{
	char *record ;
	int length ;

/************************************************************************/
	if (strncmp(rec, "DSIG", 4) == 0)
	{
		read_DSIG(rec) ;
#ifdef DEBUG
		write_DSIG() ;
#endif DEBUG
	}
	else
	{
		printf("ERROR: 2 out of sync at record:\n%s\n", rec) ;
		exit(-1) ;
	}
	length = getrecord(&record) ;
/************************************************************************/
	length = getrecord(&record) ;
	length = getrecord(&record) ;
	if (strncmp(record, "DSSG", 4) == 0)
	{
		read_DSSG(record) ;
#ifdef DEBUG
		write_DSSG() ;
#endif DEBUG
	}
	else
	{
		printf("ERROR: 3 out of sync at record:\n%s\n", record) ;
		exit(-1) ;
	}
	length = getrecord(&record) ;
/************************************************************************/
	length = getrecord(&record) ;
	length = getrecord(&record) ;
	if (strncmp(record, "DSPG", 4) == 0)
	{
		read_DSPG(record) ;
#ifdef DEBUG
		write_DSPG() ;
#endif DEBUG
	}
	else
	{
		printf("ERROR: 4 out of sync at record:\n%s\n", record) ;
		exit(-1) ;
	}
	length = getrecord(&record) ;
/************************************************************************/
	length = getrecord(&record) ;
	length = getrecord(&record) ;
	if (strncmp(record, "DSTG", 4) == 0)
	{
		read_DSTG(record) ;
#ifdef DEBUG
		write_DSTG() ;
#endif DEBUG
	}
	else
	{
		printf("ERROR: 5 out of sync at record:\n%s\n", record) ;
		exit(-1) ;
	}
	length = getrecord(&record) ;
/************************************************************************/
	length = getrecord(&record) ;
	length = getrecord(&record) ;
	if (strncmp(record, "DSCG", 4) == 0)
	{
		read_DSCG(record) ;
#ifdef DEBUG
		write_DSCG() ;
#endif DEBUG
	}
	else
	{
		printf("ERROR: 6 out of sync at record:\n%s\n", record) ;
		exit(-1) ;
	}
	length = getrecord(&record) ;
/************************************************************************/
}

read_DSIG(record)
	char *record ;
{
	sscanf(record,"%*4c%5c%20c%*5c%3c%4c%4c%6c%6c",
		DSI.product_type,
		DSI.data_set_id,
		DSI.edition,
		DSI.compilation_date,
		DSI.maintenance_date,
		DSI.FACS_version_date,
		DSI.ISO_version_date) ;
}

write_DSIG()
{
	printf("Product type           : %s\n", DSI.product_type) ;
	printf("Data set id            : %s\n", DSI.data_set_id) ;
	printf("Edition                : %s\n", DSI.edition) ;
	printf("Compilation date       : %s\n", DSI.compilation_date) ;
	printf("Maintenance date       : %s\n", DSI.maintenance_date) ;
	printf("FACS version date      : %s\n", DSI.FACS_version_date) ;
	printf("ISO version date       : %s\n", DSI.ISO_version_date) ;
	printf("\n") ;
}

read_DSSG(record)
	char *record ;
{
	sscanf(record,"%*4c%*5c%1c%2c%6c%21c",
		DSS.security_code,
		DSS.security_release,
		DSS.downgrade_date,
		DSS.security_handling) ;
}

write_DSSG(record)
	char *record ;
{
	printf("Security code          : %s\n", DSS.security_code) ;
	printf("Security release       : %s\n", DSS.security_release) ;
	printf("Downgrade date         : %s\n", DSS.downgrade_date) ;
	printf("Security handling      : %s\n", DSS.security_handling) ;
	printf("\n") ;
}

read_DSPG(record)
	char *record ;
{
	int i ;
	int j ;

	sscanf(record, "%*4c%*5c%3c%3c%5c%3c%3c%5c%4c%4c%10d%10d%9c%10c%9c%10c",
		DSP.data_type,
		DSP.horizontal_units_of_measure,
		DSP.horizontal_resolution_units,
		DSP.geodetic_datum,
		DSP.vertical_units_of_measure,
		DSP.vertical_resolution_units,
		DSP.vertical_reference_system,
		DSP.sounding_datum,
		&DSP.latitude_of_origin,
		&DSP.longitude_of_origin,
		DSP.latitude_of_SW_corner,
		DSP.longitude_of_SW_corner,
		DSP.latitude_of_NE_corner,
		DSP.longitude_of_NE_corner) ;

		j = DSP.latitude_of_origin ;
		i = j / 10000000. ;
		DSP.latitude_of_origin_in_sec = 3600. * i + 
			60 * (j - 10000000. * i)/100000. ;

		j = - DSP.longitude_of_origin ;
		i = j / 10000000. ;
		DSP.longitude_of_origin_in_sec = 3600. * i + 
			60 * (j - 10000000. * i)/100000. ;
}

write_DSPG()
{
	printf("Data type              : %s\n", DSP.data_type) ;
	printf("Horizontal units       : %s\n", DSP.horizontal_units_of_measure) ;
	printf("Horizontal resolution  : %s\n", DSP.horizontal_resolution_units) ;
	printf("Geodetic datum         : %s\n", DSP.geodetic_datum) ;
	printf("Vertical units         : %s\n", DSP.vertical_units_of_measure) ;
	printf("Vertical resolution    : %s\n", DSP.vertical_resolution_units) ;
	printf("Vertical ref system    : %s\n", DSP.vertical_reference_system) ;
	printf("Sounding datum         : %s\n", DSP.sounding_datum) ;
	printf("Latitude of origin     : %d\n", DSP.latitude_of_origin) ;
	printf("Longitude of origin    : %d\n", DSP.longitude_of_origin) ;
	printf("Latitude of SW corner  : %s\n", DSP.latitude_of_SW_corner) ;
	printf("Longitude of SW corner : %s\n", DSP.longitude_of_SW_corner) ;
	printf("Latitude of NE corner  : %s\n", DSP.latitude_of_NE_corner) ;
	printf("Longitude of NE corner : %s\n", DSP.longitude_of_NE_corner) ;
	printf("\n") ;
}

read_DSTG(record)
	char *record ;
{
	sscanf(record, "%*4c%*5c%10d%10d%10d",
		&TOPO.num_edge_records,
		&TOPO.num_node_records,
		&TOPO.num_face_records ) ;
}

write_DSTG()
{
	printf("Num edge records       : %d\n", &TOPO.num_edge_records) ;
	printf("Num node records       : %d\n", &TOPO.num_node_records) ;
	printf("Num face records       : %d\n", &TOPO.num_face_records ) ;
	printf("\n") ;
}

read_DSCG(record)
	char *record ;
{
	sscanf(record, "%*4c%*5c%10d%10d%10d",
		&CART.num_feature_component_records,
		&CART.num_feature_records) ;
}

write_DSCG()
{
	printf("Num feature comp. recs.: %d\n", &CART.num_feature_component_records) ;
	printf("Num feature records    : %d\n", &CART.num_feature_records) ;
	printf("\n") ;
}
