struct dsi 
{
	char product_type[6] ;
	char data_set_id[21] ;
	char edition[4] ;
	char compilation_date[5] ;
	char maintenance_date[5] ;
	char FACS_version_date[7] ;
	char ISO_version_date[7] ;
} DSI ;

struct dss
{
	char security_code[2] ;
	char security_release[3] ;
	char downgrade_date[7] ;
	char security_handling[22] ;
} DSS ;

struct dsp
{
	char data_type[4] ;
	char horizontal_units_of_measure[4] ;
	char horizontal_resolution_units[6] ;
	char geodetic_datum[4] ;
	char vertical_units_of_measure[4] ;
	char vertical_resolution_units[6] ;
	char vertical_reference_system[5] ;
	char sounding_datum[5] ;
	int latitude_of_origin ;
	int longitude_of_origin ;
	char latitude_of_SW_corner[10] ;
	char longitude_of_SW_corner[11] ;
	char latitude_of_NE_corner[10] ;
	char longitude_of_NE_corner[11] ;
	double latitude_of_origin_in_sec ;
	double longitude_of_origin_in_sec ;
} DSP ;

struct topo
{
	int num_edge_records ;
	int num_node_records ;
	int num_face_records ;
} TOPO ;

struct cart
{
	int num_feature_component_records ;
	int num_feature_records ;
} CART ;
