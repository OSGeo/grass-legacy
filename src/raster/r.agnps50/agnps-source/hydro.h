/* Hydrology records for source analysis */



typedef struct
{
float curve_number;
float storm_rainfall;
float retention_factor;
float equation_top;
float equation_bottom;
float runoff;
int   calc_method;
float slope;
float surface_condition;
float power;
float flow_velocity;
int   slope_length;
float overland_flow_duration;
float overland_mannings;
float time_overland;
float area;
float cell_width;
int   primary_cell;
float shallow_length;
float velocity_shallow;
float velocity_shallow_after;
float time_shallow;
int   temp_receiving_cell;
float drainage_area;
float pipe_diameter;
float cell_runoff;
float depth;
int   pond_area_coeff;
float b;
float cor;
float peak_flow;
}HYDRO_REC;

typedef struct
{
int column;
}*PEST_ROUTE_RECPTR, PEST_ROUTE_REC;
