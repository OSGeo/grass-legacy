/* Pesticide records for source analysis */



typedef struct
{
 int   column_number;
 int   soil_type;
 float bulk_density;
 float porosity;
 float surf_storage;
 float enrich_ratio;
 float tf;
 float sediment;
 float Kd;
 float B_value;
 float effpest;
 float pestplant;
 float initpestplant;
 float pestsoil;
 float initpestsoil;
 float init_pestsoil_ppm;
 float init_pestsoil_after;
 float totpestplant;
 float totpestsoil;
 float rainfall;
 float runoff;
 float washoff;
 float comb_pestsoil;
 float efi;
 float Cav;
 float percolated;
 float percolated_per;
 float soluble_con;
 float soluble_after;
 float soluble_pest;
 float soluble_pest_per;
 float soil_con;
 float sediment_con;
 float sediment_pest_per;
 float solubility;
 float sediment_pest;
 float sediment_lbs;
 float soluble_lbs;
}PEST_REC;

typedef struct
{
  int   column;
 float initial_sediment_portion;
float sed_avail[6];
float avail_sed[6];
float sediment_avail[6];
float percent_flowing_out[6];
float sed_yield[6];
float percent_surface[6];
float pesticide_yield[6];
  float enrich_ratio;
  float sediment_yield;
  int   soil_type;
  float tf;
  float Kd_adjust;
  float Kd;
  float water_lbs;
  float water;
  int   receiving_cell_num;
  int   receiving_cell_div;
  float sediment_portion;
  float water_before;
  float sediment;
  float total_pesticide;
  float water_con_before;
  float solubility;
  float water_con_after;
  float water_portion_after;
}*PEST_ROUTE_RECPTR, PEST_ROUTE_REC;
