/* input.h */
/*  This file has the structures for the data that is input */
 
/* #include <stdio.h>  put this elsewhere!! */

/* CHANGES

10/05/93	MAK	C0008	Changed buffer slope length in feedlot to int
*/
 
 
/*#define NULL          0L */
#define TR55          0
#define CREAMS        1
#define GEOMORPHIC    1
#define NONGEOMORPHIC 0
#define TRUE          1
#define FALSE         0
 
 
#define LBSpACRE_TO_KGpHA  1.1206
#define ROCK_DENSITY       2.65
#define HA_TO_ACRES        2.471
#define METERS_TO_FEET     3.281
#define METERS_TO_INCHES   39.37
#define CM_TO_INCHES       0.39
#define KG_TO_LBS          2.2046
#define LBS_TO_KG          0.4536
#define ACRES_TO_SQ_METERS 4048.6
#define ACRES_TO_SQ_FEET   43560.0
#define CMS_TO_CFS         35.320
#define TONNES_TO_TONS     1.1
#define INCHES_TO_METERS   0.0254
#define TONS_TO_LBS        2000.0
#define LBS_TO_TONS        0.0005
#define IN_ACRE_TO_CU_FT   3630.0
#define CU_FT_H2O_IN_LBS   62.4
 
 
typedef struct
{
 float channel_side_slope;     /* percent   */
 float channel_slope;          /* percent   */
 float channel_length;         /* feet      */
 float length_coef;
 float length_exp;
 float depth;                  /* feet      */
 float depth_coef;
 float depth_exp;
 float width;                  /* feet      */
 float width_coef;
 float width_exp;
 float channel_mannings;
 int   agnps_decay;
 float per_n_decay;
 float per_p_decay;
 float per_cod_decay;
 int   channel_indicator;
 int   intrusion_indicator;
 int   clay_indicator;
 int   silt_indicator;
 int   small_agg_indicator;
 int   large_agg_indicator;
 int   sand_indicator;
 float tr55_length;
 float total_length;
 float duration;
 float long_dur;
 float bank_depth;
 float max_depth;
 
 } CHANNEL_INFO;
 
 
 
typedef struct
{
  float weight;                /* particle type weight                 */
  float diameter;              /* particle type diameter               */
  float set_velocity;          /* particle type settling velocity      */
  float trans_cap;             /* particle type transport capacity     */
 
} PARTICLE_DATA;               /* arrary index 1-5 = particle type     */
 
 
 
typedef struct
{
 float base_cell_area;              /* acres       */
 int   number_of_base_cells;        /* nod         */
 float storm_rainfall;              /* inches      */
 float storm_energy_intensity;
 char   storm_type[4];
 float storm_duration;
 char  description[100];
 int   calc_method;
 int   geomorphic_calc;
 float K_triangle;
 float prepeak_RO_fraction;
 float rainfall_nitrogen;            /* ppm = mg/L */
 char  watershed[100];
 
 } *INITIAL_INFOPTR, INITIAL_INFO;
 
 
 
typedef struct
{
 float curve_number;                   /* English  */
 float cropping_factor;
 float practice_factor;
 float surface_condition_constant;     /* English  */
 int   fertilizer_level;               /* 0 - 4    */
 float nitrogen_application_rate;      /* lbs/acre */
 float phosphorus_application_rate;    /* lbs/acre */
 int   nitrogen_availability;          /* percent  */
 int   phosphorus_availability;        /* percent  */
 int   cod_factor;
 
} MANAGEMENT_INFO;
 
 
 
typedef struct
{
 float average_land_slope;     /* percent  */
 int   slope_shape_code;       /* 1,2,or 3 */
 int   slope_length;           /* feet     */
 
} SLOPE_INFO;
 
 
 
typedef struct
{
 float soil_erodibility_factor;
 int   soil_type;
 float base_soil_nitrogen;
 float base_soil_phosphorus;
 float soil_pore_nitrogen;
 float soil_pore_phosphorus;
 float nitrogen_runoff_extraction;
 float phosphorus_runoff_extraction;
 float nitrogen_leaching_extraction;
 float phosphorus_leaching_extraction;
 int soil_organic_matter;
 
} SOIL_INFO;
 
 
typedef struct GULLYDATA
{
 int   gully_source;
 int   gully_soil;
 float gully_nitrogen;
 float gully_phos;

 struct GULLYDATA *next;
 
} GULLY_INFO;
 
 
 
/* ********************************************************************* */
/*  This structure holds pesticide variables that are specific to a cell */
 
typedef struct
{
  float time_of_application;        /* days     */
  float time_since_application;     /* days     */
  float application_rate;           /* lbs/acre */
  float application_efficiency;     /* fraction */
  float incorporation_depth;        /* inches   */
  float incorporation_efficiency;   /* fraction */
  float canopy_cover;               /* percent  */
  float initial_soil_residue;       /* lbs/acre */
  float foliar_residue;             /* lbs/acre */
 
  float soluble_con;         /* pesticide conc. in runoff       ppm water */
  float soluble_pest;        /* pesticide mass in runoff           lbs    */
  float soluble_pest_per;    /* pest. in runoff / amt applied       %     */
  float percolated;          /* pesticide percolated into soil  lbs/acre  */
  float percolation_per;     /* % pest. percolated into soil        %     */
  float soil_con;            /* pest. conc. remaining in soil   ppm soil  */
/*  float soil_pest;      */ /* pest. mass in soil              lbs/acre  */
/*  float soil_pest_per;  */ /* pest. in soil / amt applied         %     */
  float sediment_con;        /* pest. conc. in eroded sediment  ppm soil  */
  float sediment_pest;       /* pest. mass in eroded sediment      lbs    */
  float sediment_pest_per;   /* pest. in sediment / amt applied     %     */
 
  float soluble_pest_exit;   /* pest. mass in runoff at exit       lbs    */
  float sediment_pest_exit;  /* pest. mass in sediment at exit     lbs    */
 
} PESTICIDE_INFO;
 
 
 
/* ********************************************************************* */
/*  This structure holds pesticide variables that                        */
/*     apply to the whole watershed                                      */
 
typedef struct
{
  char  pesticide_name[60];
  float soil_residue_halflife;       /* days    */
  float solubility;                  /* ppm     */
  float organic_carbon_sorption;     /* Koc     */
  float foliar_washoff_threshold;    /* inches  */
  float foliar_washoff_fraction;     /* 0.0-1.0 */
  float foliar_residue_halflife;     /* days    */
 
} GENERAL_PESTICIDE_DATA;
 
 
 
typedef struct IMPOUNDDATA
{
 float drainage_area;          /* acres       */
 float pipe_diameter;          /* inches      */
 float infiltration;           /* inches / hr */
 float peak_flow;              /* ft^3 / sec  */
 float volume_runoff;          /* ft^3        */
 float sediment_out[6];
 float duration;
 
 struct IMPOUNDDATA *next;
 
} IMPOUND_INFO;
 
 
 
typedef struct NONFEEDLOT_PTSRC
{
 float nitrogen_concentration;        /*   ppm                   */
 float phosphorus_concentration;      /*   ppm                   */
 float cod_concentration;             /*   ppm                   */
 float water_discharge;               /*   ft^3 / sec            */
 int   enter_at_top;                  /* top or bottom of cell ? */
 
 struct NONFEEDLOT_PTSRC *next;
 
} NONFEEDLOT_INFO;
 
 
 
typedef struct FEEDLOT
{
 float area;                           /* acres      */
 float curve_number;                   /* English    */
 float roofed_area;                    /* acres      */
 float area2_area[7];                  /* acres      */
 float area2_curve_number[7];          /* English    */
 float area3_area[7];                  /* acres      */
 float area3_curve_number[7];          /* English    */
 float buffer_slope[4];
 float buffer_surface_constant[4];
 int   buffer_flow_length[4];
 float number_of_animals[4];
 float cod_factor[4];
 float animal_phosphorus[4];
 float animal_nitrogen[4];
 float feedlot_cod;
 float feedlot_nitrogen;
 float feedlot_phosphorus;
 int   use_buffer_inputs;
 int   feedlot_number;
 float n_conc_discharge;               /* lbs/acre-in??? */
 float p_conc_discharge;               /* lbs/acre-in??? */
 float cod_conc_discharge;             /* lbs/acre-in??? */
 float n_discharge_lbs;                /* lbs of N   */
 float p_discharge_lbs;                /* lbs of P   */
 float cod_discharge_lbs;              /* lbs of COD */
 int   feedlot_rating_number;
 float decrease_n_grass;               /* percent    */
 float decrease_p_grass;               /* percent    */
 float decrease_cod_grass;             /* percent    */
 float decrease_n_overland;            /* percent    */
 float decrease_p_overland;            /* percent    */
 float decrease_cod_overland;          /* percent    */
 
 struct FEEDLOT *next;
 
} FEEDLOT_INFO;
 
 
 
typedef struct
{
 float tot_feedlot_nit;
 float tot_feedlot_phos;
 float tot_feedlot_cod;
 
} FEEDLOT_TOTALS;
 
 
 
typedef struct
{
  float area_weighted_erosion;
  float gully_erosion;
 
} SEDIMENT_DATA;
 
 
typedef struct
{
  float sed_available;
  float sed_flow_rate_within;
  float sed_flow_rate_into;
} SEDIMENT_INFO;
 
 
 
typedef struct
{
  float cell_run_off;                      /* inches  */
  float overland_flow_duration;            /* secs    */
  float available_sediment[6];
  float sediment_yield[6];
  float impound_yield[6];
  float soluble_nitrogen_runoff;            /* lbs/acre */
  float soluble_phosphorus_runoff;          /* lbs/acre */
  float cod_runoff;                         /* lbs/acre */
  float soluble_nitrogen_yield;             /* lbs      */
  float soluble_phosphorus_yield;           /* lbs      */
  float soluble_cod_yield;                  /* lbs      */
  float total_eroded_sediment;              /* lbs/acre */
  float time_overland;           /* in hours for primary at top of      */
  float time_shallow;            /*          longest flow path          */
  float time_previous;
  float time_concentrated;       /* in hours to bottom of current cell  */
  float total_time;              /* in hours to bottom of current cell  */
  float total_n_within_cell;
  float total_p_within_cell;
  float total_n_cell_outlet;
  float total_p_cell_outlet;
  float impound_nit;
  float impound_phos;
  float sol_impound_nit;
  float sol_impound_phos;
  float sol_impound_cod;
 
} RUNOFF_INFO;
 
 
 
typedef struct SOURCEA
{
 int   source_column;
 struct SOURCEA *next;
} SOURCE, *SOURCEPTR;
 
 
typedef struct
{
 float length_to_bottom;  /* channel length to bottom of current cell    */

 double sum_of_lengths;    /* channel length to bottom of current cell,
			      calculated by adding the length of channel in
			      each cell.                                  */
 float length_slope;      /* length-weighted channel slope to bottom     */
 double length_side_slope; /* length-weighted channel side slope to bottom*/
 double length_mannings;   /* length-weighted channel mannings            */

 float drainage_area;      /* drainage area to bottom of cell             */
			   /*     includes area drained by impoundments   */
 double cn_area;           /* cn times area for area draining out of cell */
 
 double runoff_vol_above;  /* sum of the volumes entering the cell   ft^3 */
 double runoff_vol_below;  /* add to above the volume gen. within    ft^3 */

 float runoff_flow_above;  /* sum of the flows entering the cell      cfs */
 float runoff_flow_below;  /* downstream flow rate                    cfs */
 
 float sum_psource_flows;   /* see note below */                 /*  cfs */
 float sum_psource_volumes; /* see note below */                 /* ft^3 */
 
} ACCUM_VALUES;
 
 
/* NOTE:   sum_psource_flows and sum_psource_volumes change their meaning
     slightly in the course of computation.  At the end of prepare_to_route,
     they include only flows coming into the cell).  During route, the
     flow and volume from nonfeedlot point sources within this cell
     are added.
*/
 
 
 
 
typedef struct
{
 int   cell_number;
 int   cell_division;
 int   receiving_cell_number;
 int   receiving_cell_division;
 int   flow_direction;
 int   num_point_sources;
 int   num_impoundments;
 int   pesticide_type;
 int   receiving_cell_position;
 int   primary_cell;
 float area;
 float area_not_terraced;           /* within cell                       */
 float overland_mannings;
 float num_nonfeedlots;
 
 ACCUM_VALUES     *accumulated;
 FEEDLOT_INFO     *feedlot;
 NONFEEDLOT_INFO  *nonfeedlot;
 IMPOUND_INFO     *impound;
 FEEDLOT_TOTALS   *feed_totals;
 CHANNEL_INFO     *channel;
 MANAGEMENT_INFO  *management;
 SLOPE_INFO       *slope;
 SOIL_INFO        *soil;
 RUNOFF_INFO      *runoff;
 PESTICIDE_INFO   *pesticide;
 GULLY_INFO       *gully;
 
 SOURCEPTR	  sourcelist;
 
} *COLUMN_INFOPTR, COLUMN_INFO;
 


typedef struct SINKHOLEA
{
 int col_number;
 struct SINKHOLEA *next;

} SINKHOLE, *SINKHOLEPTR;

typedef struct
{
 int cell_number;
 int cell_division;
 int receiving_number;
 int receiving_div;
 int flow_direction;
 float clay_sheet;
 float clay_gully;
 float silt_sheet;
 float silt_gully;
 float sagg_sheet;
 float sagg_gully;
 float lagg_sheet;
 float lagg_gully;
 float sand_sheet;
 float sand_gully;
 float sed_n_overland;
 float sed_n_gully;
 float sed_n_impoundments;
 float sed_p_overland;
 float sed_p_gully;
 float sed_p_impoundments;
 float sol_n_overland;
 float sol_n_fertilizer;
 float sol_n_feedlots;
 float sol_p_overland;
 float sol_p_fertilizer;
 float sol_p_feedlots;
 float sol_cod_overland;
 float sol_cod_feedlots;
 float runoff_volume;
} *SOURCEACCTPTR, SOURCEACCT;

typedef struct
{
int   cell_number;
int   cell_division;
float clay_bed;
float clay_deposition;
float silt_bed;
float silt_deposition;
float sagg_bed;
float sagg_deposition;
float lagg_bed;
float lagg_deposition;
float sand_bed;
float sand_deposition;
float sed_n_deposition;
float sed_p_deposition;
float sol_n_decay;
float sol_p_decay;
float sol_cod_decay;
float sol_n_nonfeedlots;
float sol_p_nonfeedlots;
float sol_cod_nonfeedlots;
float sol_n_impoundments;
float sol_p_impoundments;
/*float sed_pest_deposition;*/
} *SOURCEACCT2PTR, SOURCEACCT2;
