/* Debugging flags added for AGRUN4.03c verification. */
/* These flags are read from the DEBUG.H file which   */
/* if used, is automatically detected from the execut-*/
/* able directory.  Added 03/30/95 by John Witte...   */


typedef struct
{
 int hydro_table;    /* Hydrologic procs tabular output. [verify1.tab] */
 int sed_table;      /* Sediment process tabular output. [verify2.tab] */
 int chem_table;     /* Chemical process tabular output. [verify3.tab] */
 int fdlt_table;     /* Feedlot process tabular output.  [verify4.tab] */
 int impnd_table;    /* Impoundment tabular output.      [verify5.tab] */
 int sdtrp_table;    /* Sediment trap tabular output.    [verify6.tab] */
}FLAGS_TABLE;


typedef struct
{
 int drain_area;     /* Cell's drainage area calculations.[cellcalc.c] */
 int cell_info;      /* Cell position and characteristics.   [loop1.c] */
 int channel;        /* Channel values.                   [cellcalc.c] */
 int flow;           /* Flow values such as volume and peak.    ["".c] */
 int impound;        /* Impoundment output calculation values.[ters.c] */
 int nutrient;       /* Soil nutrient values in runoff.      [loop1.c] */
 int routing;        /* Cell routing information.         [recroute.c] */
 int sediment;       /* Sediment particle information.       [loop1.c] */
 int tr_55;          /* TR-55 method of ovrlnd flw calcs. [loop1tr5.c] */
}FLAGS_BASIC;


typedef struct
{
 int bulk_dens;      /* Soil bulk density lookup table.      [slnut.c] */
 int calc_chan;      /* Chan params calculated for TR-55. [chan_cal.c] */
 int calc_sed;       /* Sums sedimnt values for the cell. [new_soil.c] */
 int cell_calc;      /* Calculated cell values.           [cellcalc.c] */
 int chan_calc;      /* Chan params calculated for AGNPS.     [sub1.c] */
 int chan_tr_55;     /* Actual TR-55 calculations.        [chantr55.c] */
 int curve_num;      /* Calcs retention value and runoff vol.   [ro.c] */
 int curve_ro;       /* SCS cn method from NEH-4.               [ro.c] */
 int decay_nut;      /* Decay nutrients within the channel.  [decay.c] */
 int feedlot;        /* Feedlot submodel calculations.     [feedlot.c] */
 int hydrology;      /* Calcs pk flow and duratn as CREAMS.  [sub1e.c] */
 int impound;        /* Sets impndmt params (incldng sed). [impound.c] */
 int init_sed;       /* Set up sediment by particle class. [sedinit.c] */
 int length;         /* Channel parameter calculations.     [length.c] */
 int newsoil;        /* Calcs cell sediment deposition.    [newsoil.c] */
 int overland;       /* Calcs time needed for conc flow.     [ovrld.c] */
 int part_pest;      /* Partitions pest on sed & in H2O.  [pesticid.c] */
 int pesticide;      /* Pesticide calculation routine.    [pesticid.c] */
 int pntsrc;         /* Sums up feedlot model output.       [pntsrc.c] */
 int prep_route;     /* Longest flow path to currnt cell. [recroute.c] */
 int rclmap;         /* Receiving cell mapping routine.     [rclmap.c] */
 int recursive;      /* Recursive routing function.       [recroute.c] */
 int routing;        /* Actual routing and accumulation.  [recroute.c] */
 int sed_flow;       /* Sedimnt flo & trnsport cap for cell. [sub1e.c] */
 int sed_nutr;       /* Calculate nutrients in sediment.    [sednut.c] */
 int soil_nutr;      /* Calculate nutrients in soil.         [slnut.c] */
 int src_cells;      /* Finds and marks source(prim) cells.  [sub1e.c] */
 int sum_psrc;       /* Sums point source nutrients.         [sub1e.c] */
 int terh;           /* Calculate peak flow from impoundment. [terh.c] */
 int ters;           /* Calcs sediment yield from an impound. [ters.c] */
 int tr_55;          /* Calcs overland flow TR-55 method. [loop1tr5.c] */
 int xeros;          /* Soil erosion based on USLE.          [xeros.c] */

}FLAGS_ROUTINE;





typedef struct        /* DATA STRUCTURE FOR VERIFICATION TABLE 1 (HYDRO.) */
{                     /* **NOTE** For units docs see c:\wp51\verify_1.doc */
 int    column_id;         /* Total drainage area at the outlet.          */
 int    page_num;          /* Area of basic cell.                         */
 char   name[15];          /* Name of watershed being run.                */

 float  drainage_area;     /* Total drainage area at the outlet.          */
 float  total_cell_area;   /* Area of basic cell.                         */
 float  feedlot_area;      /* Sum of all feedlot subareas within cell.    */
 float  impoundment_area;  /* Tot drnage area to outlet of the impndmnt.  */
 int    receiving_cell;    /* Cell which current cell drains into.        */
 int    next_routing_cell; /* Cell which routes into current cell.        */

 float  ol_length;         /* Overland flow length.                       */
 float  ol_travel_time;    /* Overland flow travel time.                  */

 float  sc_length;         /* Shallow concentrated flow length.           */
 float  sc_velocity;       /* Shallow concentrated flow velocity.         */
 float  sc_travel_time;    /* Shallow concentrated travel time.           */

 float  ch_top_width;      /* Channel flow top width.                     */
 float  ch_depth_bankfull; /* Channel flow depth at bankfull.             */
 float  ch_seg_length;     /* Channel flow segment length.                */
 float  ch_velocity;       /* Channel flow velocity.                      */
 float  ch_travel_time;    /* Channel flow travel time.                   */
 float  ch_time_of_conc;   /* Channel flow time of concentration.         */

 float  pk_dis_outlet_cfs; /* Ave peak disharge of in and out hydrographs.*/
 float  pk_dis_outlet_iph; /*   " "  with inches/hour units.              */
 float  tot_ro_outlet_af;  /* Ave tot ro volume of in and out hydrographs.*/
 float  tot_ro_outlet_in;  /*   " "  with inches units.                   */
 float  avg_pk_discharge;  /* Ave pk discharge of in and out hydrographs. */
 float  avg_ro_volume;     /* Ave ro volume of in and out hydrographs.    */
 float  ro_pk_volume;      /* Avg ro pk volume of in and out hydrographs. */
 float  time_to_pk;        /* Avg time to peak of in and out hydrographs. */
 float  time_to_base;      /* avg time to base of in and out hydrographs. */

}HYDRO_TABLE_ENTRY;


typedef struct
{
 HYDRO_TABLE_ENTRY ht[10];
}HYDRO_TABLE, *HYDRO_TABLE_PTR;



typedef struct        /* DATA STRUCTURE FOR VERIFICATION TABLE 1 (SED.)   */
{                     /* **NOTE** For units docs see c:\wp51\verify_2.doc */
 int    column_id;         /* Column (cell) number identification.        */
 int    page_num;          /* Page number base value (used for output).   */
 int    num_partitions;    /* Number of partitions present in hydrograph. */
 char   name[15];          /* Name of watershed being run.                */

				     /* NOTE:  index: [class]             */
 float  up_amount[7];             /* Upstream particle amount.         */
 float  up_conc[7];               /* Upstream particle concentration.  */
 float  tot_in_cell_src[7];       /* Total in-cell particle sources.   */
 float  down_amount[7];           /* Downstream particle amount.       */
 float  down_conc[7];             /* Downstream particle concentration.*/

 float  constant_prop[7];         /*  Constant of proportionality.     */
 float  part_fall_vel[7];         /*  Particle fall velocity.          */
 float  str_seg_length[7];        /*  Stream segment length.           */
 float  trans_cap_factor[7];      /*  Transport capacity factor.       */
				     /* NOTE:  index: [part][class]    */
 float  water_discharge[9][7];    /*  Unit-width water discharge.      */
 float  sed_trans_cap[9][7];      /*  Unit-width sed transprt capacty. */
 float  up_sed_discharge[9][7];   /* Upstream u-width sediment dischrg.*/
 float  down_sed_discharge[9][7]; /* Downstram u-wdth sediment dischrg.*/
 float  deposition_number[9][7];  /* Deposition number.                */

}SED_TABLE_ENTRY;


typedef struct
{
 SED_TABLE_ENTRY st[10];
}SED_TABLE, *SED_TABLE_PTR;






typedef struct        /* DATA STRUCTURE FOR VERIFICATION TABLE 3 (CHEM.)  */
{                     /* **NOTE** For units docs see c:\wp51\verify_3.doc */
 int    column_id;         /* Column (cell) number identification.        */
 int    page_num;          /* Page number base value (used for output).   */
 char   name[15];          /* Name of watershed being run.                */

 float  N_dis_decay_factor;/* Nitrogen dissolved decay factor.            */
 float  N_dis_into;        /* Nitrogen dissolved in from draining cells.  */
 float  N_dis_local;       /* N dissolved generated within local cell.    */
 float  N_dis_out;         /* Nitrogen dissolved out of cell.             */

 float  N_att_into;        /* Nitrogen attached in from draining cells.   */
 float  N_att_local;       /* Nitrogen attached generated within local.   */
 float  N_att_out;         /* Nitrogen attached out of cell.              */

 float  N_tot_into;        /* Nitrogen total in from draining cells.      */
 float  N_tot_local;       /* Nitrogen total generated within local cell. */
 float  N_tot_out;         /* Nitrogen total out of cell.                 */
 float  N_tot_enrichment;  /* Nitrogen total enrichment ratio.            */

/**************************************************************************/

 float  P_dis_decay_factor;/* Phosphorus dissolved decay factor.          */
 float  P_dis_into;        /* Phosphorus dissolved in from draining cells.*/
 float  P_dis_local;       /* P dissolved generated within local cell.    */
 float  P_dis_out;         /* Phosphorus dissolved out of cell.           */

 float  P_att_into;        /* Phosphorus attached in from draining cells. */
 float  P_att_local;       /* Phosphorus attached generated within local. */
 float  P_att_out;         /* Phosphorus attached out of cell.            */

 float  P_tot_into;        /* Phosphorus total in from draining cells.    */
 float  P_tot_local;       /* Phosphorus total generated in local cell.   */
 float  P_tot_out;         /* Phosphorus total out of cell.               */
 float  P_tot_enrichment;  /* Phosphorus total enrichment ratio.          */

/**************************************************************************/

 float  Pest_dis_decay_fct;/* Pesticide dissolved decay factor.           */
 float  Pest_dis_into;     /* Pesticide dissolved in from draining cells. */
 float  Pest_dis_local;    /* Pesticide dissolved generated in local cell.*/
 float  Pest_dis_out;      /* Pesticide dissolved out of cell.            */

 float  Pest_att_into;     /* Pesticide attached in from draining cells.  */
 float  Pest_att_local;    /* Pesticide attached generated within local.  */
 float  Pest_att_out;      /* Pesticide attached out of cell.             */

 float  Pest_tot_into;     /* Pesticide total in from draining cells.     */
 float  Pest_tot_local;    /* Pesticide total generated within local cell.*/
 float  Pest_tot_out;      /* Pesticide total out of cell.                */
 float  Pest_tot_enrich;   /* Pesticide total enrichment ratio.           */

/**************************************************************************/

 float  COD_decay_fct;     /* Chemical Oxygen Demand decay factor.        */
 float  COD_into_ppm;      /* Chemical Oxygen Demand in from draing cells.*/
 float  COD_into_lbs;      /* Chemical Oxygen Demand in from draing cells.*/
 float  COD_local_ppm;     /* Chemical Oxygen Demand in local cell.       */
 float  COD_local_lbs;     /* Chemical Oxygen Demand in local cell.       */
 float  COD_out_ppm;       /* Chemical Oxygen Demand out of cell.         */
 float  COD_out_lbs;       /* Chemical Oxygen Demand out of cell.         */

}CHEM_TABLE_ENTRY;


typedef struct
{
 CHEM_TABLE_ENTRY ct[10];
}CHEM_TABLE, *CHEM_TABLE_PTR;
