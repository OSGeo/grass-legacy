/*======================================================================
   ortho_imagery.h

======================================================================*/

#ifndef _ortho_imagery_h
#define _ortho_imagery_h

#include "gis.h" 


/** TODO move these somewhere else */
/* ortho photo functions */
int mark_control(); 
int anal_control();

int I_get_photo_points_data();
int I_get_photo_auxil_data();
int I_get_photo_coefs_data();
int photo_anal_points();
int photo_trans_calculate();
int photo_trans_forward();
int photo_trans_inverse();

int I_get_ltm_points_data();
int I_get_ltm_auxil_data();
int I_get_ltm_coefs_data();
int ltm_anal_points();
int ltm_trans_calculate();
int ltm_trans_forward();
int ltm_trans_inverse();

int I_get_poly_points_data();
int I_get_poly_coefs_data();
int poly_anal_points();
int poly_trans_calculate();
int poly_trans_forward();
int poly_trans_inverse();
int mark_poly();
int analyze_poly();



/* Transformation Types */
typedef enum {  ZERO_T, POLY1, POLY2, POLY3, 
		  PHOTO, LAND_TM, PROJ, NONE_T } TransType;


typedef struct 
{
    char         name[100];     /* holds group names */
    struct Ref   ref;           /* holds names and mapset of group files */
    TransType    trans_type;    /* current transformation type */
    int          stat;          /* status of transformation */       

    void   *points;             /* control point data pointer */
    void   *auxil;              /* auxilllary data pointer */
    void   *coefs;              /* transformation coefs pointer */

    void  (*init_trans_type) ();  /* routine called to set the following */
                                  /* procedures based on transformation type */

    int   (*get_points) ();       /* routine to get control point  data */
    int   (*get_auxil) ();        /* routine to get auxillary data */
    int   (*get_coefs) ();        /* routine to get trans coeffients */

    int   (*mark_points) ();      /* routine to mark control point data */
    int   (*anal_points) ();      /* routine to analyze control point  data */

    int   (*calculate_trans) ();  /* routine to calculate the transformation */
                                  /* coefs, and fill in the coefs data */
    int   (*forward_trans) ();    /* trans from source --> to target */
    int   (*inverse_trans) ();    /* trans from target --> to source */

} Rectify_Group;


/*-------------------------------------------------------------------------*/
/* structures of Control Points Types                                      */
/*-------------------------------------------------------------------------*/

/* Lat/Lon Control Points */
typedef struct 
{   int    count;            /* number of points             */  
    double *e1;              /* source X, (col or easting)   */
    double *n1;              /* source Y, (row or northing)  */
    double *lon2;            /* target lon, (X or easting)   */
    double *lat2;            /* target lat, (Y or northing)  */
    int	   *status;          /* status: 1=use, 0=ignore      */
} Control_Points_LL;  

/* 2D Control Points */
typedef struct 
{   int    count;            /* number of points */  
    double *e1;              /* source X, (easting)  */
    double *n1;              /* source Y, (northing) */
    double *e2;              /* target X, (easting)  */
    double *n2;              /* target Y, (northing) */
    int	   *status;          /* status: 1=use, 0=ignore */
} Control_Points_2D;  

/* 3D Control Points */
typedef struct 
{   int    count;            /* number of points */  
    double *e1;              /* source X, (easting)  */
    double *n1;              /* source Y, (northing) */
    double *z1;              /* source Z, (elevation)*/
    double *e2;              /* target X, (easting)  */
    double *n2;              /* target Y, (northing) */
    double *z2;              /* target Z, (elevation)*/
    int	   *status;          /* status: 1=use, 0=ignore */
} Control_Points_3D;  


/*-------------------------------------------------------------------------*/
/* Control Points Struct.  Same for all type of transformations.           */
/* group->points will point here                                           */
/*-------------------------------------------------------------------------*/
typedef struct 
{
    Control_Points_LL  points_ll;    /* holds lat/lon control points  */
    Control_Points_2D  points_temp;  /* points converted to target coordinates  */
} Control_Points;



/*-------------------------------------------------------------------------*/
/* structures of Residuals from transformation, Used in analyze routine    */
/*-------------------------------------------------------------------------*/

/* Residuals */
typedef struct 
{   int    count;            /* number of points */  
    double *xres;            /* target X, (easting)  */
    double *yres;            /* target Y, (northing) */
    double *gnd;             /* ground distance */

    double xres_rms;         /* RMS x  */
    double yres_rms;         /* RMS y  */
    double gnd_rms;          /* RMS ground distance  */

    int	   large_x;          /* point with larget X residual */
    int	   large_y;          /* point with larget y residual */
    int	   large_gnd;        /* point with larget ground residual */
} Residuals;  


/*-------------------------------------------------------------------------*/
/* structures of Transformation coeffients                                 */
/*-------------------------------------------------------------------------*/

/* POLYNOMIAL */
typedef struct 
{
    /* polynomial coeffients */
    double E12[10], N12[10];    /* forward: source --> target */
    double E21[10], N21[10];    /* inverse: target --> source */

    /* status */
    int equation_stat;          /* overall status  */

} Coeffs_Poly;


/* ORTHO PHOTO */
typedef struct 
{
    /*--  See "ref_ortho.c" for equations --*/

    /* values for ortho photo */
    double XC, YC, ZC, omega, phi, kappa;

    /* for fiducial coeffients */
    double EF12[3], NF12[3], EF21[3], NF21[3];

    /* status */
    int equation_stat;          /* overall status  */
    int equation_stat_3d;       /* for ortho photo */
    int equation_stat_fid;      /* for fiducails   */

} Coeffs_Photo;


/* LANDSAT TM */
/** TODO **/
typedef struct 
{
    /* values for ortho photo */
    double XC, YC, ZC; 
    double omega, phi, kappa;
    double XC_dot, YC_dot, ZC_dot; 
    double omega_dot, phi_dot, kappa_dot;


/** NOTUSED
/**    /* for fiducial coeffients */
/**    double EF12[3], NF12[3], EF21[3], NF21[3];
**/

    /* status */
    int equation_stat;          /* overall status  */
    int equation_stat_3d;       /* for ortho photo */
    int equation_stat_fid;      /* for fiducails   */

} Coeffs_Ltm;



/*-------------------------------------------------------------------------*/
/* structures used in Ortho Photo Auxillary Data Structure                 */
/*-------------------------------------------------------------------------*/

/* Elevation data structure used to identify an elevation raster file  */
/* in the target location.  ORTHO_PHOTO and LAND_TM only */
/* Note: target location is available through I_get_target, and is not */
/* stored here.  TODO change this situation */

typedef struct {
   char	 elev_map[100];	   /* name of elevation map */
   char	 elev_mapset[100]; /* mapset elev_map is in */

   char  tl[100];          /* TODO: none of the things are used yet  */
   char	 math_exp[100];    /*   and ORTHOPHOTO and LANDTM expect the */
   char  units[100];       /*   elevation in meters.                 */
   char	 nd[100];          /*   no-data value */

   int	 fd;               /* used when opening the target elevation file */
                           /*   to get  a value. see notes in ...  */
} Elevation;


/* Camera data struct used in ORTHO PHOTO transformations */
typedef struct 
{       char cam_name[30];       /* camera name -- not important */
        char cam_id[30];         /* also not important */
        double Xp;               /* Principal point offsets from center */
        double Yp;               /*   of photograph. Generally zero  (mm.) */
        double CFL;              /* Calibrated Focal Length (mm.) */
        int num_fid;             /* number of fiducials entered */
        struct Fiducial          /* TODO - make these dynamically allocated */
        {   char fid_id[10];     /* Fiducial ID -- not important */
            double Xf;           /* Photograph X coordinate (mm.) */
            double Yf;           /* and Y coord of the fiducail */
        } fiducials[12];         
} Camera;


/* This define initial camera expose variables for */
/* ortho photo transformation */
#define INITIAL_X_VAR        500 
#define INITIAL_Y_VAR        500 
#define INITIAL_Z_VAR       1000 
#define INITIAL_OMEGA_VAR   0.01
#define INITIAL_PHI_VAR     0.01
#define INITIAL_KAPPA_VAR   0.1

/* Initial Exposure data for ORTHO PHOTO transformations */
typedef struct
{
    double XC_init;          /* TODO - more comments **/
    double YC_init;
    double ZC_init;
    double omega_init;
    double phi_init;
    double kappa_init;
    double XC_var;
    double YC_var;
    double ZC_var;
    double omega_var;
    double phi_var;
    double kappa_var;
    int status;
} Camera_Expose;


/*--------------------------------------------------------------------------*/
/* ORTHO PHOTO Auxillary Data Type                                          */
/* group->auxil will point to one of these in transformation is ortho_photo */
/*--------------------------------------------------------------------------*/
typedef struct 
{

    /* Fiducial (resaeu) points */
    Control_Points_2D    points_fid;

    /* Source imagery points converted to photo center coordinates */
    /* and Target location coordinates, all 3D */
    Control_Points_3D    points_photo;

    /* Target coordinates with elevation control */
    /* Control_Points_3D    points_target; */

    /* Elevation data for ORTHO */
    Elevation        elev;

    /* Camera for ortho photo */
    Camera           camera;
    Camera_Expose    camera_expose;

} Auxillary_Photo;


/*--------------------------------------------------------------------------*/
/* LANDSAT TM  Auxillary Data Type                                          */
/* group->auxil will point to one of these in transformation is ortho_photo */
/*--------------------------------------------------------------------------*/
/* Satellite data struct used in ORTHO PHOTO transformations */
typedef struct 
{       double Xpix;             /* Principal point offsets from center */
        double Ypix;             /*   of photograph. Generally zero  (mm.) */

        struct Corner            /* TODO - make these dynamically allocated */
        {   char  id[10];        /* Fiducial ID -- not important */
            double Xf;           /* Photograph X coordinate (mm.) */
            double Yf;           /* and Y coord of the fiducail */
            double Ef;           /* Photograph X coordinate (mm.) */
            double Nf;           /* and Y coord of the fiducail */
        } corners[4];         

} Satellite;


/* This define initial camera expose variables for */
/* ortho photo transformation */
/*** #define INITIAL_X_VAR        500 
/*** #define INITIAL_Y_VAR        500 
/*** #define INITIAL_Z_VAR       1000 
/*** #define INITIAL_OMEGA_VAR   0.01
/*** #define INITIAL_PHI_VAR     0.01
/*** #define INITIAL_KAPPA_VAR   0.1

/* Initial Satellite data for LANDSAT TM transformations */
typedef struct
{
    double XC_init;          /* TODO - more comments **/
    double YC_init;
    double ZC_init;
    double omega_init;
    double phi_init;
    double kappa_init;
    double XC_dot;
    double YC_dot;
    double ZC_dot;
    double omega_dot;
    double phi_dot;
    double kappa_dot;
    int status;
} Satellite_Expose;


typedef struct 
{

    /* Fiducial (resaeu) points */
    Control_Points_2D    points_fid;

    /* Source imagery points converted to photo center coordinates */
    /* and Target location coordinates, all 3D */
    Control_Points_3D    points_photo;

    /* Elevation data for ORTHO */
    Elevation        elev;

    /* satellite and orbital info  */
    Satellite           satellite;
    Satellite_Expose    satellite_expose;

} Auxillary_Ltm;



#endif    /* _ortho_imagery_h */








