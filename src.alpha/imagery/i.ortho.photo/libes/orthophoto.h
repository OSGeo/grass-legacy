#include "gis.h"
#include "orthodefs.h"

/* #define DEBUG  1 */

#define INITIAL_X_VAR   500 
#define INITIAL_Y_VAR   500 
#define INITIAL_Z_VAR  1000 
#define INITIAL_OMEGA_VAR   0.01
#define INITIAL_PHI_VAR     0.01
#define INITIAL_KAPPA_VAR   0.1

struct Ortho_Image_Group_Ref
{
    int  nfiles;
    struct Ortho_Image_Group_Ref_Files
    {
	char name[30];
	char mapset[30];
    } *file;
    struct Ref_Color
    {
        unsigned char *table      ;  /* color table for min-max values */
        unsigned char *index      ;  /* data translation index */
        unsigned char *buf        ;  /* data buffer for reading color file */
        int fd                    ;  /* for image i/o */
        CELL min, max             ;  /* min,max CELL values */
        int n                     ;  /* index into Ref_Files */
    } red, grn, blu;
} ;

struct Ortho_Camera_File_Ref
    {   char cam_name[30];
        char cam_id[30];
        double Xp;
        double Yp;
        double CFL;
        int num_fid;
        struct Fiducial
        {   char fid_id[30];
            double Xf;
            double Yf;
        } fiducials[20];
 } ;

struct Ortho_Photo_Points
{   int count;
    double *e1;
    double *n1;    
    double *e2;    
    double *n2;
    double *z1;
    double *z2;
    int  *status;
};

struct Ortho_Control_Points
{
    int  count;
    double *e1;
    double *n1;
    double *z1;
    double *e2;
    double *n2;
    double *z2;
    int  *status;
} ;

struct Ortho_Camera_Exp_Init
{
    double XC_init;
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
} ;


struct Ortho_Image_Group
{
    char  name[100];
    struct Ortho_Image_Group_Ref    group_ref;
    struct Ortho_Camera_File_Ref    camera_ref;
    struct Ortho_Photo_Points       photo_points;
    struct Ortho_Control_Points     control_points;
    struct Ortho_Camera_Exp_Init    camera_exp;
    int ref_equation_stat;
    int con_equation_stat;
    double E12[3], N12[3], E21[3], N21[3], Z12[3], Z21[3];
    double XC, YC, ZC, omega, phi, kappa;
} ;

