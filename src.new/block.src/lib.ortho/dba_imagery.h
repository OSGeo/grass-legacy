#include "gis.h"
#include "imagedefs.h"

struct Block_Image_Group_Ref
{
    int nfiles;
    struct Block_Image_Group_Ref_Files
    {
	char name[30];
	char mapset[30];
	int  group_ref_points_stat;
	int  group_con_points_stat;
	int  group_initial_exp_stat;
	int  group_final_exp_stat;
    } *file;
} ;

struct Camera_File_Ref
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
 } *cam_file;

struct Ref_Points
{   int count;
    double *e1;
    double *n1;    
    double *e2;    
    double *n2;
    int  *status;
};

struct Con_Points
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

struct Camera_Exp_Init
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




