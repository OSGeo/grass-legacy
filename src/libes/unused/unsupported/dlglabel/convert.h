/*  @(#)convert.h	1.1  5/4/87  */
#define MAX_WINDOWS		10

int c_wind ;   /* Current window */

double _west[MAX_WINDOWS]   ;  /*  western edge  (UTM/meters)  */
double _east[MAX_WINDOWS]   ;  /*  eastern edge  (UTM/meters)  */
double _south[MAX_WINDOWS]  ;  /*  southern edge (UTM/meters)  */
double _north[MAX_WINDOWS]  ;  /*  northern edge (UTM/meters)  */

double U_west   ; /*  western edge  (UTM) */
double U_east   ; /*  eastern edge  (UTM) */
double U_south  ; /*  southern edge (UTM) */
double U_north  ; /*  northern edge (UTM) */

double D_west   ; /*  western edge  (screen dots) */
double D_east   ; /*  eastern edge  (screen dots) */
double D_south  ; /*  southern edge (screen dots) */
double D_north  ; /*  northern edge (screen dots) */


double U_to_D_xconv, U_to_D_yconv ;     /* UTM to Dot   */
