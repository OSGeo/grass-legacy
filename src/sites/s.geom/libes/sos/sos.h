/* sos/sos.h  ---  SoS Library headerfile. */

#ifndef __SOS_H__  /* Include only once! */
#define __SOS_H__  1

/*--------------------------------------------------------------------------*/

#include "basic.h"
#include "lia.h"

/*--------------------------------------------------------------------------*/

/* sos.c */

void    sos_init();
int     sos_bytes();
Lia_ptr sos_lia();
double  sos_scale();
void    sos_param();

#ifdef __DEBUG__
/* globally accessible flags! */
extern int sos_test_flag;
extern int sos_proto_flag;
extern int sos_proto_e_flag;
#endif

/*--------------------------------------------------------------------------*/

/* predicates */

int  sos_smaller();
int  sos_smaller_dist_abcd();
int  sos_above3();
int  sos_above3_star();
void sos_above3_star_set();
int  sos_above4();
int  sos_positive3();
int  sos_in_sphere();
int  sos_in_sphere_p();

/*--------------------------------------------------------------------------*/

/* non-boolean primitive operations */
void sos_rho3();
void sos_rho2();
void sos_rho1();

/*--------------------------------------------------------------------------*/

/* primitives */

typedef struct sos_primitive_result_type
{
  int signum;
  Lia_ptr lia_pointer;
  int depth;
  int two_k;
  int *epsilon;
} SoS_primitive_result;

SoS_primitive_result * sos_lambda3();
SoS_primitive_result * sos_lambda4();
SoS_primitive_result * sos_lambda5();

SoS_primitive_result * sos_lambda3_star();
SoS_primitive_result * sos_lambda4_star();

SoS_primitive_result * sos_rho3_num(),  * sos_rho3_den();
SoS_primitive_result * sos_rho2_num(),  * sos_rho2_den();
SoS_primitive_result * sos_rho1_num(),  * sos_rho1_den();

void sos_depth_counters_output();
void sos_depth_counters_summary();

/*--------------------------------------------------------------------------*/

/* minor.c */

void    sos_minor();
Lia_ptr sos_minor1();
Lia_ptr sos_minor2();
Lia_ptr sos_minor3();
Lia_ptr sos_minor4();
Lia_ptr sos_minor5();

/*--------------------------------------------------------------------------*/

/*                .........................................................
                  "If we do not succeed, then we face the risk of failure."
                  -- Dan Quayle, Vice-President of the United States [1990]
                  ......................................................... */

#endif  /* #ifndef __SOS_H__ */
