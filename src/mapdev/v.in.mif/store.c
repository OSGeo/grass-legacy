/******************************************************************************
 * store.c [v.in.mif]
 * Static buffers for storage of parameters

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 9th. Jan. 2001
 * Last updated 13th. Jan. 2001
 *

 * This file is part of GRASS GIS. It is free software. You can 
 * redistribute it and/or modify it under the terms of 
 * the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 ******************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include "store.h"

int G_process_snap_distance(int fflag, double *snap) {

  /* Process snap distance entries */

  static double sn = 1.0e-10;

  if( fflag == GET_VAL ) {
    if(snap) {
      *snap = sn;
      return 0;
    }
    else return -1;
  }

  else if( fflag == SET_VAL ) {
    
    if(snap) {
      sn = *snap;
      return 0;
    }
    else return -1;
  }

  else return -1;
}

int G_process_colinear_tolerance(int fflag, double *tolerance) {

  /* Process colinear angular tolerance entries */

  static double tol0 = 1.745e-4;

  if( fflag == GET_VAL ) {
    if(tolerance) {
      *tolerance = tol0;
      return 0;
    }
    else return -1;
  }

  else if( fflag == SET_VAL ) {
    
    if(tolerance) {
      tol0 = *tolerance;
      return 0;
    }
    else return -1;
  }

  else return -1;
}

int
G_process_bbox_params(int fflag, double *fe, double *fn) {

  static double e0 = 0.0;
  static double n0 = 0.0;

  if( fflag == GET_VAL ) {
    if(fe && fn) {
      *fe = e0;
      *fn = n0;
      return 0;
    }
    else return -1;
  }

  else if( fflag == SET_VAL ) {
    
    if(fe && fn) {
      e0 = *fe;
      n0 = *fn;
      return 0;
    }
    else return -1;
  }

  else return -1;

}

int
G_process_key_params(int fflag, int *digits, double *fe, double *fn) {

  static double e0 = 0.0;
  static double n0 = 0.0;
  static int idig = 10;

  if( fflag == GET_VAL ) {
    if(fe && fn) {
      *fe = e0;
      *fn = n0;
      *digits = idig;
      return 0;
    }
    else return -1;
  }

  else if( fflag == SET_VAL ) {
    
    if(fe && fn) {
      e0 = *fe;
      n0 = *fn;
      idig = *digits;
      return 0;
    }
    else return -1;
  }

  else return -1;

}

int 
G_process_scale_value(int flag, int *scale_val) {

  static int scale_ = 200;

  if(!scale_val) {
    fprintf( stderr, "Could not set scale value\n");
    return -1;
  }

  if( flag == SET_VAL ) {
    scale_ = *scale_val;
    return 0;
  }

  else if( flag == GET_VAL ) {
    *scale_val = scale_;
    return 0;
  }
  
  else return -1;
  
}
