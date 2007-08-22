/* File: driver.i */

%module grass6_wxdriver
%{
#include <grass/gis.h>
#include <grass/gisdefs.h>
#include <grass/Vect.h>
#include <grass/vect/dig_structs.h>
#include "driver.h"
%}

%include "std_vector.i" 
namespace std { 
   %template(IntVector) vector<int>;
}
