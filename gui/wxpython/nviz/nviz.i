/* File: nviz.i */

%module grass6_wxnviz
%{
#include "nviz.h"
#include <grass/gsurf.h>
#include <grass/gstypes.h>
#undef check
%}

%include "std_vector.i"
namespace std { 
   %template(IntVector) vector<int>;
   %template(DoubleVector) vector<double>;
}
%include "std_map.i"
namespace std { 
   %template(IntVecIntMap) map<int, vector<int> >;
}

%include "nviz.h"
