%module Grass
%{
#include "grass/gis.h"
%}
%include "../dist.i686-pc-linux-gnu/include/grass/gisdefs.h"

%include typemaps.i

void set_my_error_routine();
