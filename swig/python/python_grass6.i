//File : python_grass6.i


%module python_grass6
%{
#include <grass/gis.h>
#include <grass/gisdefs.h>
#include <grass/imagery.h>
#include <grass/imagedefs.h>
%}

%include "my_typemaps.i"
%include "renames.i"
%include "interfaces/gis.i"
%include "interfaces/gisdefs.i"
%include "interfaces/imagery.i"
%include "interfaces/imagedefs.i"


