/* File : libgis.i */
%module libgis

%{
#include "gis.h"
#include "gisdefs.h"
%}

/* Swig doesn't like register function arguments */
#define register

/* Swig doesn't like nested struct declaration */
#define SWIGGING

%include "gis.h"
%include "gisdefs.h"