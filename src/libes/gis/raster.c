#include "gis.h"

void *G_incr_void_ptr(
/* advances ptr by size bytes returns new position */
   void *ptr,
   int size)
{
   /* assuming that the size of unsigned char is 1 */
   return (void *) ((unsigned char *) ptr + size);
}

int G_raster_cmp( void *v1,void *v2, RASTER_MAP_TYPE data_type)
{
    if(G_is_null_value(v1, data_type ) )
    {
       if (G_is_null_value(v2, data_type ))
          return 0;
       else return -1;
    }
    else if(G_is_null_value(v2, data_type ) )
       return 1;

    switch (data_type)
    {
       case CELL_TYPE:  if(*((CELL *) v1) > *((CELL *) v2))
			     return 1;
                        else if(*((CELL *) v1) == *((CELL *) v2))
			     return 0;
                        else return -1;
       case FCELL_TYPE: if(*((FCELL *) v1) > *((FCELL *) v2))
			     return 1;
                        else if(*((FCELL *) v1) == *((FCELL *) v2))
			     return 0;
                        else return -1;
       case DCELL_TYPE: if(*((DCELL *) v1) > *((DCELL *) v2))
			     return 1;
                        else if(*((DCELL *) v1) == *((DCELL *) v2))
			     return 0;
                        else return -1;
     }

     return 0;
}

int G_raster_cpy(
    void *v1,void *v2,
    int n,
    RASTER_MAP_TYPE data_type)
{
    G_copy((char *) v1, (char *) v2, n * G_raster_size(data_type));
    return 0;
}

int G_set_raster_value_c(
    void *rast,
    CELL cval,
    RASTER_MAP_TYPE data_type)
{
    CELL c;
    c = cval;
    if(G_is_c_null_value(&c))
    {
       G_set_null_value(rast, 1, data_type);
       return 0;
    }
    switch (data_type)
    {
       case CELL_TYPE: *((CELL *)rast) = cval; break;
       case FCELL_TYPE: *((FCELL *)rast) = (FCELL ) cval; break;
       case DCELL_TYPE: *((DCELL *)rast) = (DCELL ) cval; break;
    }

    return 0;
}

int G_set_raster_value_f(
    void *rast,
    FCELL fval,
    RASTER_MAP_TYPE data_type)
{
    FCELL f;
    f = fval;
    if(G_is_f_null_value(&f))
    {
       G_set_null_value(rast, 1, data_type);
       return 0;
    }
    switch (data_type)
    {
       case CELL_TYPE: *((CELL *)rast) = (CELL ) fval; break;
       case FCELL_TYPE: *((FCELL *)rast) = fval; break;
       case DCELL_TYPE: *((DCELL *)rast) = (DCELL ) fval; break;
    }

    return 0;
}

int G_set_raster_value_d(
    void *rast,
    DCELL dval,
    RASTER_MAP_TYPE data_type)
{
    DCELL d;
    d = dval;
    if(G_is_d_null_value(&d))
    {
       G_set_null_value(rast, 1, data_type);
       return -1;
    }
    switch (data_type)
    {
       case CELL_TYPE: *((CELL *)rast) = (CELL ) dval; break;
       case FCELL_TYPE: *((FCELL *)rast) = (FCELL ) dval; break;
       case DCELL_TYPE: *((DCELL *)rast) = dval; break;
    }

    return 0;
}

CELL G_get_raster_value_c(
    void *rast,
    RASTER_MAP_TYPE data_type)
{
    CELL c;
    if(G_is_null_value(rast, data_type))
    {
       G_set_c_null_value(&c, 1);
       return c;
    }
    switch (data_type)
    {
       case CELL_TYPE: return *((CELL *)rast);
       case FCELL_TYPE: return (CELL) *((FCELL *)rast);
       case DCELL_TYPE: return (CELL) *((DCELL *)rast);
    }

    return 0;
}

FCELL G_get_raster_value_f(
    void *rast,
    RASTER_MAP_TYPE data_type)
{
    FCELL f;
    if(G_is_null_value(rast, data_type))
    {
       G_set_f_null_value(&f, 1);
       return f;
    }
    switch (data_type)
    {
       case CELL_TYPE: return (FCELL) *((CELL *)rast);
       case FCELL_TYPE: return *((FCELL *)rast);
       case DCELL_TYPE: return (FCELL) *((DCELL *)rast);
    }

    return 0;
}

DCELL G_get_raster_value_d(
    void *rast,
    RASTER_MAP_TYPE data_type)
{
    DCELL d;
    if(G_is_null_value(rast, data_type))
    {
       G_set_d_null_value(&d, 1);
       return d;
    }
    switch (data_type)
    {
       case CELL_TYPE: return (DCELL) *((CELL *)rast);
       case FCELL_TYPE: return (DCELL) *((FCELL *)rast);
       case DCELL_TYPE: return *((DCELL *)rast);
    }

    return 0;
}
