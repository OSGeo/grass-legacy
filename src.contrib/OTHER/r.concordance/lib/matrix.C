#include "matrix.H"
#include <memory.h>
#include <malloc.h>
#include <stdio.h>

extern "C" {
void free(void*);
}

typedef double *dblp;
typedef double **dblpp;

Matrix1D::Matrix1D(int n) : _error(0)
{
  if ((_data  = (double*)calloc(n,sizeof(double))) == NULL) _error = 1;
} 

Matrix1D::~Matrix1D()
{
  delete _data;
} 

Matrix2D::Matrix2D(int n, int m) : _error(0)
{
  _x = n; 
  if ((_data  = (double**)calloc(n,sizeof(double*))) == NULL) 
    {
      _error = 1;
      return;
    }
  
  for (int i = 0; i < n; i++)
    {
      if ((_data[i] = (double*)calloc(m,sizeof(double))) == NULL) 
	{
	  _error = 1;
	  break;
	}
    }  
} 

Matrix2D::~Matrix2D()
{
  if (_error) return;
  for (int i = 0; i < _x; i++)
      free(_data[i]);
  delete _data;
} 

Matrix3D::Matrix3D(int n, int m, int z) : _error(0)
{
  _x = n; _y = m;

  if ((_data  = (double***)calloc(n,sizeof(double**))) == NULL) 
    {
      _error = 1;
      return;
    }
  
  for (int i = 0; i < n; i++)
    {
     if ((_data[i] = (double**)calloc(m,sizeof(double*))) == NULL) 
	{
	  _error = 1;
	  break;
	}
      else
	for (int j = 0; j < m; j++)
	  {
	    if ((_data[i][j] = (double*)calloc(z,sizeof(double))) == NULL) 
	      {
		_error = 1;
		break;
	      }
	  }  
    }  
} 

Matrix3D::~Matrix3D()
{
  if (_error) return;
  for (int i = 0; i < _x; i++)
    {
     for (int j = 0; j < _y; j++)
	{
	  free(_data[i][j]);
	}
     free(_data[i]);
  }
  delete _data;
} 

