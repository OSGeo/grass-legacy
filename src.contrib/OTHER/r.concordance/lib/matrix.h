#ifndef __MATX__H
#define __MATX__H

class Matrix1D
{
  double* _data;
  bool    _error;
public:
  double& operator()(int i) { return _data[i]; }
  Matrix1D(int n);
  ~Matrix1D();
};

class Matrix2D
{
  double** _data;
  bool     _error;
  int      _x;
public:
  double& operator()(int i, int k) { return _data[i][k]; }
  Matrix2D(int n, int k);
  ~Matrix2D();
};

class Matrix3D
{
  double*** _data;
  bool    _error;
  int     _x, _y;
public:
  double& operator()(int i, int j, int k) { return _data[i][j][k]; }
  Matrix3D(int n, int k, int m);
  ~Matrix3D();
};

#endif




