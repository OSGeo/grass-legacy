extern double sin(), cos();
#define PI 3.14159

double
dcos(x)
	double x;
{
	return cos(x * PI/180.0);
}

double
dsin(x)
	double x;
{
	return sin(x * PI/180.0);
}
