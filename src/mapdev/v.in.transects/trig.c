extern double sin(), cos();
#define PI 3.14159

double 
dcos (double x)
{
	return cos(x * PI/180.0);
}

double 
dsin (double x)
{
	return sin(x * PI/180.0);
}
