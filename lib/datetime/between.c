
int 
datetime_is_between (int x, int a, int b)
{
    if (a <= b)
	return a <= x && x <= b;
    else
	return b <= x && x <= a;
}
