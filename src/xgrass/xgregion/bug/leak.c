int func()
{
  return malloc(50000);
}

int main()
{
	int d;
	int e;
	printf("hello world\n");
	d=func();
	e = d * 2;
	d = 0;
	return e;
}

