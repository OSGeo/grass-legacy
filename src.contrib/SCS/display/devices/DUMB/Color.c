color(number)
	int number;
{
	extern int cur_color;

	cur_color = number;
	color_set(number);
}
