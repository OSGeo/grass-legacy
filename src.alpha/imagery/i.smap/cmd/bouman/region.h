struct Region
{
    int xmin,xmax;
    int ymin,ymax;
    struct Free
    {
        int left;
	int right;
        int top;
        int bottom;
    } free;
};

