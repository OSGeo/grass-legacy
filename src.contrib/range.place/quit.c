 struct box *quit(box)

        struct box *box;
{
	extern int button;
	
        button = 4;
        return(box);
}

