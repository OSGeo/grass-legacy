static int which_env = -1; /* 0 = cur, 1 = target */

select_current_env()
{
    if (which_env < 0)
    {
	G__create_alt_env();
	which_env = 0;
    }
    if (which_env != 0)
    {
	G__switch_env();
	which_env = 0;
    }
}

select_target_env()
{
    if (which_env < 0)
    {
	G__create_alt_env();
	which_env = 1;
    }
    if (which_env != 1)
    {
	G__switch_env();
	which_env = 1;
    }
}




