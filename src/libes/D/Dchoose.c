Dchoose(name)
	char *name ;
{
	int stat ;

	if(stat = D_set_cur_wind(name))
		return(stat) ;
	else
		D_timestamp() ;
}
