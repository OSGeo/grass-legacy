move(x,y)
{
}
cont(x,y)
{
}
setup_graphics()
{
    extern double D_get_d_north();
    extern double D_get_d_south();
    extern double D_get_d_west();
    extern double D_get_d_east();


    D_setup(0);
    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	move, cont);

}
