extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_east();
extern double D_get_d_west();
extern void D_move_abs();
extern void D_cont_abs();

setup()
{
	D_setup(0);
	G_setup_plot (D_get_d_north(), D_get_d_south(),
	              D_get_d_west(),  D_get_d_east(),
	              D_move_abs,      D_cont_abs);
}
