d.mon x0

echo
echo "Prepare for simulation:"
g.remove my_ros.max,my_ros.base,my_ros.maxdir,my_ros.spotdist,\
my_spread,my_spread.x,my_spread.y

echo
echo "Calculate the ROS (rate of spread), etc.:"
r.ros -sv model=fuel_model moisture_1h=1hour_moisture \
 	moisture_live=live_moisture velocity=wind_speed \
	direction=wind_direction slope=slope aspect=aspect \
	elevation=elevation output=my_ros


d.frame -s at=0,100,0,50
echo
echo "Spread:"
r.spread -ds max=my_ros.max base=my_ros.base dir=my_ros.maxdir \
	spot_dist=my_ros.spotdist w_speed=wind_speed f_mois=1hour_moisture \
	start=fire_origin lag=95 backdrop=image_burned output=my_spread \
	x_output=my_spread.x y_output=my_spread.y

echo "Spread done."
echo
echo "Find some fire paths:"
echo "Thanks for your interest!"
r.random my_spread nsites=10 raster_output=my_path
r.spreadpath -v x_input=my_spread.x y_input=my_spread.y \
	output=my_path
d.frame -s at=0,100,50,100
d.erase
d.rast -o image_burned
d.rast -o my_path
echo
echo "Simulation done. Thanks for your interest!"
