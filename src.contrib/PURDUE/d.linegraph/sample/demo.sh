#!/bin/sh

#
# this is a shell script to demonstrate the usage of the d.linegraph
# program. you need to be running grass 4.0 with a graphics display
# monitor running and selected. (using d.mon).
# this shell script will draw example graphs from the sample data
# and wait for the return key to continue
#

#
# set DB variable to relative path to the directory containing data
#

DB=./data

echo "Demo program"
echo ""
echo "database is $DB"

d.erase

d.erase
../cmd/d.linegraph directory=./data x_file=hydro_time y_file=hydro_rain y_color=blue title=Outlet_Hydrograph y_title=Rainfall_mm/h x_title=Minutes
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

d.erase
../cmd/d.linegraph directory=./data x_file=hydro_time y_file=hydro_runoff y_color=magenta title=Outlet_Hydrograph y_title=Runoff_mm/h x_title=Minutes
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

d.erase
../cmd/d.linegraph directory=./data x_file=hydro_time y_file=hydro_sed1 y_color=brown title=Sediment_Yield y_title=Sediment_Kg x_title=Minutes
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

d.erase
../cmd/d.linegraph directory=./data x_file=hydro_time y_file=hydro_sed2 y_color=yellow title=Outlet_Hydrograph y_title=Sediment_Concentration_mg/l x_title=Minutes
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

d.erase
../cmd/d.linegraph directory=../sample/data x_file=x2 y_file=y1,y2,y3,y4,y5 x_title=Title_for_x_data y_title=Title_for_Y_lines title=Main_title_for_graph title_color=yellow
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

