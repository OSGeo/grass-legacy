#!/bin/sh

# Chris Rewerts, Agricultual Engineering, Purdue University.
# rewerts@ecn.purdue.edu
# demo.sh - a shellscript to demonstrate the d.linegraph program.

# USAGE:
# Set the DB variable to location of the sample data.
# Start a graphics monitor.
# demo.sh


# $DB is the relative path to the sample data
# You can change this to a full path to the data on your file system
# if you want to run the demo from another directory

DB=./data

echo ""
echo "d.linegraph demonstation shellscript"
echo ""
echo "data directory is set to $DB"

d.erase

d.linegraph directory=$DB x_file=hydro_time y_file=hydro_rain y_color=blue title=Outlet_Hydrograph y_title=Rainfall_mm/h x_title=Minutes
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

d.erase
d.linegraph directory=$DB x_file=hydro_time y_file=hydro_runoff y_color=magenta title=Outlet_Hydrograph y_title=Runoff_mm/h x_title=Minutes
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

d.erase
d.linegraph directory=$DB x_file=hydro_time y_file=hydro_sed1 y_color=brown title=Sediment_Yield y_title=Sediment_Kg x_title=Minutes
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

d.erase
d.linegraph directory=$DB x_file=hydro_time y_file=hydro_sed2 y_color=yellow title=Outlet_Hydrograph y_title=Sediment_Concentration_mg/l x_title=Minutes
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

d.erase
d.linegraph directory=../sample/data x_file=x2 y_file=y1,y2,y3,y4,y5 x_title=Title_for_x_data y_title=Title_for_Y_lines title=Main_title_for_graph title_color=yellow
clear
echo ""
echo -n "Hit RETURN to continue "
read ans

