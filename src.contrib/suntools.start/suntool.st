
if [ `tty` != /dev/console ]
then
	if [ `who | grep console | wc -l` = 0 ]
	then
		echo Checking status of suntools
		rm -f /tmp/grass.$$
		ps -aux > /tmp/grass.$$
		if [ `grep suntools /tmp/grass.$$ | wc -l` = 1 ]
		then
			if [ `grep grass.sun.dr /tmp/grass.$$ | wc -l` = 1 ]
			then
				echo "  Suntools and grass.sun.driver are ready"
				/usr/grass3/bin/Dselect.mon SUN
				m.screen
			else
				echo "  Warning: suntools is running, but"
				echo "    the Grass driver is not.  You must"
				echo "    either stop suntools and start GRASS"
				echo "    again or start the grass driver by hand"
			fi
		else
			(suntools -S -s /usr/grass3/.suntools.grass &)
			echo Waiting 30 seconds for suntools to start up
			sleep 30
			Dselect.mon SUN
			m.screen
		fi
		rm /tmp/grass.$$
	else
		echo Can not start graphics with someone working on the console
	fi
else
	echo You must run GRASS on another terminal to get graphics on this device
fi
