#!/bin/bash --
######################################
# Frontend for r.mapcalc             #
# R. Brunzema                        #
# mailto: r.brunzema@web.de          #
######################################

function display_help {
	echo "You can create a calculation formula in the field"
	echo "formula like this:"
	echo "A+C or (more complex:) exp(A+C)+(B-2)*7"
	echo "A, B, C, etc. are your coosen raster"
	echo "maps. You must not insert the output file:"
	echo "Right: A+B"
	echo "Wrong: newfile = A+B"
	echo "Use no blanks!"
	echo "For details on creating a formula see the r.mapcalc"
	echo "manual page (man r.mapcalc)."
}

if [ "$GISBASE" = "" ]; then
	echo "you maust be in GRASS to run this program"
	exit 1
fi

if [ -z "$1" ]; then
	display_help
	exit 0
fi

case $1 in
	help=help)
	display_help
	exit 0
	;;
	help=man)
	g.manual r.mapcalc
	exit 0
	;;
	expert=expert)
	# use old style mapcalc
	xterm -e r.mapcalc &
	exit 0
	;;
esac

for args 
	do
  	case $args in
			# look for raster map names
			A_MAP=*)
			amap="${args#*=}"
			;;
			B_MAP=*)
			bmap="${args#*=}"
			;;
			C_MAP=*)
			cmap="${args#*=}"
			;;
			D_MAP=*)
			dmap="${args#*=}"
			;;
			E_MAP=*)
			emap="${args#*=}"
			;;
			F_MAP=*)
			fmap="${args#*=}"
			;;
			formula=*)
			formula="${args#*=}"
			;;
			outfile=*)
			outfile="${args#*=}"
			;;
			not_over=true)
			n_o=true
			;;
		esac
	done
	
# Check for required arguments
if [ -z "$formula" ]; then
	echo "ERROR: Missing formula!"
	echo "Please enter a formula in the field formula"
fi

if [ -z "$outfile" ]; then
	echo "ERROR: Missing name of outputfile!"
	echo "Please enter a name for the resulting map and try again."
	exit 1
elif [ "$n_o" = true ]; then
		echo `g.list type=rast | grep -w "$outfile" `
		outtest=`g.list type=rast | grep -w $outfile`
		if [ -n "$outtest" ]; then
			echo "File $outfile exists. Exiting."
			exit 0
		fi		
fi

typeset -i form_length=${#formula}
typeset -i count=0

# Get the parts of the formula,
while [ $count -le $form_length ] 
	do
	compart[$count]="${formula:$count:1}"
	# ... get the rasterfile names
	case ${compart[$count]} in
		A)
		# ... replace them
		compart[count]=" \"$amap\" "
		;;
		B)
		compart[count]=" \"$bmap\" "
		;;
		C)
		compart[count]=" \"$cmap\" "
		;;
		D)
		compart[count]=" \"$dmap\" "
		;;
		E)
		compart[count]=" \"$emap\" "
		;;
		F)
		compart[count]=" \"$fmap\" "
		;;
	esac
	# .. and put the formula together again
	command="$command${compart[$count]}"
	let count=count+1
done

echo r.mapcalc "$outfile" ="($command)" # Show the resulting commandline

r.mapcalc "$outfile" ="($command)" # Start the command

# Check for errors
if [ $? -ne 0 ]; then
	echo "Error calculating $outfile. Try expert mode."
fi

exit	