#!../glnviz/nvwish -f
# 9/2/94
# M. Astley
# USACERL, blah blah blah
##########################################################################
# The following application is used to provide a visual interface to
# the mkdspf program.  Basically we allow the user to specify an input
# grid3 file and an output dspf file and allow a variety of methods
# for selecting the isosurfaces to be extracted.
#
##########################################################################

# Changes
#
# 9/12/94
# Incorporated .mkdspf into an Nvision panel by creating the
# mkmkdspfPanel procedure
#

# The following procedures each create custom widgets of
# some specified type.  We use these widgets to implement
# features which tk doesn't provide

# This procedure creates a single select listbox widget with
# a vertical scrollbar.
# Note that the "exportselection" setting is important
# as it allows mulitple selection across several lists.
# WIDGET:	list_type1
# CHILDREN:	listbox called $window.l
# 		vertical scrollbar called $window.sr
#		label called $window.t
proc list_type1 {window} {
	frame $window -relief raised 
	listbox $window.l -relief raised -yscrollcommand "$window.sr set" -exportselection 0 -selectmode single
	scrollbar $window.sr -command "$window.l yview"
	pack $window -side bottom -expand yes -fill both
	pack $window.l -side left -expand yes -fill both
	pack $window.sr -side right -fill y
}

##########################################################################

proc mkmkdspfPanel { BASE } {

	set panel [St_create {window name size priority} $BASE "Mkdspf" 2 5]
	frame $BASE -relief groove -border 4
	Nv_mkPanelname $BASE "Mkdspf Panel"

	##########################################################################
	# First section contains slots for input file name, output file
	# name, and color file name
	frame $BASE.files -relief groove -border 1

	frame $BASE.files.texts
	label $BASE.files.texts.infile  -text "Input File:"  -relief flat
	label $BASE.files.texts.outfile -text "Output File:" -relief flat
	label $BASE.files.texts.cfile   -text "Color File:"  -relief flat
	pack $BASE.files.texts.infile $BASE.files.texts.outfile \
	$BASE.files.texts.cfile -side top -fill both -expand yes -padx 2 -pady 2

	frame $BASE.files.ents
	entry $BASE.files.ents.infile  -relief sunken
	entry $BASE.files.ents.outfile -relief sunken
	entry $BASE.files.ents.cfile   -relief sunken
	pack $BASE.files.ents.infile $BASE.files.ents.outfile \
	$BASE.files.ents.cfile -side top -fill both -expand yes -padx 2 -pady 2

	pack $BASE.files.texts $BASE.files.ents -side left -fill both -padx 2 -pady 2
	
	pack $BASE.files -side top -padx 2 -pady 2 -fill x -expand yes

	##########################################################################
	# Second section contains a pair of radiobuttons for selecting the
	# lighting model
	frame $BASE.lightmodel -relief groove -bd 2

	global mkdspf_lightmodel
	set mkdspf_lightmodel flat
	label $BASE.lightmodel.title -text "Lighting Model: " -relief flat
	radiobutton $BASE.lightmodel.flat -text "flat" \
		-variable mkdspf_lightmodel -value flat
	radiobutton $BASE.lightmodel.gradient -text "gradient" \
		-variable mkdspf_lightmodel -value gradient
	pack $BASE.lightmodel.title $BASE.lightmodel.flat $BASE.lightmodel.gradient \
	-side left -fill y -padx 2 -pady 2
	
	pack $BASE.lightmodel -side top -padx 2 -pady 2 -fill x -expand yes

	##########################################################################
	# Make a "Go" button to do everything
	frame $BASE.control -relief groove -border 2
	button $BASE.control.start -text "Accept" -command "mkdspf_go $BASE"
	button $BASE.control.cancel -text "Close" -command "Nv_closePanel $BASE"
	pack $BASE.control.start -fill y -side left
	pack $BASE.control.cancel -fill y -side right
	
	pack $BASE.control -side bottom -padx 2 -pady 2 -fill x -expand yes

	# Seperator
	Nv_makeSeparator $BASE.sep1   
	pack $BASE.sep1 -side top -fill x
	##########################################################################
	##########################################################################
	# Third section contains controls for selecting the threshold type
	frame $BASE.threshold -relief groove -border 2
	pack $BASE.threshold -padx 2 -side top \
		-fill both -expand yes
		
	global mkdspf_threshtype
	set mkdspf_threshtype individual
	label $BASE.threshold.title -text "Threshold Type: " -relief flat
	
	# Create a menu selector for choosing a particular threshold panel
	menubutton $BASE.threshold.list -text "Complete" \
	-menu $BASE.threshold.list.m -relief flat
	menu $BASE.threshold.list.m
	set pname $BASE.threshold.list.m
	$pname add command -label "Individual" -command "shuffle_threshold $BASE individual"
	$pname add command -label "Complete" -command "shuffle_threshold $BASE complete"
	$pname add command -label "Range" -command "shuffle_threshold $BASE range"
	pack $BASE.threshold.title $BASE.threshold.list \
	-side left -fill y -padx 4
	

	###############################################################################	
	# Threshold type is broken into three sub-panels: complete, range and individual

	################################
	# Create the "complete" subpanel
	set pname $BASE.complete
	frame $pname -relief raised -bd 3
	label $pname.isize_title -text "Interval Size:" -relief flat
	entry $pname.isize_entry -relief sunken -width 10
	pack $pname.isize_title $pname.isize_entry -side left -fill y -padx 2 -pady 2


	##################################
	# Create the "range" subpanel
	set pname $BASE.range
	frame $pname -relief raised -bd 3

	frame $pname.levels
	label $pname.levels.label1 -text "Min:" -relief flat
	entry $pname.levels.entry1 -relief sunken -width 7
	label $pname.levels.label2 -text "Max:" -relief flat
	entry $pname.levels.entry2 -relief sunken -width 7
	label $pname.levels.label3 -text "Levels:" -relief flat
	entry $pname.levels.entry3 -relief sunken -width 7
	
	pack $pname.levels.label1 $pname.levels.entry1 \
	$pname.levels.label2 $pname.levels.entry2 \
	$pname.levels.label3 $pname.levels.entry3 \
	-side left -fill y
	
	global mkdspf_range_interp
	set mkdspf_range_interp linear
	frame $pname.itype
	frame $pname.itype1
	frame $pname.itype2
	frame $pname.itype3
	label $pname.itype.title -text "Interpolation Type" -relief flat
	radiobutton $pname.itype1.lin -text "Linear" -relief flat \
		-variable mkdspf_range_interp -value linear
		
	radiobutton $pname.itype1.log -text "Logarithmic" -relief flat \
		-variable mkdspf_range_interp -value logarithmic
		
	radiobutton $pname.itype1.qua -text "Quadratic"	-relief flat \
		-variable mkdspf_range_interp -value quadratic
		
	radiobutton $pname.itype2.arb -text "Arbitrary"	-relief flat \
		-variable mkdspf_range_interp -value arbitrary
		
	entry $pname.itype2.arb_entry -relief sunken -width 5
	
	pack $pname.itype.title -side top -fill x -padx 2 -pady 2
	pack $pname.itype1.lin $pname.itype1.log $pname.itype1.qua \
	-side left -padx 2 -fill y
	pack $pname.itype2.arb $pname.itype2.arb_entry -side left -padx 2 \
		-fill y

	pack $pname.levels $pname.itype \
		$pname.itype1 $pname.itype2 $pname.itype3 \
		-fill both -expand yes -side top

	###################################
	# Create the "individual" subpanel
	set pname $BASE.individual
	frame $pname -relief raised -bd 3
	list_type1 $pname.list
	label $pname.label -relief flat -text "Iso Levels: "
	entry $pname.level -relief sunken -width 10
	bind $pname.level <Return> "individual_add $BASE"
	button $pname.addb -text "Add"    -command "individual_add $BASE"
	button $pname.delb -text "Delete" -command "individual_delete $BASE"
	pack $pname.label $pname.level $pname.addb $pname.delb \
	-side left -fill y -padx 2
	pack $pname.list -side bottom -expand yes -fill both
		

	shuffle_threshold $BASE individual


	return $panel
}

# A quick routine for shuffling the current threshold type
proc shuffle_threshold { BASE new } {
	catch {pack forget $BASE.complete}
	catch {pack forget $BASE.range}
	catch {pack forget $BASE.individual}
	pack $BASE.$new -side bottom -fill both -expand yes
		
	switch $new {
		complete	{ $BASE.threshold.list configure -text "Complete" }
		range		{ $BASE.threshold.list configure -text "Range" }
		individual	{ $BASE.threshold.list configure -text "Individual" }
	}

	global mkdspf_threshtype
	set mkdspf_threshtype $new
}

# Two quick routines to add or delete isosurface levels for
# selecting them individually
proc individual_add { BASE } {
	# For this routine we just use the value stored in the
	# entry widget
	# Get the value from the entry widget
	set level [$BASE.individual.level get]

	# Now just append it to the list
	$BASE.individual.list.l insert end $level
}

proc individual_delete { BASE } {
	# For this procedure we require that the user has selected
	# a range of values in the list which we delete
	# Get the range of selections
	set range [$BASE.individual.list.l curselection]
	
	# Now delete the entries
	foreach i $range {
		$BASE.individual.list.l delete $i
	}
}


##########################################################################
# Here's the main routine which puts everything together for the
# call to mkdspf

proc mkdspf_go { BASE } {
	global mkdspf_lightmodel
	global mkdspf_threshtype
	global mkdspf_range_interp

	# First grab file names
	set in_name [$BASE.files.ents.infile get]
	set out_name [$BASE.files.ents.outfile get]

	# Now get shading model to use
	if {"$mkdspf_lightmodel" == "flat"} then {
		set light_model f
	} else {
		set light_model g
	}

	# Finally figure out the threshold type and create the appropriate args
	switch $mkdspf_threshtype {
		complete {
			# First set the threshold type
			set thresh_type c

			# Now figure out the interval size to use
			set thresh_args [$BASE.threshold.complete.isize_entry get]
		}
		range {
			# First figure out the threshold type
			set thresh_type i
			set num_levels [$BASE.threshold.range.levels.entry get]
			set min_level [$BASE.threshold.range.rmin.mentry get]
			set max_level [$BASE.threshold.range.rmax.mentry get]
			set arb_func [$BASE.threshold.range.itype.arb_entry get]

			# Now create threshold levels depending on the type of interpolation
			# specified
			switch $mkdspf_range_interp {
				linear { 
					set thresh_args [mkdspf_map_range x $num_levels $min_level $max_level] 
				}
				logarithmic {
					set thresh_args [mkdspf_map_range "1/x" $num_levels $min_level $max_level]
				}
				quadratic {
					set thresh_args [mkdspf_map_range "x*x" $num_levels $min_level $max_level]
				}
				arbitrary {
					set thresh_args [mkdspf_map_range "$arb_func" $num_levels $min_level $max_level]
				}
			}
		}
		individual {
			# First figure out the threshold type
			set thresh_type i

			# Now create a list for the individual thresholds
			set thresh_args [list]
			set list_size [$BASE.threshold.individual.list.l size]
			for {set i 0} {$i < $list_size} {incr i} {
				lappend thresh_args [$BASE.threshold.individual.list.l get $i]
			}
		}
	}

# Lastly make the dspf call
# puts "mkdspf $in_name $out_name $thresh_type $thresh_args $light_model"

	puts "r3.mkdspf $in_name out=$out_name"
	catch {r3.mkdspf $in_name out=$out_name} my_out
	puts "$my_out"
}

# Simple function to create the range interpolation the user desires
# Arguments:
#		func - A function of the variable x valid for tcl/tk function "expr"
#		num_levels - Total number of levels to specify
#		min - minimum threshold value
#		max - max threshold value
#
# Returns a list of threshold values spaced equally in terms of "func"
proc mkdspf_map_range {func num_levels min max} {
	# Create a temporary list which we linearly map to
	# the desired range
	set tlist [list]

	# Create a new function by replacing each x term with $x
	regsub -all x $func {$x} new_func

	# Now do the mapping
	for {set i 1.0} {$i < [expr $num_levels + 1]} {set i [expr $i + 1.0]} {
		set x $i
		lappend tlist [expr $new_func]		
	}
	set tlist [lsort -real $tlist]

	# Finally map back to the original range
	set rlist [list]
	set lmin [lindex $tlist 0]
	set lmax [lindex $tlist [expr [llength $tlist] - 1]]
	foreach k $tlist {
		set i [expr 0.0 + ($k - $lmin) / ($lmax - $lmin)]
		set j [expr 0.0 + $min + ($max - $min) * $i]
		lappend rlist $j
	}

	return $rlist	
}



