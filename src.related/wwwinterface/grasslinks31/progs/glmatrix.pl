#!/moby/misc/bin/perl -w
###############################################
# GRASSLinks Helper program
# Creates HTML table of files used in GRASSLinks
# to show gifs & metadata if available
# Should editted for local use
# Use at your own risk! 
###############################################


$GLData_list = $ARGV[0];
$outfile = "GL_matrix.html"; # will overwrite existing file with this name
$gif_dir = "tnails";
$md_dir = "mdata";

if (! $GLData_list ) {
	die "USAGE: glmatrix.pl <gl_datalist_file>\n";
}	


&get_fileinfo;
&make_glmatrix;

exit;	

#-------------------------------------------------------------------------
# subroutines
#-------------------------------------------------------------------------

sub get_fileinfo {
        open(LIST, "$GLData_list") || die "Error: unable to open GL data list file: $GLData_list ($!)\n";

	@thefile = <LIST>;
	close(LIST);

        foreach $line (@thefile) {
		chop $line;
		@data = split(/\|/,$line);
		for ($i = 0; $i < 7; $i++) {
                        $data[$i] = "  " unless defined $data[$i];
                }

		next if $data[2] =~ "none";

		if ($data[0] eq "raster") {
			$rec = {};
			$rec->{projects} = $data[1];
			$rec->{filename} = $data[2];
			($fbase, $junk) = split(/\@/,$rec->{filename});
			$rec->{desc} = $data[3];
			$rec->{mdata} = $data[4];
			#$rec->{gif} = $data[5];
			$rec->{gif} = "$fbase.gif";
			$rec->{note} = $data[6] ;
			#whole thing in HoH by file description
			$RASTERS{ $rec->{desc} } = $rec;
                } 

		if ($data[0] eq "vector") {
			$rec = {};
			$rec->{projects} = $data[1];
			$rec->{filename} = $data[2];
			$rec->{desc} = $data[3];
			$rec->{mdata} = $data[4];
			$rec->{gif} = $data[5];
			$rec->{note} = $data[6];
			#whole thing in HoH by file description 
			#$VECTORS{ $rec->{filename} } = $rec;
			$VECTORS{ $rec->{desc} } = $rec;
                } 

		if ($data[0] eq "sites") {
			$rec = {};
			$rec->{projects} = $data[1];
			$rec->{filename} = $data[2];
			$rec->{desc} = $data[3];
			$rec->{mdata} = $data[4];
			$rec->{gif} = $data[5];
			$rec->{note} = $data[6];
			#whole thing in HoH by file description
			#$SITES{ $rec->{filename} } = $rec;
			$SITES{ $rec->{desc} } = $rec;
                } # end if

        }# end foreach 

}


sub make_glmatrix {

	open (OUT, ">$outfile") || die "Error: unable to open output file $outfile: $!\n";	
	print OUT << "ENDPRINT";
	<html>
	<head>
	<title>GRASSLinks GRASS File Listing</title>
	</head>
	<body bgcolor="#efefeg">
	
	<h2>GRASSLinks 3.1 at REGIS</h2>
	<p>
	<b>
	<font=size+3>GRASS Data Files available via GRASSLinks at this Site:</font>
	</b>
	<hr>
	<p>
	<table border=1 cellspacing=4 cellpadding=4>
	<tr bgcolor="#CCCC66">
	<td><b>RASTER FILES: Name / Description:</b><br>
	<sub><i>Click filename to view thumbnail</i></sub></td>
	<sub>
	<td><b>Metadata Available?</b></td>
	<td><b>GRASSLinks Project(s):</b></td>
	</sub>
	</tr>
ENDPRINT
	
	foreach $rast (sort keys %RASTERS) {
		my $rec = $RASTERS{$rast};
		print OUT "<tr>\n";
		if ( $rec->{gif} eq "  " ) {
			print OUT "<td><b>$rec->{desc}</b><br>$rec->{note}</td> \n";
		} else {
			print OUT "<td><b><a href=\"$gif_dir/$rec->{gif}\"> \
			 $rec->{desc}</a></b><br>$rec->{note}</td> \n";
		}
		if ( $rec->{mdata} eq "  " ) {
			print OUT "<td align=center>N</td> \n";
		} else {
			print OUT "<td align=center><a href=\"$md_dir/$rec->{mdata}\">Y</a></td> \n";
		}
		print OUT "<td align=center><b>$rec->{projects}</b></td> \n";
		print OUT "</tr> \n";
	}

	print OUT << "ENDPRINT";
	<tr bgcolor="#CCCC66">
	<td><b>VECTOR FILES: Name / Description</b><br>
	<sub><i>Click filename to view thumbnail</i></sub></td>
	<td><b>Metadata Available?</b></td>
	<td><b>GRASSLinks Project(s):</b></td>
	</tr>
ENDPRINT

	foreach $vect (sort keys %VECTORS) {
		my $rec = $VECTORS{$vect};
		print OUT "<tr>\n";
		if ( $rec->{gif} eq "  " ) {
			print OUT "<td><b>$rec->{desc}</b><br>$rec->{note}</td> \n";
		} else {
			print OUT "<td><b><a href=\"$gif_dir/$rec->{gif}\"> $rec->{desc}</a></b><br>$rec->{note}</td> \n";
		}
		if ( $rec->{mdata} eq "  " ) {
			print OUT "<td align=center>N</td> \n";
		} else {
			print OUT "<td align=center><a href=\"$md_dir/$rec->{mdata}\">Y</a></td> \n";
		}
		print OUT "<td align=center><b>$rec->{projects}<b></td> \n";
		print OUT "</tr> \n";
	}

	print OUT << "ENDPRINT";
	</tr>
	<tr bgcolor="#CCCC66">
	<td><b>SITES FILES: Name / Description</b><br>
	<sub><i>Click filename to view thumbnail</i></sub></td>
	<td><b>Metadata Available?</b></td>
	<td><b>GRASSLinks Project(s):</b></td>
	</tr>
ENDPRINT

	foreach $site (sort keys %SITES) {
		my $rec = $SITES{$site};
		print OUT "<tr>\n";
		if ( $rec->{gif} eq "  " ) {
			print OUT "<td><b>$rec->{desc}</b><br>$rec->{note}</td> \n";
		} else {
			print OUT "<td><b><a href=\"$gif_dir/$rec->{gif}\"> $rec->{desc}</a></b><br>$rec->{note}</td> \n";
		}
		if ( $rec->{mdata} eq "  " ) {
			print OUT "<td align=center>N</td> \n";
		} else {
			print OUT "<td align=center><a href=\"$md_dir/$rec->{mdata}\">Y</a></td> \n";
		}
		print OUT "<td align=center><b>$rec->{projects}</b></td> \n";
		print OUT "</tr> \n";
	}

	print OUT << "ENDPRINT";

	</table>
	<p>
	<h3>Please Note:</h3>

        <li>GRASS files are grouped and made available via GRASSLinks by the
        projects to which they relate and/or for which they were created. To
        view a Particular GRASS file in GRASSLinks, make a note of the project(s)
        with which the file is associated and select that project when you
        begin GRASSLinks. Below is a list of current GRASSLinks projects.
        <p>
        <b>GRASSLinks Projects by Project Code:</b>
	<ul>
	<li><b>b</b>: San Francisco BCDC North Bay Wetlands Protection Plan 
	<li><b>d</b>: Sacramento - San Joaquin Delta Area 
	<li><b>g</b>: San Francisco Bay Demonstration GIS Project 
	<li><b>r</b>: San Francisco Bay Area and Delta Regions 
	<li><b>s</b>: North Coast Salmon Initiative (CERES / WITS) 
	<li><b>t</b>: Twitchell Island Region 
	</ul>
	<p>
	<hr>
	<p>
	<b>Disclaimer:</b> Use of GRASSLinks and the data accessed by it is
	provided as-is without any guarantees of quality, completeness, currentness,
	or performance. For more information, see the
	<a href="gl_copyright.html"> GRASSLinks (c) Copyright and Disclaimer</a>.
	<p>
	<hr>
	<p>
	
	</body>
	</html>
ENDPRINT

	close OUT;

}

