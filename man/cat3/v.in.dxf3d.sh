


v.in.dxf3d.sh <scriptsGRASS Reference Manu<scripts> v.in.dxf3d.sh



NAME
     Iv.in.dxf3d.sh R -	Imports	contour	levels and master contour
     levels in DXF file	format to GRASS	vector file format.
     (GRASS Vector Program)

SYNOPSIS
     v.in.dxf3d.sh dxf=name lines=name,name

DESCRIPTION
     The v.in.dxf3d.sh data conversion program generates GRASS
     vector files from a DXF file with contour levels and master
     contour levels layers with	Z values.

     This shell	run successively the programs v.in.dxf,
     v.in.dxf3d	and v.support for both specified layers.



COMMAND	LINE OPTIONS
     Parameters

     dxf  Name of the DXF input	design file to be converted to
	  GRASS	vector format.

     lines
	  Name(s) of layer(s) in DXF input file	containing  the
	  contour levels and master contour levels with	Z values,
	  and the mane(s) to be	assigned to the	GRASS vector
	  files	output.


SEE ALSO
     v.in.dxf, v.in.dxf3d, v.support, v.digit


AUTHOR
     The shell was writted by Evaristo Quiroga,	Environmental and
     Territorial Analisis Center, UAB (12/95).

















GRASS 4.2		Baylor University			1



