E.1	NCSA HDF Calling Interfaces and Utilities

Routine Lists	E.1

National Center for Supercomputing Applications

July 1990

                                                                

July 1990




Appendix  E	Routine Lists



Raster Image Routines

DFR8addimage	d8aimg	appends the RIS8 for the image to the 
		file.

DFR8getdims	d8gdims	retrieves the dimensions of the image 
		and indicates whether a palette is 
		associated and stored with the image.

DFR8getimage	d8gimg	retrieves the image and any associated 
		palette, and stores them in arrays.

DFR8putimage	d8pimg	writes out the RIS8 for the image as the 
		first image in the file.

DFR8restart	d8first	sets the next get command to read 
		from the first RIS8 in the file, rather 
		than the next.

DFR8setpalette	d8spal	sets the default palette to be used for 
		subsequent images.

DF24addimage	d2iaimg	appends the RIS24 for the image to the 
		file.

DF24getdims	d2igdims	retrieves the dimensions and interlace 
		of the image.

DF24getimage	d2igimg	retrieves the image and stores it in an 
		array.

DF24reqil	d2reqil	specifies an interlace to be used in place 
		of the interlace indicated in the file 
		when the next raster image is read.

DF24setil	d2setil	sets the interlace to be used when 
		writing out the RIS24 for the image.


Palette Routines

DFPaddpal	dpapal	appends a palette to a file.

DFPgetpal	dpgpal	reads in the next palette in the file.

DFPlastref	dplref	returns the value of the reference 
		number most recently read or written.

DFPnpals	dpnpals	indicates number of palettes in a file.

DFPputpal	dpppal	writes a palette to a file.

DFPreadref	dprref	sets the reference number of the next 
		palette to be retrieved.

DFPrestart	dprest	specifies that the next call to DFPgetpal 
		reads the first palette in the file, rather 
		than the next.

DFPwriteref	dpwref	sets the reference number of the next 
		palette to be written.


Annotation Routines

DFANgetdesc	dagdesc	gets description of tag/ref.

DFANgetdesclen	dagdlen 	gets length of description of tag/ref.

DFANgetlabel	daglab	gets label of tag/ref..

DFANgetlablen	dagllen	gets length of label of tag/ref.

DFANlablist	dallist	gets list of labels for a particular tag.

DFANlastref		returns ref of last annotation read or 
		written.

DFANputdesc	dapdesc	puts description of tag/ref.

DFANputlabel	daplab	puts label of tag/ref.


Scientific Dataset Routines

DFSDadddata	dsadata	appends the data to the file, not 	
		overwriting other file contents.

DFSDclear	dsclear	clears all possible set values.

DFSDendslice	dseslc	indicates write completion for part of a 
		dataset.

DFSDgetdata	dsgdata	reads the next dataset in the file.

DFSDgetdatastrs	dsgdast	reads the label, unit, and format  
		specification for the data.

DFSDgetdims	dsgdims	gets the number of dimensions of the 
		dataset and the sizes of the dimensions 
		for the next SDS in the file.

DFSDgetdimscale	dsgdisc	reads the scale for a dimension.

DFSDgetdimstrs	dsgdist	reads the label, unit, and format for a 
		dimension and its scale.

DFSDgetmaxmin	dsgmaxm	reads the maximum and minimum 
		values.

DFSDgetslice	dsgslc	reads part of a dataset.

DFSDputdata	dspdata	writes the data to the file, overwriting 
		other file contents.

DFSDputslice	dspslc	writes part of a dataset to a file.

DFSDrestart	dsfirst	sets the next get command to read 
		from the first SDS in the file, rather 
		than the next.

DFSDsetdatastrs	dissdast	sets label, unit, and format 	
		specifications for the data.

DFSDsetdims	dssdims	sets the default rank and dimension 
		sizes for succeeding files.

DFSDsetdimscale	dssdisc	sets the scale for a dimension.

DFSDsetdimstrs	dssdist	sets label, unit,  and format 	
		specifications for a dimension and its 
		scale.

DFSDsetlengths	dsslens	sets maximum lengths for strings that 
		will hold labels, units, formats, and the 
		name of the coordinate system.

DFSDsetmaxmin	dssmaxm	sets maximum and minimum data 
		values.

DFSDsettype	dsstype	specifies data attributes—data type and 
		representation, system type, and array 
		order.

DFSDstartslice	dssslc	prepares system to write part of a 
		dataset to a file.


General Purpose Routines

DFopen		provides an access path to the file 
		named in filename with the access 
		given in access. 

DFclose		updates the DD blocks, then closes the 
		access path to the file referred to by 
		dfile.

DFsetfind	dfsfind	initializes searches for elements using 
		tags or reference numbers.

DFfind	dffind	locates the data descriptor needed for 
		the next read from the file.

DFgetelement	dfget	extracts the data referred to by the 
		tag/ref  and places the data in the array 
		storage.

DFputelement	dfput	 adds or replaces elements in dfile.

DFaccess		inititiates a read or write on the data 
		element with the specified tag/ref 
		combination. 

DFread		reads a portion of a data element.

DFwrite		appends data to a data element.

DFseek		sets the read pointer to an offset within 
		a data element.

DFupdate		writes out the DD blocks necessary to 
		update the file.

DFdup		generates a DD whose offset and length 
		are the same as those of another tag/ref.

DFdel		deletes a tag/ref From the list of DDs.

DFerrno		reports the value of DFerror.

DFishdf		tells if a file is an HDF file

DFnewref		generates an unused reference number

DFstat		provides status information about an 
		HDF file.
Utility Routines

hdfls	displays the tags, ref numbers, and (optionally) lengths 
	of data elements.

hdfed	lets you browse in an HDF file and manipulate some of 
	the data.

fptohdf	converts floating-point data to HDF floating point data 
	and/or 8-bit raster images.

r8tohdf	converts one or more raw 8-bit images to HDF RIS8 
	format and writes them to a file, possibly with palettes.

hdftor8	converts images and or palettes from HDF format to 
	raw format and stores them in two corresponding sets 
	of files.

r24tohdf	converts a raw RGB 24-bit image to an 8-bit RIS8 with 
	a palette.

paltohdf	converts a raw palette to hdf format.

hdftopal	converts palette in an hdf file to raw format.

hdfseq/hdfrseq	displays sequences of images directly to the screen 
	from HDF files containing raster images.


