echo ""
if [ $# != 1 ]
then
	echo Usage: $0 program.name
	exit
fi
result=`grep $1\$ << EOF
d.3d               D3d
d.3d               d.3d
3d.view.sh         D3d.view
d.frame            Dclear.screen
d.colormode        Dcolormode
d.colors           d.colors
d.colortable       Dcolortable
i.composite        Dcomposite
dcorrelate.sh      Dcorr
d.display          display
d.erase            Derase
d.font             Dfont
d.frame            Dchoose
d.frame            Dnew
d.frame            Dremove
d.frame            Dstatus
d.frame            Dwhich
d.geodesic         Dgeodesic
d.graph            Dgraph
grass.logo.sh       Dgrass.logo
d.grid             Dgrid
d.his              Ghis
d.histogram        Dhistogram
d.icons
d.label            Dlabel
d.legend           Dlegend
d.mapgraph         Dmapgraph
d.measure          Dmeasure
d.menu             Dmenu
d.mon              Dlist.mon
d.mon              Drelease.mon
d.mon              Dselect.mon
d.mon              Dstart.mon
d.mon              Dstatus.mon
d.mon              Dstop.mon
d.mon              Dwhich.mon
not-available      Dmove
d.paint.labels     Dpaint.labels
p.ppm              paint.ppm
d.points           Dpoints
d.profile          profile
d.rast             Dcell
d.what.rast        Dwhat
d.what.rast        Dwhat.once
d.frame            d.window
d.rgb              Grgb
d.rhumbline        Drhumbline
d.save             Dsave
d.scale            Dscale
d.scale            Dscale2
d.frame            Dscreen
show.colors.sh     Dshow.colors
show.fonts.sh      Dshow.fonts
d.sites            d.sites
slide.show.sh      Dslide.show
d.text             Dtext
d.title            Dtitle
d.vect             Dvect
d.what.vect        Dvhat
d.where            Dwhere
d.zoom
g.access           access
g.access           access
g.ask              Gask
g.copy             Gcopy
g.copy             copy
not-available      Gcount.file
g.demo-unavailable demo
m.examine.tape     examine.tape
g.filename         Gfilename
g.findfile         Gfindfile
g.gisenv           gisenv
g.help             help
g.list             Glist
g.list             list
g.manual           manual
g.mapsets          Gmapsets
g.mapsets          mapsets
g.region           Gwindow
g.region           window
g.remove           Gremove
g.remove           remove
g.rename           Grename
g.rename           rename
g.version          version
i.build.blk        i.build.block
i.class            i.class
i.cluster          i.cluster
i.colors           i.colors
i.composite        i.composite
i.grey.scale       i.grey.scale
i.group            i.group
i.maxlik           i.maxlik
i.median           i.median
i.mod.camera       i.mod.camera
i.points           i.points
i.rectify          i.rectify
i.rectify.blk      i.rectify.block
d.rgb              i.rgb
i.tape.mss         i.tape.mss
i.tape.mss.h       i.tape.mss.h
i.tape.other       i.tape.other
i.tape.tm          i.tape.tm
i.target           i.target
m.datum.shift      Mdshift
m.dem.examine      Mdem.examine
m.dem.extract      Mdem.extract
m.dmaUSGSread      MdmaUSGSread
m.dted.examine     Mdted.examine
m.dted.extract     Mdted.extract
m.flip             Mflip
m.gc2ll            Mgc2ll
m.ll2gc            Mll2gc
m.ll2utm           Mll2u
not-available      Mlulc.USGS
not-available      Mlulc.read
m.rot90            Mrot90
m.utm.ll           Mu2ll
m.window.ll        Mwindow.ll
p.map
p.map              Pmap
p.ppm
p.screen           Pscreen
p.select
p.select           Pselect
p.map              paint
r.average
r.average          Gaverage
r.basins.fill      basins.fill
r.binfer           Gbinfer
r.buffer           Gdistance
r.buffer           distance
r.cats             Gcats
r.clump            Gclump
r.clump            clump
r.coin             coin
r.colors           Gcell.colors
r.combine          combine
r.compress         compress
r.compress         decompress
not-available      Gcorr
r.cost             Gcost
r.covar            Gcovar
r.cross            Gcross
r.describe         Gdescribe
r.describe         describe
r.drain            Gdrain
i.fft
r.grow             Ggrow
r.grow             grow
hsv.rgb.sh         hsv.to.rgb
i.ifft
r.in.ascii         Mimportcell
r.in.ll            Mimport.ll
r.in.sunrast
r.infer            Ginfer
r.info             Glayer.info
r.info             layer.info
r.line             Gline
r.los              Glos
r.mapcalc          Gmapcalc
not-available      mapmask
r.mask             mask
r.mfilter          Gmfilter
r.neighbors        neighbors
r.out.ascii        Gdumpcell
not-available      cell.to.dat
r.out.tga
not-available      Gpat.place
r.patch            Gpatch
r.patch            patch
r.poly             Gpoly
r.random           Grandom
r.random           random
r.reclass          Greclass
r.reclass          reclass
r.report           Greport
r.report           report
r.resample         Gresample
r.resample         resample
r.rescale          Grescale
r.rescale          rescale
rgb.hsv.sh         rgb.to.hsv
r.rgb.to.his
r.slope.aspect     Gslope.aspect
r.slope.aspect     slope.aspect
r.stats            Gstats
r.stats            Gstats.misc
r.stats            Gstats.old
r.stats            cell.stats
r.support          support
r.surf.contour     Gcontour.surf
r.surf.idw         Gpoints.surf
r.surf.idw2        Gpoints.surf2
r.surf.idw         Gsurface
r.thin             Gthin
r.traj             Gtraj
r.volume           Gvolume
r.watershed        watershed
r.weight           Gweight
r.weight           weight
r.what             Gwhat
i.zc
s.db.rim           Gdbsites
s.db.rim           dbsites
s.in.ascii
s.out.ascii  	   Gsites
s.menu             sites
v.area
v.cadlabel         Vcadlabel
v.clean         
v.db.rim           Gdbvect
v.digit            digit
v.import           import.to.vect
v.in.arc           Garc.to.digit
v.in.ascii      
v.in.ascii         a.b.dlg
v.in.ascii         a.b.vect
v.in.dxf           dxf2dig
v.mkgrid        
v.mkgrid           V.mkgrid
v.mkquads          V.mkquads
v.out.arc          Garc.to.digit
v.out.ascii     
v.out.ascii        b.a.dlg
v.out.ascii        b.a.vect
v.out.dlg          Vexport.dlg
v.out.dlg          export.dlg
not-available      mkdspf
v.out.dxf          dig2dxf
v.out.moss         vect.to.moss
v.patch            Vpatch
v.prune         
v.prune            Vprune
v.spag             spag
not-available      Vstate
v.stats         
v.stats            Vstat
v.support          support.vect
v.to.rast       
v.to.rast          vect.to.cell
v.to.sites      
v.to.sites         make.sites
v.transform        v.transform
EOF
`
echo -n "$1 replaced with: "
if [ x"$result" = x ]
then
	echo nothing
else
	echo $result | awk '{print $1}'
fi
