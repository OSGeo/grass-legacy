N=tbl -TX man.version /home/grass3/src/man.help/utilities/man.header $? | nroff | col -b > $@
all: list
cat1/vect.to.moss: man1/vect.to.moss ;  $N
cat1/i.class: man1/i.class ;  $N
cat1/i.camera: man1/i.camera ;  $N
cat1/i.mod.camera: man1/i.mod.camera ;  $N
cat1/i.rectify.block: man1/i.rectify.block ;  $N
cat1/arc.to.grass: man1/arc.to.grass ;  $N
cat1/grass.to.arc: man1/grass.to.arc ;  $N
cat2/Dscale2: man2/Dscale2 ;  $N
cat2/Gdbvect: man2/Gdbvect ;  $N
cat2/Gpoly2: man2/Gpoly2 ;  $N
cat2/Gdbsites: man2/Gdbsites ;  $N
cat2/Dhistogram: man2/Dhistogram ;  $N
cat2/Dicons: man2/Dicons ;  $N
cat2/Gline: man2/Gline ;  $N
cat2/Gthin: man2/Gthin ;  $N
cat2/Gtrim: man2/Gtrim ;  $N
cat2/d.labels: man2/d.labels ;  $N
cat2/profile: man2/profile ;  $N
cat2/Mlulc.USGS: man2/Mlulc.USGS ;  $N
cat2/Mlulc.read: man2/Mlulc.read ;  $N
cat2/Mdem.extract2: man2/Mdem.extract2 ;  $N
cat3/i.block.reference: man3/i.block.reference ;  $N
cat3/i.block.initial: man3/i.block.initial ;  $N
cat3/i.block.rectify: man3/i.block.rectify ;  $N
cat3/i.block.control: man3/i.block.control ;  $N
list:  cat1/vect.to.moss cat1/i.class cat1/i.camera cat1/i.mod.camera cat1/i.rectify.block cat1/arc.to.grass cat1/grass.to.arc cat2/Dscale2 cat2/Gdbvect cat2/Gpoly2 cat2/Gdbsites cat2/Dhistogram cat2/Dicons cat2/Gline cat2/Gthin cat2/Gtrim cat2/d.labels cat2/profile cat2/Mlulc.USGS cat2/Mlulc.read cat2/Mdem.extract2 cat3/i.block.reference cat3/i.block.initial cat3/i.block.rectify cat3/i.block.control
