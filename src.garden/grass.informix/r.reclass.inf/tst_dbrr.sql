{ test select file for r.reclass.db -s, database=seco_soils, GISdb=SECO }
        { curly brackets are Informix comment delimiters }


select grass_cat from seco_soil_cats,layer where layer.tfact >0 and layer.tfact <=2 and
layer.muid=seco_soil_cats.muid ;

select grass_cat from seco_soil_cats,layer where layer.tfact>2 and layer.tfact<=4 
and layer.muid=seco_soil_cats.muid ;

select grass_cat from seco_soil_cats,layer where layer.tfact=5 and layer.muid=seco_soil_cats.muid 

{
d.rast.db key=grass_cat input=seco.soils output=seco.soils.recl tab=seco_soil_cats \
col=cropname join=compyld,compyld.muid,seco_soil_cats.muid
}


