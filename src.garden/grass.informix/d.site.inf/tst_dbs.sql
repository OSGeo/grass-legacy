{ test select file for d.site.db -s, database=seco_wells, GISdb is SECO }

SELECT well.utm_east,well.utm_north FROM well  WHERE aquif matches "g*"
AND utm_east > 0 AND utm_north > 0
