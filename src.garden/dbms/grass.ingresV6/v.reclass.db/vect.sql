{select record_num from feature where cfcc matches "A1*"
select unique record_num from feature where feature.cfcc matches "A1*" 
order by record_num;
select unique record_num from feature where feature.cfcc matches "A2*" 
order by record_num;
select unique record_num from feature where feature.cfcc matches "A3*" 
order by record_num
}
select unique record_num from feature where feature.cfcc matches "H12*" 
order by record_num
