select unique tract_blck,pop100 from stf1_main 
where pop100 <1000;
select unique tract_blck,pop100 from stf1_main 
where pop100 >1000 and pop100 <1500;
select unique tract_blck,pop100 from stf1_main 
where pop100 >1500 and pop100 <2000;
select unique tract_blck,pop100 from stf1_main 
where pop100 >2000 and pop100 <2500
