SELECT UNIQUE * FROM ns_fea
WHERE ns_siteno in
(select site_no from site_mng where
( (utm_e - 606060.579065) * (utm_e - 606060.579065) +
(utm_n - 4915838.307350) * (utm_n - 606060.579065) ) < 500000.000000 )
