
#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include <datetime.h>

$include sqltypes;

#include "globals.h"

main()
{
  $char *last_name;
  $struct {
    int customer_num;
    char fname[15];
    char lname[15];
    char company[20];
    char address1[20];
    char address2[20];
    char city[15];
    char state[2];
    char zipcode[5];
    char phone[18];
  } cust_rec;

  $database stores;
  $declare q_curs cursor for select * from customer where lname matches
    $last_name for update;
  $open q_curs;
  $begin work;
  last_name = "Pauli";
  $fetch q_curs into $cust_rec;
  $update customer where current of q_curs;
  $commit work;
  $close database;
}
