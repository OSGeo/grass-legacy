#define CHKTYP "SELECT coltype FROM syscolumns WHERE colname="
#define MIN_MAX "SELECT MIN(%s), MAX(%s) FROM %s "
#define MIN_MAX_ALL "SELECT COUNT (DISTINCT %s),MIN(%s), MAX(%s) FROM %s "
