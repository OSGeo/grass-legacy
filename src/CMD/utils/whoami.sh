cd /tmp
rm -rf $$
> $$
ls -l $$ | awk '{print $3}'
rm -f $$
