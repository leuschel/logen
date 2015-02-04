#!/bin/sh
ulimit -d unlimited
bta_path=`dirname $0`
cp -r $bta_path/bta_files .
#bad hack as webserver wasnt finding file : I guess www has wrong path
sicstus="/Users/leuschel/bin/bin/sicstus"
if [ -e /usr/bin/sicstus ] 
then
sicstus="/usr/bin/sicstus"
fi
if [ -e /usr/local/bin/sicstus ] 
then
sicstus="/usr/local/bin/sicstus"
fi
$sicstus -r $bta_path/bta.sav --goal 'bta.'
