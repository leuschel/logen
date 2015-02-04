#!/bin/sh

bta_path=`dirname $0`
cp -r $bta_path/bta_files .
/usr/local/bin/sicstus -r $bta_path/bta.sav --goal 'filt_prop.'
