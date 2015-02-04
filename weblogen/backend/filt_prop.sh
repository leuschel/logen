#!/bin/sh

BACKEND=$1
echo `/bin/pwd` >> $BACKEND/log
cp -r $1/../../logen_source/bta_files .
/usr/local/bin/sicstus -r $BACKEND/bta.sav --goal 'filt_prop.' 2>>$BACKEND/log

