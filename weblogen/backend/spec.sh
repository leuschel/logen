#!/bin/sh

cd $2
BACKEND=$1

echo "*** spec.sh --" `date` >> $BACKEND/log
#$BACKEND/runner /usr/local/bin/sicstus -r $BACKEND/spec.sav 2>>$BACKEND/log
/usr/local/bin/sicstus -r $BACKEND/spec.sav 2>>$BACKEND/log
echo "*** spec.sh done --" `date` >> $BACKEND/log
