#!/bin/sh

TEMPFILE=`mktemp /tmp/tmp.XXXXXX`
./collect_anns ../public_html/annmenus.php $TEMPFILE
sh $TEMPFILE
rm $TEMPFILE
