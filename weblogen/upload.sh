#!/bin/bash

echo "Connecting to stupslog:"
ssh stupslog.cs.uni-duesseldorf.de  svnserve -X & 
echo "svnserve started"
echo "Connecting to pe@www.stups"

echo "cd logen && svn up && touch *.pl && make && cd weblogen/backend && make" | ssh pe@www.stups.uni-duesseldorf.de
