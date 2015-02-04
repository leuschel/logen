#!/bin/sh

# This script exports the logen trunk without version control information
# and creates a compressed tar archive of it. The main directory has the name
# logen or logen-VERSION if a file called VERSION exists (under VC) in the
# trunk main directory.

# You can make a tarball of a specific revision by giving that revision as the
# sole argument to create_dist.sh

if [ x$1 == x--help ];
then
	echo "./create_dist [revision]";
	exit 0
fi

if [ x$1 == x--local ];
then
	shift
	REPO=.
	echo "Using local working copy.";
else
	REPO=svn+ssh://stupslog.cs.uni-duesseldorf.de/home/mal/svnrootfsfs/logen/trunk
	echo "Using remote repository.";
fi

if [ x$1 != x ];
then
	REV="-r $1"
else
	REV=
fi

DIR=`mktemp -d`

echo -n "Retrieving version information... "

VERSION=`svn cat $REV $REPO/VERSION 2>/dev/null`

echo "done"

if [ x$VERSION != x ];
then
	echo -n "Exporting logen (version $VERSION)... "
	VERSION="-$VERSION"
else
	echo -n "Exporting logen (no version number)... "
fi

svn export -q $REV $REPO $DIR/logen$VERSION

echo "done"
LASTDIR=`pwd`
cd $DIR
# remove files that give details of svn repo and are unneeded for external use
rm logen$VERSION/weblogen/upload.sh logen$VERSION/create_dist.sh
echo -n "Making tarball $LASTDIR/logen$VERSION.tgz ... "
tar czf $LASTDIR/logen$VERSION.tgz logen$VERSION
echo "done"
echo -n "Removing export directory... "
cd $LASTDIR
rm -rf $DIR
echo "done"
echo

