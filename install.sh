#!/bin/sh

if ! type -p make > /dev/null;
then
	echo "make is required to build logen";
	exit 1;
fi

if ! type -p ciaoc > /dev/null;
then
	echo "ciaoc is required to build logen";
	exit 1;
fi

echo "Building logen";
make

if [ $? != 0 ];
then
	echo "Failed to build logen. Please check error messages."
	exit 1;
fi

echo "Building weblogen";
cd weblogen
./install.sh
