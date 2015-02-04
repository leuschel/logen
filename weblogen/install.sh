#!/bin/sh

set -e

function question()
{
	QUESTION=blah
	until [ \( x$QUESTION = x$2 \) -o \( x$QUESTION = x$3 \) ];
	do
		echo -n "$1 "
		read QUESTION
		QUESTION=`echo $QUESTION | sed 's/^ *\(.\).*/\1/' | tr [a-z] [A-Z]`
	done

	test x$QUESTION == x$2
}

function check_bta()
{
	if ! type -p sicstus > /dev/null;
	then
		echo
		echo "*********************************************************"
		echo "Sicstus is not installed. Auto BTA will not be installed."
		echo "*********************************************************"
		echo
		ENABLE_BTA=false
		return
	fi

	ENABLE_BTA=true
}

function set_logen_dir()
{
	echo
	echo "In order to precompile the prolog files, you need to enter the path"
	echo "to the logen directory."

	if [ x$logen_directory != x ]; then
		echo
		echo "logen_directory is set to $logen_directory"
		if question "Is this the right directory (Y/N) ?" "Y" "N"; then
			return 0
		else
			unset logen_directory
		fi
	fi

	cd $INSTALL_DIR
	if [ -d ../../logen ]; then
		cd ..
		DEFAULT_LOGEN_DIR=`/bin/pwd`
		echo
		echo "A directory logen appears at $DEFAULT_LOGEN_DIR"
		if question "Is this the right directory (Y/N) ?" "Y" "N"; then
			export logen_directory=$DEFAULT_LOGEN_DIR
			return 0
		fi
	fi

	while [ x$logen_directory != x ];
	do
		echo
		echo "Please enter the path of the logen directory"
		echo "E.g. /home/joeblogs/logen"
		read logen_directory
		export logen_directory
	done

	return 0
}

echo WebLogen Install
echo "~~~~~~~~~~~~~~~~"
echo
echo "Front-end: This contains all the files that are accessible via the"
echo "web. As such they must be installed into a directory that is exported via"
echo "your webserver."
echo
echo "Please enter the directory into which you want to install the front-end."
echo "In this directory a directory or symbolic link called weblogen will be"
echo "installed."

OS=`uname`

if [ x$OS == xDarwin ]; then
	DEFAULT_FRONTEND_DIR=~/Sites
	email="webmaster@"`hostname`;
else
	DEFAULT_FRONTEND_DIR=~/public_html
	email="webmaster@"`hostname -f`;
fi

echo "(If you leave this blank it will default to $DEFAULT_FRONTEND_DIR)"
read FRONTEND_DIR

if [ x$FRONTEND_DIR == x ]; then
	FRONTEND_DIR=$DEFAULT_FRONTEND_DIR
fi

echo

echo -n "Creating $FRONTEND_DIR ... "
if ! mkdir -p $FRONTEND_DIR; then
	echo "Failed to create front-end directory!"
	echo "Please check the path is valid and that you have the right permissions."
	exit 1
fi

echo "done"
echo
echo "Now you need to choose whether to copy the front-end into this directory"
echo "or to create a symbolic link. The first option is best for production"
echo "systems, while the latter works best when the front-end directory is"
echo "being updated from a version control system. Note that the second option"
echo "may not work if your web server is configured not to follow symbolic"
echo "links."
echo 

TARGET=$FRONTEND_DIR/weblogen

if question "(C)opy files or create a (S)ymbolic link? " "S" "C"; then
	echo "Creating symbolic link to frontend"
	# If the target is already a symbolic link then overwrite it
	# otherwise let the user know.
	if [ -L $TARGET ]; then
		rm $TARGET
		ln -s `/bin/pwd`/public_html $TARGET
	elif [ -d $TARGET ]; then
		echo "$TARGET already exists as a directory."
		echo "Please delete it or choose to copy the front-end."
		exit 1
	else
		ln -s `/bin/pwd`/public_html $TARGET
	fi
else
	echo -n "Copying frontend ... "
	mkdir $TARGET
	cp -r public_html/* $TARGET
	echo "done"
fi

BACKEND_DIR=`/bin/pwd`/backend
echo "(Backend dir is $BACKEND_DIR)"

INSTALL_DIR=`/bin/pwd`
cd $BACKEND_DIR
#echo `/bin/pwd` > $FRONTEND_DIR/weblogen/backend_directory

chmod o+r $TARGET/*
chmod o+rx $TARGET
chmod o+rx $FRONTEND_DIR
chmod o+r $BACKEND_DIR/*
chmod o+x $BACKEND_DIR/*.sh


set_logen_dir
check_bta

echo "# Running install.sh will overwrite this file so don't make changes here" > $BACKEND_DIR/Makefile.inc
echo >> $BACKEND_DIR/Makefile.inc
echo "ENABLE_BTA=$ENABLE_BTA" >> $BACKEND_DIR/Makefile.inc
	
if ! type -p swipl > /dev/null;
then
	if ! type -p pl > /dev/null;
	then
		echo "SWI Prolog is required for the syntax highlighting in weblogen";
		exit 1;
	else
		echo "SWIPL=pl" >> $BACKEND_DIR/Makefile.inc
	fi
else
	echo "SWIPL=swipl" >> $BACKEND_DIR/Makefile.inc
fi

cd $BACKEND_DIR

# Get and install pillow 

if ! [ -d pillow ];
then
	if type -p wget > /dev/null;
	then
		echo "Getting pillow from http://clip.dia.fi.upm.es/~clip/Software/pillow/pillow1.1.swi.tar.gz";
		wget http://clip.dia.fi.upm.es/~clip/Software/pillow/pillow1.1.swi.tar.gz
		tar xzf pillow1.1.swi.tar.gz
		rm -rf pillow
		mv pillow1.1.swi pillow
	else
		echo "Pillow is not present and wget is not installed, so it cannot be"
		echo "automatically retrieved."
		echo
		echo "Please download pillow from:"
		echo "http://clip.dia.fi.upm.es/~clip/Software/pillow/pillow1.1.swi.tar.gz"
		echo "uncompress it, rename the resulting directory pillow and move it"
		echo "into the backend directory."
		exit 1;
		
	fi
else
	echo "Pillow is present";
fi

echo "Building backend"
echo
make

ciao_dir=`type -p ciaoc`
ciao_dir=`dirname $ciao_dir`
config=$TARGET/config.php
if ! [ -e $config ];
then
	echo "Creating $config";
	echo "<?php" > $config
	echo >> $config
	echo '# $admin_email is the email address that appears at the bottom of all pages' >> $config
	echo "\$admin_email = '$email';" >> $config;
	echo >> $config
	echo '# this points to the directory containing backend stuff for weblogen' >> $config
	echo "\$backend_dir = '$BACKEND_DIR';" >> $config
	echo >> $config
	echo '# this points to the directory containing the cogen executable' >> $config
	echo "\$logen_dir = '$logen_directory';" >> $config
	echo >> $config
	echo '# this points to the directory containing the ciaoc executable' >> $config
	echo "\$ciao_dir = '$ciao_dir';" >> $config
	echo >> $config
	echo '# this declares whether the autobta is enabled' >> $config
	echo "\$enable_bta = $ENABLE_BTA;" >> $config
	echo >> $config
	echo '# Uncomment the following line to disable safe mode' >> $config
	echo "# \$safe = false;" >> $config
	echo >> $config
	echo '# The following line controls whether watchdog mode is enabled by default' >> $config
	echo "\$default_watchdog = false;" >> $config
	echo >> $config
	echo '# The following lines control various timeouts:' >> $config
	echo '# This is the default timeout' >> $config
	echo '$maxtime = 10;' >> $config
	echo '$timeouts = Array();' >> $config
	echo '# By uncommenting out the following lines you override the default timeout' >> $config
	echo '# for a specific action' >> $config
	echo '# Making the timeouts longer than the apache/PHP timeout is pointless!' >> $config
	echo "# \$timeouts['specialise'] = 10;" >> $config
	echo "# \$timeouts['show_annotated'] = 10;" >> $config
	echo "# \$timeouts['gx'] = 10;" >> $config
	echo "# \$timeouts['highlight'] = 10;" >> $config
	echo "# \$timeouts['auto_bta'] = 10;" >> $config
	echo "# \$timeouts['simple_bta'] = 10;" >> $config
	echo "# \$timeouts['modify_anns'] = 10;" >> $config
	echo >> $config
	echo '# These next two lines control how respecialisation occurs using ecce and asap' >> $config
	echo '# comment out either line to remove the appropriate button from the interface.' >> $config
	echo "\$ecce_url = 'http://www.stups.uni-duesseldorf.de/~pe/ecce/index.php';" >> $config
	echo "\$asap_url = 'http://www.stups.uni-duesseldorf.de/~asap/asap-online-demo/index.php';" >> $config
	echo >> $config;
	echo '# uncomment the following lines and set the values appropriately, if you want' >> $config
	echo '# bug submitting to be enabled. This requires a database to be created.' >> $config
	echo '# To do this run ../backend/create_db.sh, although you will have to set up the' >> $config
	echo '# account and privileges yourself.' >> $config
	echo "#\$db_name = 'weblogen';" >> $config;
	echo "#\$db_username = 'weblogen';" >> $config;
	echo "#\$db_password = 'password';" >> $config;
	echo >> $config;
	echo "# Uncomment this line to have errors printed to the screen instead of the" >> $config;
	echo "# webserver error log" >> $config;
	echo "# ini_set('display_errors', true);" >> $config;
	echo '?>' >> $config
else
	echo "$config already exists."
	echo "Delete it to regenerate.";
fi

echo
echo "Installation complete"
echo 
echo "Now you need to view the setup page. This will confirm that you have"
echo "PHP configured properly and have everything installed properly."
echo "The URL will be for setup.php in the directory in which you installed"
echo "weblogen."
echo
echo "e.g. http://localhost/~joeblogs/weblogen/setup.php"
echo
echo "Obviously you must have a webserver and PHP installed to make this work."
echo "PHP 5.0 is required."
