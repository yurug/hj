#!/usr/bin/env bash

# Stop on first error
set -e

# Realpathing the script.
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd`
popd > /dev/null
topdir=$SCRIPTPATH/..

if [ x$1 != x"fast" ]; then

###################################
# Compilation of the hackojo-core #
###################################
make -C $topdir/dojo/core
make -C $topdir/dojo/core uninstall
make -C $topdir/dojo/core install

#############################
# Compilation of the webapp #
#############################
cp $topdir/etc/hackojo.conf.local $topdir/dojo/app/hackojo.conf.in
make -C $topdir/dojo/app clean
make -C $topdir/dojo/app

fi

#########################################
# Web app deployment (you must be root) #
#########################################
sudo mkdir -p /var/upload
sudo chown www-data:www-data /var/upload

# The following command will ask for you to choose
# an administration password.
sudo \
  PATH=$PATH \
  OCAMLPATH=$OCAMLPATH \
  CAML_LD_LIBRARY_PATH=$CAML_LD_LIBRARY_PATH \
  LD_LIBRARY_PATH=$LD_LIBRARY_PATH \
  make -C $topdir/dojo/app install run.opt

