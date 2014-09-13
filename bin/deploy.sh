#!/usr/bin/env bash

# Realpathing the script.
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd`
popd > /dev/null
topdir=$SCRIPTPATH/..

# Copying the configuration file.
cp $topdir/etc/hackojo.conf.local $topdir/dojo/hackojo.conf.in

# Compilation 
make -C $topdir/dojo

# Deployment (you must be root)
sudo mkdir -p /var/upload
sudo chown www-data:www-data /var/upload
sudo PATH=$PATH OCAMLPATH=$OCAMLPATH CAML_LD_LIBRARY_PATH=$CAML_LD_LIBRARY_PATH  LD_LIBRARY_PATH=$LD_LIBRARY_PATH make -C $topdir/dojo install run.opt
