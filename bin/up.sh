#!/usr/bin/env bash

# Stop on first error
set -e

# Realpathing the script.
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd`
popd > /dev/null
topdir=$SCRIPTPATH/..

# The hackojo command-line client
# (clone it from https://github.com/yurug/hjc)
HJC=hjc

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
make -C $topdir/dojo/app

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

#########
# Setup #
#########

# Adjust this part of the script to your needs.

# Login as an administrator.
$(HJC) login --dojo https://localhost --username admin

# Set the directory where the hackojo state is stored.
$(HJC) chroot /var/data/hackojo

####################
# User information #
####################

# Setup a shell script for the Dojo to get user information.
# Adjust to your local environment.

$(HJC) get_user_info "
  case %what in
  exists) echo 1;;
  status) echo teacher;;
  esac
"

####################
# Virtual machines #
####################

# You must setup at least ONE virtual machine.

$(HJC) machinist_create debian
$(HJC) machinist_upload debian key1 $topdir/etc/id_rsa
$(HJC) machinist_set_logins debian test key1
$(HJC) machinist_set_addresses debian 127.0.0.1 22

########################
# Aka standard library #
########################

# Setup standard library for the AKA language.
$(HJC) exercise_create std
$(HJC) exercise_focus std
$(HJC) exercise_push $topdir/lib/std.aka
