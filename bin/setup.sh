#!/usr/bin/env bash

# Realpathing the script.
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd`
popd > /dev/null
topdir=$SCRIPTPATH/..

# The hackojo command-line client
# (clone it from https://github.com/yurug/hjc)
HJC=hjc

#########
# Setup #
#########

# Adjust this part of the script to your needs.

# Login as an administrator.
echo -n 'password for administrator:'
read -s password
echo ''
$HJC login    --dojo https://localhost --username admin --password $password

####################
# User information #
####################

# Setup a shell script for the Dojo to get user information.
# Adjust to your local environment.

read -d '' SCRIPT <<EOF
case %what in
 exists) echo 1;;
 email) echo foo@bar.com;;
 status) echo teacher;;
esac
EOF

$HJC get_user_info "$SCRIPT"

# Set the directory where the hackojo state is stored.
$HJC chroot /var/data/hackojo

####################
# Virtual machines #
####################

# You must setup at least ONE virtual machine.

$HJC machinist_create debian
$HJC machinist_upload debian key1 $topdir/etc/id_rsa
$HJC machinist_set_logins debian test key1
$HJC machinist_set_addresses debian 127.0.0.1 22

########################
# Aka standard library #
########################

# Setup standard library for the AKA language.
$HJC exercise_create std
$HJC exercise_focus std
$HJC exercise_push $topdir/lib/std.aka
