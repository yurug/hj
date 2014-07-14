# -*-autoconf-*-

# Autoconfiguration of AutomaTeX, a generic makefile for building LaTeX
# documents.
#
# Copyright 2003-2005 Christian Rinderknecht
#
# You may redistribute this file freely without any changes and with
# its `configure.ac' (modified or not).
# You may also modify and redistribute freely this file as long as this
# copyright notice remains, the name (AutomaTeX) changes and the
# original author remains in AC_INIT macro call and in the `echo'
# shell command below.

# Documentation is to be found in the companion file `Makefile.in'.

# Process this file with `autoconf' to produce a `configure' script.

# $Id: configure.ac 325 2005-04-02 06:13:30Z rinderkn $

# ------------------------------------------------------------------------
# General settings
#
AC_INIT(AutomaTeX,0.5,Christian.Rinderknecht@devinci.fr)
AC_PREREQ(2.53)
AC_CONFIG_SRCDIR

ac_version=$(autoconf -V | grep GNU | sed -n 's|.*) \(.*\)$|\1|p')

echo "Autoconfiguration of AutomaTeX,"
echo "a generic makefile for building LaTeX documents."
echo "(c) 2003-2005 Christian Rinderknecht"
echo
echo "This is autoconf (GNU Autoconf)" $ac_version

# ------------------------------------------------------------------------
# Checking for files named core, core.* and *.core.
#
# These files are deleted by the configure script (feature of
# autoconf), but core.* may be not a system core file. Hence this
# section backs up such files and prompts the user to check them.
#
core_files=$(ls core *.core core.* 2> /dev/null)
maybenot_core=$(ls core.* 2> /dev/null)

if test -n "$core_files";
then
  echo -----------------------------------------------------------------
  if test $(echo $core_files | wc --words) = 1;
  then
    echo "File $core_files is going to be deleted!"
  else
    echo "Files $core_files are going to be deleted!"
  fi
  if test -n "$maybenot_core";
  then
    echo -n "  => Backup of $maybenot_core in BACKUP-$maybenot_core..."
    cp $maybenot_core BACKUP-$maybenot_core
    echo " done."
    echo "  => Rename or delete it and rerun autoconf and configure."
  fi
fi

echo -----------------------------------------------------------------
echo System tools

AC_PATH_PROG(SHELL,bash,none)

if test "$SHELL" = "non";
then
  echo "  => You must install \`bash'!"
fi

AC_PATH_PROG(GMAKE,gmake,none)

if test "$GMAKE" = "none";
then
  AC_PATH_PROG(MAKE,make,none)
  if test "$MAKE" = "none";
  then
    echo "  => You must install \`gmake\'!"
  else
    $MAKE --version | grep GNU &> /dev/null
    if test $? != 0;
    then
      echo "  => You must install \`GNU make\'!"
    else
      $MAKE --version | grep 'GNU Make 3.80' &>/dev/null
       if test $? != 0
        then
          echo "  => You need GNU Make 3.80 or upper."
       fi
    fi
  fi
else
  MAKE=$GMAKE
fi

AC_PATH_PROG(PERL,perl,none)

if test "$PERL" = "none";
then
  echo "  => You must install \`perl\'!"
fi

AC_PATH_PROG(EXPR,expr,none)

if test "$EXPR" = "none";
then
  echo "  => You must install \`expr\'!"
fi

AC_PATH_PROG(SED,sed,none)

if test "$SED" = "none";
then
  echo "  => You must install \`sed\'!"
fi

AC_PATH_PROG(AWK,awk,none)

if test "$AWK" = "none";
then echo "  => You must install \`awk\'!"
fi

AC_PATH_PROG(PSNUP,psnup,none)

if test "$PSNUP" = "none";
then echo "  => You should install \`psnup\' for 4-up thumbnails."
fi

echo -----------------------------------------------------------------
echo LaTeX, BibTeX, MakeIndex, TeXindy

AC_PATH_PROG(LATEX,latex,none)
AC_PATH_PROG(BIBTEX,bibtex,none)
AC_PATH_PROG(MAKEINDEX,makeindex,none)
AC_PATH_PROG(TEXINDY,texindy,none)

echo -----------------------------------------------------------------
echo Tools

AC_PATH_PROG(CONVERT,convert,none)
AC_PATH_PROG(FIG2DEV,fig2dev,none)
AC_PATH_PROG(OCAMLWEB,ocamlweb,none)
AC_PATH_PROG(DVIPS,dvips,none)
AC_PATH_PROG(HEVEA,hevea,none)
AC_PATH_PROG(HACHA,hacha,none)
AC_PATH_PROG(DVIPDFM,dvipdfm,none)
AC_PATH_PROG(DVIPDF,dvipdf,none)
AC_PATH_PROG(PS2PDF,ps2pdf,none)
AC_PATH_PROG(PDFTEX,pdftex,none)

echo -----------------------------------------------------------------
echo Directories and paths

echo "  => The source directory is" \'$srcdir\'

# ------------------------------------------------------------------------
# Explicit variable substitutions
#
AC_SUBST(MAKE)
AC_SUBST(SHELL)

AC_SUBST(PSNUP)

AC_SUBST(LATEX)
AC_SUBST(BIBTEX)
AC_SUBST(MAKEINDEX)
AC_SUBST(TEXINDY)

AC_SUBST(CONVERT)
AC_SUBST(FIG2DEV)
AC_SUBST(DVIPS)
AC_SUBST(HEVEA)
AC_SUBST(HACHA)

AC_SUBST(OCAMLWEB)

AC_SUBST(DVIPDFM)
AC_SUBST(DVIPDF)
AC_SUBST(PS2PDF)
AC_SUBST(PDFTEX)

AC_SUBST(srcdir)

AC_SUBST(ac_version)

echo -----------------------------------------------------------------
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
chmod a-w Makefile
