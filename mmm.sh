#!/bin/sh
# This script runs MMM after merging user options with default options

### BEGINNING OF CONFIGURATION

# CHECK: the MMM_MAIL variable
# for use with the internal mailto: interface. This command *must* be
# compatible with /usr/ucb/mail, that is, accept
# $ $MMM_MAIL -s subject address < msg_body
# If left undefined, MMM will use the first "mail" command in the user's PATH
# We suggest the following:
if [ -f /bin/mail ]; then
  MMM_MAIL=/bin/mail
  export MMM_MAIL
fi

# CHECK: default proxy host. Can be overidden by users.
proxy=www-proxy.domain
port=80

# CHECK: MMMDIR should be the same as INSTALLLIBDIR in Makefile.config
# "make install" normally does this substitution.
MMMDIR=_INSTALLLIBDIR_

### END OF CONFIGURATION
# Below this line, there should be nothing to change

# Binaries copied during installation
bin=$MMMDIR/mmm.bin
binx=$MMMDIR/mmmx.bin
# The default I18N message file
msgfile_default=$MMMDIR/msgs.txt
# The default language if any
lang=`echo $LANG | sed -e 's/^ja.*/ja/'`

# This switch should contain all possible options of mmm.bin
# For options with values in this file, do not add to opts, as we
# rebuild the command line later.
opts=
while : ; do
  case $1 in
    "") break;;
   -proxy) proxy=$2; shift;;
   -port) port=$2; shift;;
   -d) display=$2; shift;;
   -display) display=$2; shift;;
   -suffixes) suffixes=$2; shift;;
   -external) opts="$opts -external";;
   -lang) lang=$2; shift;;
   -msgfile) msgfile=$2; shift;;
   -prefs) prefs=$2; shift;;
   -helpurl) helpurl=$2; shift;;
   -fast) [ -f $binx ] && bin=$binx;;
    *) opts="$opts $1";;
   esac
   shift
done

# msgfile
msgfile_i18n=$MMMDIR/msgs.$lang.txt
if [ "x$msgfile" = "x" -a -f "$msgfile_i18n" ] ; then
  msgfile=$msgfile_i18n
fi

# Default location of doc (installed files).
# You may override this (eg pointing to a server), but
# please do *not* point directly to our doc page.
if [ "X$helpurl" = "X" ] 
then
    if [ -f $MMMDIR/doc/docindex-$lang.html ]
    then
	helpurl=file://$MMMDIR/doc/docindex-$lang.html
    else
	helpurl=file://$MMMDIR/doc/docindex.html
    fi
fi

# Create ~/.mmm if it does not exists
if [ ! -d $HOME/.mmm ]; then
   mkdir $HOME/.mmm
fi

# Copy MMM.ad.$lang or MMM.ad if it does not exists
if [ \( ! -f "$HOME/.mmm/MMM.ad.$lang" \) -a \( ! -f "$HOME/.mmm/MMM.ad" \) ]
then
  if [ -f "$MMMDIR/MMM.ad.$lang" ]
  then
    cp "$MMMDIR/MMM.ad.$lang" "$HOME/.mmm/"
  else
    cp "$MMMDIR/MMM.ad" "$HOME/.mmm/"
  fi
fi  

# Rebuild command line
args=
[ "$proxy" ] && args="$args -proxy $proxy"
[ "$port" ] && args="$args -port $port"
[ "$display" ] && args="$args -display $display"
[ "$suffixes" ] && args="$args -suffixes $suffixes"
# external is in opts
[ "$lang" ] && args="$args -lang $lang"
[ "$msgfile" ] && args="$args -msgfile $msgfile"
[ "$prefs" ] && args="$args -prefs $prefs"
[ "$helpurl" ] && args="$args -helpurl $helpurl"
[ "$opts" ] && args="$args $opts"

exec $bin $args
