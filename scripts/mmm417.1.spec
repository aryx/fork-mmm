Summary: The MMM Web browser
Name: mmm
Version: 0.417
Release: 1
Copyright: freely redistributable (see LICENSE file)
Group: Applications/Networking
Source: ftp://ftp.inria.fr/INRIA/Projects/cristal/mmm/mmm417-src.tar.gz
Vendor: INRIA Rocquencourt
Packager: author <Francois.Rouaix@inria.fr>
URL: http://pauillac.inria.fr/mmm/

%description
MMM is a Web browser written in Objective Caml with its Tcl/Tk interface.
MMM implements HTTP 1.0, HTML 2.0, most of HTML 3.2 and frames.
MMM supports Caml applets.

Requires: ocaml >= 3.0
Requires: tcl
Requires: tk

%prep
%setup

%build
# We produce a Makefile.config with predefined values
(echo include /usr/lib/ocaml/camltk41/Makefile.camltk;
 echo INSTALLLIBDIR=/usr/lib/mmm;
 echo INSTALLBINDIR=/usr/bin;
 echo MANDIR=/usr/man/man1;
 echo MANEXT=1;
 sed -n '/## Common/,$p' Makefile.config.tpl) > Makefile.config

make depend
make all opt
# At this point we have produced all binaries

%install
# Install everything in /usr/lib/mmm and /usr/bin
make install installopt install-mdk
# Surboard
cp sboard/surfboard sboard/surfboard_remote /usr/bin

%post
# Interactive shell does not work here (and it isn't such a good idea).
echo "You might want to edit /usr/bin/mmm and set the variables"
echo "proxy and port to default values."

%files
/usr/bin/mmm
/usr/bin/mmmc
/usr/bin/mmm_remote
/usr/bin/htparse
/usr/bin/surfboard
/usr/bin/surfboard_remote
/usr/lib/mmm
/usr/man/man1/mmm.1
/usr/man/man1/mmm_remote.1
/usr/man/man1/htparse.1
%doc LICENSE README ALIRE
