Release: 1
Version: 0.418
Name: camltk
Summary: camltk library
Source: ftp://ftp.inria.fr/lang/caml-light/bazar-ocaml/ocamltk/ocamltk418.tar.gz
Group: Development/Libraries
URL: http://pauillac.inria.fr/camltk/
Copyright: LGPL
Packager: <Jun.Furuse@inria.fr>
Provides: camltk
Prefix: /usr

%description 
CamlTk is a library for interfacing Objective Caml with the scripting language
Tcl/Tk.

%prep

%setup -n camltk418

%build
./configure --with-config=rpm.config
make all opt

%install
make install installopt

%clean
make clean

%files
%dir /usr/lib/ocaml/camltk
/usr/lib/ocaml/camltk/Makefile.camltk
/usr/lib/ocaml/camltk/balloon.cmi
/usr/lib/ocaml/camltk/balloon.mli
/usr/lib/ocaml/camltk/bell.cmi
/usr/lib/ocaml/camltk/bell.mli
/usr/lib/ocaml/camltk/button.cmi
/usr/lib/ocaml/camltk/button.mli
/usr/lib/ocaml/camltk/camltk.a
/usr/lib/ocaml/camltk/camltk.cma
/usr/lib/ocaml/camltk/camltk.cmxa
/usr/lib/ocaml/camltk/canvas.cmi
/usr/lib/ocaml/camltk/canvas.mli
/usr/lib/ocaml/camltk/checkbutton.cmi
/usr/lib/ocaml/camltk/checkbutton.mli
/usr/lib/ocaml/camltk/clipboard.cmi
/usr/lib/ocaml/camltk/clipboard.mli
/usr/lib/ocaml/camltk/cltkaniminit.o
/usr/lib/ocaml/camltk/dialog.cmi
/usr/lib/ocaml/camltk/dialog.mli
/usr/lib/ocaml/camltk/entry.cmi
/usr/lib/ocaml/camltk/entry.mli
/usr/lib/ocaml/camltk/fileevent.cmi
/usr/lib/ocaml/camltk/fileevent.mli
/usr/lib/ocaml/camltk/fileselect.cmi
/usr/lib/ocaml/camltk/fileselect.mli
/usr/lib/ocaml/camltk/focus.cmi
/usr/lib/ocaml/camltk/focus.mli
/usr/lib/ocaml/camltk/frame.cmi
/usr/lib/ocaml/camltk/frame.mli
/usr/lib/ocaml/camltk/frx_after.cmi
/usr/lib/ocaml/camltk/frx_after.mli
/usr/lib/ocaml/camltk/frx_color.cmi
/usr/lib/ocaml/camltk/frx_color.mli
/usr/lib/ocaml/camltk/frx_ctext.cmi
/usr/lib/ocaml/camltk/frx_ctext.mli
/usr/lib/ocaml/camltk/frx_dialog.cmi
/usr/lib/ocaml/camltk/frx_dialog.mli
/usr/lib/ocaml/camltk/frx_entry.cmi
/usr/lib/ocaml/camltk/frx_entry.mli
/usr/lib/ocaml/camltk/frx_fillbox.cmi
/usr/lib/ocaml/camltk/frx_fillbox.mli
/usr/lib/ocaml/camltk/frx_fit.cmi
/usr/lib/ocaml/camltk/frx_fit.mli
/usr/lib/ocaml/camltk/frx_focus.cmi
/usr/lib/ocaml/camltk/frx_focus.mli
/usr/lib/ocaml/camltk/frx_font.cmi
/usr/lib/ocaml/camltk/frx_font.mli
/usr/lib/ocaml/camltk/frx_lbutton.mli
/usr/lib/ocaml/camltk/frx_listbox.cmi
/usr/lib/ocaml/camltk/frx_listbox.mli
/usr/lib/ocaml/camltk/frx_mem.cmi
/usr/lib/ocaml/camltk/frx_mem.mli
/usr/lib/ocaml/camltk/frx_misc.cmi
/usr/lib/ocaml/camltk/frx_misc.mli
/usr/lib/ocaml/camltk/frx_req.cmi
/usr/lib/ocaml/camltk/frx_req.mli
/usr/lib/ocaml/camltk/frx_rpc.cmi
/usr/lib/ocaml/camltk/frx_rpc.mli
/usr/lib/ocaml/camltk/frx_selection.cmi
/usr/lib/ocaml/camltk/frx_selection.mli
/usr/lib/ocaml/camltk/frx_synth.cmi
/usr/lib/ocaml/camltk/frx_synth.mli
/usr/lib/ocaml/camltk/frx_text.cmi
/usr/lib/ocaml/camltk/frx_text.mli
/usr/lib/ocaml/camltk/frx_toplevel.mli
/usr/lib/ocaml/camltk/frx_widget.cmi
/usr/lib/ocaml/camltk/frx_widget.mli
/usr/lib/ocaml/camltk/grab.cmi
/usr/lib/ocaml/camltk/grab.mli
/usr/lib/ocaml/camltk/grid.cmi
/usr/lib/ocaml/camltk/grid.mli
/usr/lib/ocaml/camltk/imagebitmap.cmi
/usr/lib/ocaml/camltk/imagebitmap.mli
/usr/lib/ocaml/camltk/imagephoto.cmi
/usr/lib/ocaml/camltk/imagephoto.mli
/usr/lib/ocaml/camltk/jpf_font.cmi
/usr/lib/ocaml/camltk/jpf_font.mli
/usr/lib/ocaml/camltk/jtk.cmi
/usr/lib/ocaml/camltk/jtk.cmo
/usr/lib/ocaml/camltk/jtk.cmx
/usr/lib/ocaml/camltk/jtk.mli
/usr/lib/ocaml/camltk/jtk.o
/usr/lib/ocaml/camltk/label.cmi
/usr/lib/ocaml/camltk/label.mli
/usr/lib/ocaml/camltk/libcamltk.a
/usr/lib/ocaml/camltk/libfrx.a
/usr/lib/ocaml/camltk/libfrx.cma
/usr/lib/ocaml/camltk/libfrx.cmxa
/usr/lib/ocaml/camltk/libjpf.a
/usr/lib/ocaml/camltk/libjpf.cma
/usr/lib/ocaml/camltk/libjpf.cmxa
/usr/lib/ocaml/camltk/libtkanim.a
/usr/lib/ocaml/camltk/listbox.cmi
/usr/lib/ocaml/camltk/listbox.mli
/usr/lib/ocaml/camltk/menu.cmi
/usr/lib/ocaml/camltk/menu.mli
/usr/lib/ocaml/camltk/menubutton.cmi
/usr/lib/ocaml/camltk/menubutton.mli
/usr/lib/ocaml/camltk/message.cmi
/usr/lib/ocaml/camltk/message.mli
/usr/lib/ocaml/camltk/ocamltktop
/usr/lib/ocaml/camltk/optionmenu.cmi
/usr/lib/ocaml/camltk/optionmenu.mli
/usr/lib/ocaml/camltk/pack.cmi
/usr/lib/ocaml/camltk/pack.mli
/usr/lib/ocaml/camltk/palette.cmi
/usr/lib/ocaml/camltk/palette.mli
/usr/lib/ocaml/camltk/pixmap.cmi
/usr/lib/ocaml/camltk/pixmap.mli
/usr/lib/ocaml/camltk/place.cmi
/usr/lib/ocaml/camltk/place.mli
/usr/lib/ocaml/camltk/protocol.cmi
/usr/lib/ocaml/camltk/protocol.mli
/usr/lib/ocaml/camltk/radiobutton.cmi
/usr/lib/ocaml/camltk/radiobutton.mli
/usr/lib/ocaml/camltk/resource.cmi
/usr/lib/ocaml/camltk/resource.mli
/usr/lib/ocaml/camltk/safefrx.ml
/usr/lib/ocaml/camltk/safefrx.mli
/usr/lib/ocaml/camltk/safejtk.ml
/usr/lib/ocaml/camltk/safejtk.mli
/usr/lib/ocaml/camltk/safetk.ml
/usr/lib/ocaml/camltk/safetk.mli
/usr/lib/ocaml/camltk/safetkanim.ml
/usr/lib/ocaml/camltk/safetkanim.mli
/usr/lib/ocaml/camltk/scale.cmi
/usr/lib/ocaml/camltk/scale.mli
/usr/lib/ocaml/camltk/scrollbar.cmi
/usr/lib/ocaml/camltk/scrollbar.mli
/usr/lib/ocaml/camltk/selection.cmi
/usr/lib/ocaml/camltk/selection.mli
/usr/lib/ocaml/camltk/shell.cmi
/usr/lib/ocaml/camltk/shell.mli
/usr/lib/ocaml/camltk/text.cmi
/usr/lib/ocaml/camltk/text.mli
/usr/lib/ocaml/camltk/textvariable.cmi
/usr/lib/ocaml/camltk/textvariable.mli
/usr/lib/ocaml/camltk/timer.cmi
/usr/lib/ocaml/camltk/timer.mli
/usr/lib/ocaml/camltk/tk.cmi
/usr/lib/ocaml/camltk/tk80.cmi
/usr/lib/ocaml/camltk/tk80.cmo
/usr/lib/ocaml/camltk/tk80.cmx
/usr/lib/ocaml/camltk/tk80.mli
/usr/lib/ocaml/camltk/tk80.o
/usr/lib/ocaml/camltk/tkanim.cmi
/usr/lib/ocaml/camltk/tkanim.cmo
/usr/lib/ocaml/camltk/tkanim.cmx
/usr/lib/ocaml/camltk/tkanim.mli
/usr/lib/ocaml/camltk/tkanim.o
/usr/lib/ocaml/camltk/tkaniminit.cmi
/usr/lib/ocaml/camltk/tkaniminit.mli
/usr/lib/ocaml/camltk/tkwait.cmi
/usr/lib/ocaml/camltk/tkwait.mli
/usr/lib/ocaml/camltk/toplevel.cmi
/usr/lib/ocaml/camltk/toplevel.mli
/usr/lib/ocaml/camltk/widget.cmi
/usr/lib/ocaml/camltk/widget.mli
/usr/lib/ocaml/camltk/winfo.cmi
/usr/lib/ocaml/camltk/winfo.mli
/usr/lib/ocaml/camltk/wm.cmi
/usr/lib/ocaml/camltk/wm.mli

%changelog

* Mon Feb 04 2002 <Jun.Furuse@inria.fr>

Packaged in a rpm. Here is the modus operandi:
cd /usr/src/redhat/SPECS/
cp camltk.spec ./
rpm -ba --clean camltk.spec
