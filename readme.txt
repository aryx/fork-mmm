MMM is a Web browser written in Objective Caml with its Tcl/Tk interface.
This is release 0.418 of MMM; its main features are:

 - runs on any Unix/X11 platform 
 - supports HTTP/1.0, (most of) HTML 3.2, frames
   Some will find table display in MMM preposterous. It is, in some sense.
   Check the FAQ for more information on this.
 - supports Objective Caml applets
 - supports CGI calling (no need for a server)
 - localized to english, français, and japanese
   (can easily be localized for other western languages)
 - supports japanese documents (if linked with appropriate Tk libs)
 - can display HTML errors and edit HTML source
 - freely available in source form
 - freely available in binary form for Linux/i386 (RPM)
 - can be used freely
 - can be distributed unmodified freely

Distributions and documentation are available from
<URL:http://pauillac.inria.fr/~rouaix/mmm/>

Compilation requirements:
 Objective Caml 3.00
 ocamltk418 (available at <URL:http://pauillac.inria.fr/camltk/>) 
 Tcl/Tk (8.3/8.3, 8.0/8.0, 7.5/4.1, or 7.6/4.2 only, includes and libraries).

Runtime-requirements:
 Tcl/Tk (8.3/8.3, 8.0/8.0, 7.5/4.1 or 7.6/4.2 only, shared libraries
 and standard script files),
 metamail,
 djpeg (or any JPEG to GIF/PNM converter or Tcl/Tk extension to support JPEG),
 (facultative)
 Objective Caml 3.04, if you want to be able to compile applets on the fly.
     
Main changes since last version:
 - Fixes of the safe libraries for O'Caml 3.04

