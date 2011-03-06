#include <string.h>
#include <unistd.h>  /* for R_OK */
#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include "camltk.h"

/* Support for using Caml as a slave in a Tcl/Tk + C program */

/* initialisation of camltk with an existing tcl interpreter 
   This should be called by the application.
 */
char *default_argv[] = { NULL };

void Camltk_Init(interp)
     Tcl_Interp *interp;
{

  cltclinterp = interp;
  cltk_mainWindow = Tk_MainWindow(cltclinterp);
  cltk_slave_mode = 1;

  /* now initialize the runtime : bytecode and stub code must have been 
   * obtained by ocamlc -output-obj
   */
  caml_startup(default_argv);
  /* normally, the code of the application does opentk,
     which will in this case, since slave_mode=1, install only
     camlcb and set various globals.
     Also, mainLoop in slave_mode does nothing.
     Basically, we should get back here, but with some Tk widgets
     installed, and possibly some proc defined.
     Note: an "application" doing cleanup on return of mainLoop would
     not fit as is.
   */
}
