#include <tk.h>
#include <caml/mlvalues.h>

extern Tcl_Interp *cltclinterp;

value blt_init (rien) /* ML */
     value rien;
{
  if (Blt_Init(cltclinterp) != TCL_OK)
    tk_error ("Can't initialize BLT");
  return Val_unit;
}
