#include <tk.h>
#include <caml/mlvalues.h>

extern Tcl_Interp *cltclinterp;

value tkanim_init (rien) /* ML */
     value rien;
{
  if (Tkanim_Init(cltclinterp) != TCL_OK)
    tk_error ("Can't initialize TkAnim");
  return Val_unit;
}
