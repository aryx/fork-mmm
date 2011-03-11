#include <tk.h>
#include <caml/mlvalues.h>
#include "camltk.h"

extern Tcl_Interp *cltclinterp;
extern int Tkanim_Init( Tcl_Interp * );

value tkanim_init (rien) /* ML */
     value rien;
{
  if (Tkanim_Init(cltclinterp) != TCL_OK)
    tk_error ("Can't initialize TkAnim");
  return Val_unit;
}
