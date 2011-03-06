#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include "camltk.h"


/* Basically the same thing as FileProc */
void TimerProc (clientdata)
     ClientData clientdata;
{
  callback2(*handler_code,Val_int(clientdata),Val_int(0));
}

value camltk_add_timer(milli, cbid) /* ML */
     value milli;
     value cbid;
{
  CheckInit();
  /* look at tkEvent.c , Tk_Token is an int */ 
  return (Val_int(Tcl_CreateTimerHandler(Int_val(milli), TimerProc, 
				       (ClientData) (Int_val(cbid)))));
}

value camltk_rem_timer(token) /* ML */
     value token;
{
  Tcl_DeleteTimerHandler((Tcl_TimerToken) Int_val(token));
  return Val_unit;
}

