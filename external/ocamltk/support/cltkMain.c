#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#ifdef HAS_UNISTD
#include <unistd.h>  /* for R_OK */
#endif
#include "camltk.h"

#ifndef R_OK
#define R_OK 4
#endif

/* 
 * Dealing with signals: when a signal handler is defined in Caml,
 * the actual execution of the signal handler upon reception of the
 * signal is delayed until we are sure we are out of the GC.
 * If a signal occurs during the MainLoop, we would have to wait
 *  the next event for the handler to be invoked.
 * The following function will invoke a pending signal handler if any,
 * and we put in on a regular timer.
 */

#define SIGNAL_INTERVAL 300

int signal_events = 0; /* do we have a pending timer */

void invoke_pending_caml_signals (ClientData clientdata)
{
  signal_events = 0;
  enter_blocking_section(); /* triggers signal handling */
  /* Rearm timer */
  Tk_CreateTimerHandler(SIGNAL_INTERVAL, invoke_pending_caml_signals, NULL);
  signal_events = 1;
  leave_blocking_section();
}

/* Now the real Tk stuff */

Tk_Window cltk_mainWindow;


/* In slave mode, the interpreter *already* exists */
int cltk_slave_mode = 0;

/* Initialisation, based on tkMain.c */
value camltk_opentk(value argv) /* ML */
{
  /* argv must contain argv[0], the application command name */
  value tmp = Val_unit;
  char *argv0;

  Begin_root(tmp);

  if ( argv == Val_int(0) ){
    failwith("camltk_opentk: argv is empty");
  }
  argv0 = String_val( Field( argv, 0 ) );

  if (!cltk_slave_mode) {
    /* Create an interpreter, dies if error */
#if TCL_MAJOR_VERSION >= 8
    Tcl_FindExecutable(String_val(argv0));
#endif
    cltclinterp = Tcl_CreateInterp();

    if (Tcl_Init(cltclinterp) != TCL_OK)
      tk_error(cltclinterp->result);
    Tcl_SetVar(cltclinterp, "argv0", String_val (argv0), TCL_GLOBAL_ONLY);

    { /* Sets argv if needed */
      int argc = 0;

      tmp = Field(argv, 1); /* starts from argv[1] */
      while ( tmp != Val_int(0) ) {
	argc++;
	tmp = Field(tmp, 1);
      }

      if( argc != 0 ){
	int i;
	char *args;
	char **tkargv;
	char argcstr[256];

	tkargv = malloc( sizeof( char* ) * argc );

	tmp = Field(argv, 1); /* starts from argv[1] */
	i = 0;
	while ( tmp != Val_int(0) ) {
	  tkargv[i] = String_val(Field(tmp, 0));
	  tmp = Field(tmp, 1);
	  i++;
	}
	
	sprintf( argcstr, "%d", argc );

        Tcl_SetVar(cltclinterp, "argc", argcstr, TCL_GLOBAL_ONLY);
        args = Tcl_Merge(argc, tkargv); /* args must be freed by Tcl_Free */
        Tcl_SetVar(cltclinterp, "argv", args, TCL_GLOBAL_ONLY);
        Tcl_Free(args);
	free( tkargv );
      }
    }
    if (Tk_Init(cltclinterp) != TCL_OK)
      tk_error(cltclinterp->result);

    /* Retrieve the main window */
    cltk_mainWindow = Tk_MainWindow(cltclinterp);

    if (NULL == cltk_mainWindow)
      tk_error(cltclinterp->result);
  
    Tk_GeometryRequest(cltk_mainWindow,200,200);
  }

  /* Create the camlcallback command */
  Tcl_CreateCommand(cltclinterp,
                    CAMLCB, CamlCBCmd, 
                    (ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

  /* This is required by "unknown" and thus autoload */
  Tcl_SetVar(cltclinterp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
  /* Our hack for implementing break in callbacks */
  Tcl_SetVar(cltclinterp, "BreakBindingsSequence", "0", TCL_GLOBAL_ONLY);

  /* Load the traditional rc file */
  {
    char *home = getenv("HOME");
    if (home != NULL) {
      char *f = stat_alloc(strlen(home)+strlen(RCNAME)+2);
      f[0]='\0';
      strcat(f, home);
      strcat(f, "/");
      strcat(f, RCNAME);
      if (0 == access(f,R_OK)) 
        if (TCL_OK != Tcl_EvalFile(cltclinterp,f)) {
          stat_free(f);
          tk_error(cltclinterp->result);
        };
      stat_free(f);
    }
  }

  End_roots();
  return Val_unit;
}

value camltk_finalize(value unit) /* ML */
{
  Tcl_Finalize();
  return Val_unit;
}
