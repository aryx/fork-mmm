open Unix

(************************************************************* Subshell call *)

let subshell cmd = 
  let r,w = pipe () in
    match fork () with
      0 -> close r; dup2 w stdout; 
	   close stderr;
      	   execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]; exit 127
    | id -> 
        close w; 
        let rc = in_channel_of_descr r in
        let rec it () = try 
       	    let x = input_line rc in x:: it ()
          with _ -> []
        in 
      	  let answer = it() in
	  close_in rc;	(* because of finalize_channel *)
	  let p, st = waitpid [] id in answer

