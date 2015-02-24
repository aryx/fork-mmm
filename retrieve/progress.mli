(*s: ./retrieve/progress.mli *)
(*s: signature Progress.no_meter *)
val no_meter : Scheduler.progress_func
(*e: signature Progress.no_meter *)
(*s: signature Progress.meter *)
val meter : Widget.widget -> Scheduler.progress_func
(*e: signature Progress.meter *)
(*e: ./retrieve/progress.mli *)
