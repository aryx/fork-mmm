open Html
open Misc

exception FoundAt of int
exception Done_string of string

let idcnt = ref 0
let newid () = incr idcnt; !idcnt

type itemtype =
    Separator
  | Page
  | Dir

type status =
    Opened
  | Closed

let indent lev = PCData (String.make (lev * 4) ' ')

let list_str elem_str delim = function 
    [] -> ""
  | l ->
    let ebuf = Ebuffer.create 100 in
    let rec sub = function
	[] -> Ebuffer.get ebuf
      |	x::xs -> 
	  Ebuffer.output_string ebuf (elem_str x);
	  if xs <> [] then Ebuffer.output_string ebuf delim;
	  sub xs
    in
    sub l

let encode_comment s =
  let len = 
    begin try
      for i = String.length s - 1 downto 0 do
	match s.[i] with
	  '\n' | '\t' | ' ' -> ()
	| _ -> raise (FoundAt (i + 1))
      done;
      0
    with
      FoundAt i -> i
    end
  in
  let ebuf = Ebuffer.create (String.length s) in
  for i = 0 to len - 1 do
    match s.[i] with
      '\n' -> Ebuffer.output_string ebuf "<BR>\n"
    | c -> Ebuffer.output_char ebuf c
  done;
  Ebuffer.get ebuf
  
let do_tokens f tl =
  List.iter (function
      PCData s -> f s
    | OpenTag {tag_name= tn; attributes= al} ->
	f "<";
	f tn;
	if al <> [] then begin
	  f " ";
	  f (list_str (fun (t,v) ->
	       if v = "" then t 
	       else (Printf.sprintf "%s=\"%s\"" t v)) " " al);
	end;
	f ">"
    | CloseTag t ->
	f "</"; f t; f">"
    | _ -> () ) tl

let output_tokens oc = do_tokens (output_string oc)
let string_of_tokens tl = 
  let ebuf = Ebuffer.create 1000 in
  do_tokens (Ebuffer.output_string ebuf) tl;
  Ebuffer.get ebuf

class type t_item = object
 method sel : bool
 method set_sel : bool -> unit
 method id : int
 method parent : t_dir option
 method set_parent : t_dir option -> unit
 method itemtype : itemtype
 method name : string
 method separator : t_separator
 method page : t_page
 method dir : t_dir
 method set_comment : string -> unit
 method remove_self : unit -> unit
 method tokens : int -> Html.token list 
 end

and t_separator = t_item

and t_dir = object ('a)
  inherit t_item

  method status : status
  method set_status : status -> unit
  method adddate : int
  method set_adddate : int -> unit

  method title : string
  method set_title : string -> unit

  method comment : string

  method wheretoadd : bool
  method set_wheretoadd : bool -> unit

  method items : t_item list
  method set_items : t_item list -> unit
  method iter : ((t_dir -> unit) * (t_page -> unit) * (t_separator -> unit)) -> unit
end

and t_page = object ('a)
  inherit t_item
  method url : string
  method set_url : string -> unit

  method title : string
  method set_title : string -> unit

  method comment : string

  method adddate : int
  method set_adddate : int -> unit

  method lastvisit : int
  method set_lastvisit : int -> unit

  method lastmodified : int
  method set_lastmodified : int -> unit

 end


class virtual item () = object (self)
  val mutable sel = false
  method sel = sel
  method set_sel new_sel = sel <- new_sel
 
  val id = newid ()
  method id = id

  val mutable parent = None
  method parent = parent
  method set_parent (new_parent : t_dir option) = parent <- new_parent

  method virtual itemtype : itemtype

  method virtual name : string

  method separator = (Obj.magic self : t_separator)
  method page      = (Obj.magic self : t_page)
  method dir       = (Obj.magic self : t_dir)

  method virtual set_comment : string -> unit

  method remove_self () =
    match parent with
      Some p ->
	p#set_items (List.fold_right (fun x st ->
	  (* we cannot use self because it closes the class *)
	  if x#id = id then st else x :: st) p#items [] );
	parent <- None
    | None -> raise (Failure "Tried to remove isolated item")
	
  method virtual tokens : int -> Html.token list 
 end

and separator () = object
  inherit item ()

  method itemtype = Separator

  method page      = raise (Failure "??? sep#page")
  method dir       = raise (Failure "??? sep#dir")

  method set_comment = fun _ -> ()

  method tokens lev = 
    indent lev ::
    [OpenTag {tag_name= "HR"; attributes= []}]

  method name = "Separator"
 end

and page () = object (self)
  inherit item ()

  method itemtype = Page

  method separator = raise (Failure "??? page#sep")
  method dir       = raise (Failure "??? page#dir")

  val mutable url = ("" : string)
  method url = url
  method set_url newurl = url <- newurl 

  val mutable title = ""
  method name = title

  method title = title
  method set_title newtitle = title <- newtitle

  val mutable comment = ""
  method comment = comment
  method set_comment newcomment = comment <- newcomment

  val mutable adddate = -1
  method adddate = adddate
  method set_adddate new_adddate = adddate <- new_adddate

  val mutable lastvisit = -1
  method lastvisit = lastvisit
  method set_lastvisit new_lastvisit = lastvisit <- new_lastvisit

  val mutable lastmodified = -1
  method lastmodified = lastmodified
  method set_lastmodified new_lastmodified = lastmodified <- new_lastmodified

  method tokens lev = 
    indent lev ::
    [ OpenTag { tag_name= "DT"; attributes= []};
      OpenTag { tag_name= "A"; attributes=
		["HREF", url] @
		( if adddate = -1 then []
		  else ["ADD_DATE", string_of_int adddate] ) @
		( if lastvisit = -1 then [] 
		  else ["LAST_VISIT", string_of_int lastvisit]) @
		( if lastmodified = -1 then [] 
		  else [ "LAST_MODIFIED", 
			 string_of_int lastmodified])};
      PCData title;
      CloseTag "A";
      PCData "\n" ] @
    ( if comment = "" then [] 
      else 
	indent lev :: 
	[ OpenTag {tag_name= "DD"; attributes= []};
	  PCData (encode_comment comment);
	  PCData "\n"] )
 end

and dir () = object (self)
  inherit item ()

  method itemtype = Dir

  method separator = (raise (Failure "??? dir#sep") : t_separator)
  method page      = (raise (Failure "??? dir#page") : t_page)

  val mutable status = Opened
  method status = status
  method set_status new_status = status <- new_status

  val mutable adddate = -1
  method adddate = adddate
  method set_adddate new_adddate = adddate <- new_adddate

  val mutable title = ""
  method name = title

  method title = title
  method set_title newtitle = title <- newtitle

  val mutable comment = ""
  method comment = comment
  method set_comment newcomment = comment <- newcomment

  val mutable wheretoadd = false
  method wheretoadd = wheretoadd
  method set_wheretoadd b = wheretoadd <- b

  val mutable items = ([] : t_item list)
  method items = items
  method set_items new_items = 
    items <- new_items;
    List.iter (fun i -> i#set_parent (Some (self : #t_dir :> t_dir))) items

  method tokens level = 
    begin match parent with
      None ->
	[ PCData "<!DOCTYPE MMM-Surfboard-file-1>\n<!-- This is an automatically generated file.\nIt will be read and overwritten.\nDo Not Edit! -->\n";
	  OpenTag {tag_name= "TITLE"; attributes= []};
	  PCData title;
	  CloseTag "TITLE";
	  PCData "\n";
	  OpenTag {tag_name= "H1"; attributes= []};
	  PCData title;
	  CloseTag "H1";
	  PCData "\n"] 
    | Some _ ->
	indent level ::
	[ OpenTag {tag_name= "DT"; attributes= []};
	  OpenTag {tag_name= "H3"; attributes= 
		    (if status = Closed then ["FOLDED", ""] else []) @
		    (if wheretoadd then ["NEWITEMHEADER", ""] else []) @ 
		    (if adddate = -1 then [] 
		     else ["ADD_DATE", string_of_int adddate])};
	  PCData title;
	  CloseTag "H3";
	  PCData "\n"]
    end @ 
    ( if comment = "" then []
      else 
        [ OpenTag {tag_name= "DD"; attributes= [] };
	  PCData (encode_comment comment);
	  PCData "\n" ] ) @
    [ indent level;
      OpenTag {tag_name= "DL"; attributes= []};
      OpenTag {tag_name= "p"; attributes= []};
      PCData "\n"] @
    List.flatten (List.map (fun i -> i#tokens (level + 1)) items) @
    [ CloseTag "DL";
      OpenTag {tag_name= "p"; attributes= []};
      PCData "\n"]

  method iter ((d : t_dir -> unit),p,s) =
    d (self : #t_dir :> t_dir);
    List.iter (fun i ->
      let i = (i : #t_item :> t_item) in
      match i#itemtype with
	Separator -> s i#separator
      |	Page -> p i#page
      |	Dir -> i#dir#iter (d,p,s) ) items
end

class tree (init_root : dir) = object (self)
  val mutable wheretoadd = None
  method wheretoadd = wheretoadd
  method set_wheretoadd (new_wheretoadd : dir option) = 
    begin match wheretoadd with
      Some d -> d#set_wheretoadd false
    | _ -> ()
    end;
    wheretoadd <- new_wheretoadd;
    begin match wheretoadd with
      Some d -> d#set_wheretoadd true
    | _ -> ()
    end
		  
  val root = init_root
  method root = root

  method output oc =
    output_tokens oc ((root :> item)#tokens 0)

  method add_page (p : page) =
    match wheretoadd with
      Some d -> d#set_items (d#items @ [(p :> item)])
    | None -> raise (Failure "wheretoadd is not set")
end

let rec root (item : item) =
  match item#parent with
    None -> item#dir (* must be a dir *)
  | Some p -> root (p :> item)

exception Done_dir of dir

let current_pos = ref 0

let parse lexbuf =
  let lexer = Html_eval.sgml_lexer Dtd.dtd20 in
  current_pos := 0;
  let get_next_token =
    let buf = ref [] in
    (function () ->
      (* if it is empty then get new one *) 
      while !buf = [] do
	let _, _, tokens, Loc (_,pos) = lexer lexbuf in
	current_pos := pos;
	buf := !buf @ tokens
      done;
      let token = List.hd !buf in
      buf := List.tl !buf;
      token)
  in
  let rec jump t =
    let buf = ref "" in
    try
      while true do 
	match get_next_token () with
	  OpenTag {tag_name= tag} when tag = t -> (* nested tags *)
	    buf := !buf ^ jump t 
	| CloseTag tag when tag = t -> (* matched closing tag *)
	    raise (Done_string !buf)
	| EOF -> raise (Failure "No matched tag at jump")  
	| x -> buf := !buf ^ string_of_tokens [x]
      done;
      !buf
    with
      Done_string s -> s
  in
  let next_title = ref "No title"
  and next_attr = ref []
  and next_comment = ref ""
  and in_comment = ref false
  and wheretoadd = ref None
  in
  let rec read_directory () =
    try
    let dir = new dir () in
    dir#set_status (default (fun () ->
      List.assoc "folded" !next_attr; Closed) Opened);
    dir#set_title !next_title; 
    dir#set_comment !next_comment;
    dir#set_adddate (default (fun () ->
      int_of_string (List.assoc "add_date" !next_attr)) (-1));
    (* reset next *)
    next_title := "No title";
    next_comment := "";
    next_attr := []; (* NEWITEMHEADER *)
    let items = ref [] in
    let cur_item = ref (dir :> item) in
    let close_comment () = 
      if !in_comment then begin
	(* wrong. this is the comment for the next *)
	(* (!cur_item)#set_comment !comment; *)
	in_comment := false
      end
    in
    while true do
      let t = get_next_token () in
      match t with
	CloseTag "dl"
      |	EOF -> (* end of this directory *)
	  close_comment ();
	  (* pages are stored in reverse order *)
	  dir#set_items (List.rev !items);
	  raise (Done_dir dir)
      |	OpenTag { tag_name= "dt" } ->
	  (* DT is not important in this system, but it stops comment *)
	  close_comment ()
      |	OpenTag { tag_name= h; attributes= attr }
	when h = "h1" || h = "h2" || h = "h3" -> (* directory title *)
	  (* directory title is stored, but it is never used if there is *)
	  (* no <DL> occurred later.                                     *)
	  next_attr := attr; 
	  next_title := jump h
      |	OpenTag {tag_name= "a"; attributes= attr} -> (* a page *)
	  begin try 
	    let title = jump "a" in
  	    (* Log.debug ("Page: "^title); *)
	    let page = new page () in
	    cur_item := (page :> item);
	    page#set_url (List.assoc "href" attr);
	    if not (Uri.is_absolute page#url) then raise (Failure "not abs");
	    page#set_lastvisit (default (fun () -> 
  	      int_of_string (List.assoc "last_visit" attr)) (-1));
	    page#set_lastmodified (default (fun () ->
  	      int_of_string (List.assoc "last_modified" attr)) (-1));
  	    page#set_title title;
	    page#set_adddate (default (fun () -> 
	      int_of_string (List.assoc "add_date" attr))  (-1));
	    page#set_comment !next_comment;
	    next_comment := "";
	    items :=  (page :> item) :: !items 
	  with
	    _ -> ()
	  end
      |	OpenTag {tag_name= "hr"} ->
	  let sep = new separator () in
	  cur_item := (sep :> item);
	  items := (sep :> item) :: !items
      |	OpenTag {tag_name= "dl"} -> (* a sub directory *)
	  close_comment ();
	  let nih = List.mem_assoc "newitemheader" !next_attr in
	  let newdir = read_directory () in 
	  if nih then wheretoadd := Some newdir;
	  items := (newdir :> item) :: !items 
      |	OpenTag {tag_name= "dd"} -> (* comment *)
	  (* clean current comment *)
	  next_comment := "";
	  (* enter comment mode *)
	  in_comment := true
      |	OpenTag {tag_name= "br"} -> (* comment CR. ignore *)
	  ()
      |	PCData s ->
	  (* if it is in comment mode, store the string as a comment. *)
	  if !in_comment then next_comment := !next_comment ^ s
      |	CData s ->
	  (* if it is in comment mode, store the string as a comment. *)
	  if !in_comment then next_comment := !next_comment ^ s
      |	_ -> ()
    done;
    dir
  with
    Done_dir dir -> dir
  in
    let dir = read_directory () in
    let final_dir = 
      match dir#items with
  	[] -> (* What ??? I don't want to take care about such strange case*) 
  	  dir
      | [i] when i#itemtype = Dir -> (* good! singleton dir *)
  	  i#set_parent None;
	  i#dir
      | _ -> dir
    in 
    let wheretoadd = match !wheretoadd with
      None -> 
	Log.debug "No newitemheader";
	Some final_dir
    | s -> s
    in
    let tree = new tree final_dir 
    in
    tree#set_wheretoadd wheretoadd;
    tree

