(*
 * Mediawiki wysiwyg engine 
 *
 * (c) 2010 Myrilion SAS
 *
 *)

open Printf
open Misc
open Tools 

external utf8_decode : string -> string = "@Utf8.decode"


let text t : Dom.element = 
  Obj.magic (dd#createTextNode (utf8_decode t)) 
 
let textl tl : Dom.element = 
  let t = 
    match tl with 
	t::q -> 
	  List.fold_left (fun acc e -> acc ^ " " ^ e) t q 
      | [] -> "" in

    Obj.magic (dd#createTextNode (utf8_decode t )) 
		    
let heading d = 
  let k = sprintf "h%d" d in
   dd#createElement k

let quoted q = 
  let e = dd#createElement "span" in
    List.iter (function t -> e#appendChild t >>> ignore) q ;  
    e

let b () = 
   dd#createElement "b"

let i () = 
  dd#createElement "i"
    
let table () = 
  dd#createElement "table"

let tr () = 
  dd#createElement "tr"

let td () = 
  dd#createElement "td"

let th () = 
  dd#createElement "th"

let reference () = 
  dd#createElement "span"
  
let link () = 
  dd#createElement "a"
  
let ul () = 
  dd#createElement "ul" 
  
let ol () = 
  dd#createElement "ol" 

let li () = 
  dd#createElement "li"

let code () = 
  let e = dd#createElement "span" in 
    e#setAttribute "kind" "code"; 
    e 

let nowiki () = 
 let e = dd#createElement "span" in 
    e#setAttribute "kind" "nowiki"; 
    e 
  
let internal_link l = 
  match l with 
      t::[] -> 
	let e = dd#createElement "a" in 
	  e#setAttribute "href" (utf8_decode (format_link t));
	  e#setAttribute "wiki_href" (utf8_decode t) ; 
	  e#appendChild (text t) ;  
	  e 
    | t::l -> 
	let e = dd#createElement "a" in 
	  e#setAttribute "href" (utf8_decode (format_link t));
	  e#setAttribute "wiki_href" (utf8_decode t) ; 
	  e#appendChild (textl l) ;  
	  e
    | [] -> 
	let e = dd#createElement "a" in 
	  e#setAttribute "href" "explain_missing_link";
	  e#setAttribute "wiki_href" "missing_link" ; 
	  e#appendChild (text "missing link") ;  
	  e

let external_link l =
  match l with 
      t::[] -> 
	let e = dd#createElement "a" in 
	  e#setAttribute "href" (utf8_decode (format_link t));
	  e#setAttribute "wiki_href" (utf8_decode t) ; 
	  e#appendChild (text t) ;  
	  e 
    | t::l -> 
	let e = dd#createElement "a" in 
	  e#setAttribute "href" (utf8_decode (format_link t));
	  e#setAttribute "wiki_href" (utf8_decode t) ; 
	  e#appendChild (textl l) ;  
	  e
    | [] -> 
	let e = dd#createElement "a" in 
	  e#setAttribute "href" "explain_missing_link";
	  e#setAttribute "wiki_href" "missing_link" ; 
	  e#appendChild (text "missing link") ;  
	  e
	
  
let paragraph () =
  dd#createElement "p"
