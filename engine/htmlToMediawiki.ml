(*
 *
 *  This file is part of Mediawiki-OCaml-Js
 *
 *  Mediawiki-OCaml-Js is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  Mediawiki-OCaml-Js is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with Mediawiki-OCaml-Js. If not, see <http://www.gnu.org/licenses/>.
 *
 *  htmlToMediawiki.ml 03/02/2010 15:55
 *  
 *  { william.le-ferrand@polytechnique.edu; william@myrilion.com } 
 *
 *)

open Printf 

open Misc
open Helper
open Tools
open Errors 

open Aiuto

(* Straight recursion is not enough *)
let rec process_internal (e: Dom.element) hlp = 
  
  let tgname = e#_get_tagName in 

    if Ocamljs.is_null tgname then
      begin
	(Obj.magic e)#_get_data (* Dirty hack *)
      end
    else 
      begin
	match tgname with 
	    "P" ->      
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n hlp)) "" (e#_get_childNodes) in
	      sprintf "%s\r\n\r\n" c 

	  | "I" -> 	  
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n hlp)) "" (e#_get_childNodes) in
	      sprintf "''%s''" c 

	  | "B" -> 	  
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n hlp)) "" (e#_get_childNodes) in
	      sprintf "'''%s'''" c 

	  | "A" ->
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n hlp)) "" (e#_get_childNodes) in
	      let kind = e#getAttribute "kind" in 
	      let wiki_href = e#getAttribute "wiki_href" in (* Can be null .. *)

		if kind = "internal" then
		  sprintf "[[%s|%s]]" wiki_href c
		else 
		  sprintf "[%s %s]" wiki_href c
			
	  | "UL" -> 
	      let nhlp = { depth = hlp.depth + 1; ordered = false } in
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n nhlp)) "" (e#_get_childNodes) in
		sprintf "%s" c

	  | "OL" -> 
	      let nhlp = { depth = hlp.depth + 1; ordered = true } in
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n nhlp)) "" (e#_get_childNodes) in
		sprintf "%s" c
	      

	  | "LI" -> 
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n hlp)) "" (e#_get_childNodes) in
		sprintf "%s%s\r\n" (String.make hlp.depth (if hlp.ordered then '#' else '*')) c
		
		
	  | "H2" -> 
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n hlp)) "" (e#_get_childNodes) in
	      sprintf "== %s ==\r\n" c

	  | "BR" -> 
	      "\r\n"

	  | "SPAN" -> 
	      let kind = e#getAttribute "kind" in
	      let c = Array.fold_left (fun acc n -> acc ^ (process_internal n hlp)) "" (e#_get_childNodes) in
		sprintf "<%s>%s</%s>" kind c kind

	  | _ as a -> 
	      Array.fold_left (fun acc n -> acc ^ (process_internal n hlp)) "" (e#_get_childNodes) 
		
      end

let process e = 
  process_internal e (Aiuto.create ())
