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
 *  reporting.ml 03/02/2010 15:55
 *  
 *  { william.le-ferrand@polytechnique.edu; william@myrilion.com } 
 *
 *)

open Misc
open Helper

let encode = Javascript.encodeURIComponent 

let alert_internal article msg = 
  let hdl = Dom.new_XMLHttpRequest () in 
  let msg = Printf.sprintf "article=%s&msg=%s" (encode article) (encode msg) in
    
    hdl#open_ "POST" "report" true ;
    hdl#setRequestHeader "Content-Type" "application/x-www-form-urlencoded"; 
    hdl#setRequestHeader "Content-Length" (Obj.magic (String.length msg)); 
    hdl#send (msg) 
  
let alert hlp fmt = 
  Printf.ksprintf (fun s -> alert_internal hlp.article s) fmt

