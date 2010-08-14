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
 *  tools.ml 03/02/2010 15:55
 *  
 *  { william.le-ferrand@polytechnique.edu; william@myrilion.com } 
 *
 *)

let count c s =
  let n = ref 0 in
    for i = 0 to String.length s - 1 do if s.[i] = c then incr n done;
    !n

external format_link : string -> string = ".replace (/ /g, '_')"
external clean_attr1 : string -> string = ".replace (/\"/g, '')"
external clean_attr2 : string -> string = ".replace (/ /g, '')"


let clean_attrs attr = 
  clean_attr2 (clean_attr1 attr)
        
let split_attrs attr = 
  let attr = clean_attrs attr in
  let l = String.length attr in
  let i = ref 0 in 
    while (attr.[!i] <> '=') do incr i done ; 
    String.sub attr 0 !i, 
  String.sub attr (!i+1) (l - !i - 1)
      
