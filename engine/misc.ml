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
 *  misc.ml 03/02/2010 15:55
 *  
 *  { william.le-ferrand@polytechnique.edu; william@myrilion.com } 
 *
 *)

(** Warning *)
(** This operator seems to mislead the compiler .. *)

open Dom

let (>>>) f g = g f 

external alert : 'a -> unit = "@alert"

(* let dd = Dom.document    *)

let dd = Javascript.eval "window.frames[0].document" 
