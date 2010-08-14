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
 *  msg.ml 03/02/2010 15:55
 *  
 *  { william.le-ferrand@polytechnique.edu; william@myrilion.com } 
 *
 *)

let rec rule1 = lexer 
  | eof -> ""
  | ['0'-'9']+ -> let ch = Ulexing.utf8_lexeme lexbuf in ch ^ (rule1 lexbuf)
;;

let _ = 
  let lexbuf = Ulexing.from_utf8_string "0121212" in
  let result = rule1 lexbuf in 
  let cpls = Printf.sprintf "alert ('%s');" result in
  Javascript.eval cpls
