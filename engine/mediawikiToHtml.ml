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
 *  mediawikiToHtml.ml 03/02/2010 15:55
 *  
 *  { william.le-ferrand@polytechnique.edu; william@myrilion.com } 
 *
 *)

open Misc
open Helper
open Tools
open Errors 

exception Done 
exception ParagraphDone

let regexp number = [ '0'-'9' ]+ ;;

let regexp line_break = '\n' | '\r' | "\r\n" ;;

let regexp not_line_break = [^ '\n' '\r'] ;;

let regexp white_space = [ ' ' '\t' ] ;;

let regexp heading_chars = [^ '\n' '\r' '=' ] ;;

let regexp utf8_quotes = [ 8000 - 9000 ] ;;

let regexp standard_char_old =  [^ '\n' '\r' 8220 8221 ];;

let regexp standard_char =  [^ '\n' '\r' '<' '[' '\'' '{' '}' '|'] ;;

let regexp special_char =  [^ '\n' '\r' '}' ] ;;

let regexp xml_attr_label = [^ '\n' '\r' ' ' '>' '='] ;;

let regexp xml_chars = [^ '>' ] ;;
let regexp xml_attr_val_chars = [^ '>' '<' ' ' ] ;;
let regexp xml_attr_label_chars = [^ '>' '<' ' ' '=' ] ;;

let regexp internal_link_chars = [^ ']' '|'] ;;
let regexp external_link_chars = [^ ']' ' '] ;;

let regexp straight = [^ '\''] ;;

let regexp col_headings_chars = [^ '\r' '\n' '!' ];;

let regexp attr_chars = [^ '\r' '\n' '=' '\t' ' ' '|'];;

let rec lex_source hlp = lexer 
  | line_break -> 
      ()

  | white_space * ("=" | "==" | "===" | "====" | "=====" | "======") ->   
      
	let d = count '=' (Ulexing.utf8_lexeme lexbuf) in
	let e = Builder.heading d in
	  lex_heading (Helper.dup hlp e) lexbuf ; 
	  Helper.append hlp.mbuf e ;
	  lex_source hlp lexbuf

  | white_space * '*' -> 
      let e = Builder.ul () in 
      let fi = Builder.li () in
	Helper.append hlp.mbuf e ;
	Helper.append e fi ;
      	lex_line (Helper.dup hlp fi) lexbuf ;
	(* wtf? imperative style *)
	let step = ref (lex_ulist (Helper.dup hlp e) 1 lexbuf) in
	  while !step = 0 do
	    let fi = Builder.li () in 
	      Helper.append e fi ;
	      lex_line (Helper.dup hlp fi) lexbuf; 
	      step := lex_ulist (Helper.dup hlp e) 1 lexbuf ; 
	  done ;
	lex_source hlp lexbuf 

  | white_space * '#' -> 
      let e = Builder.ol () in 
      let fi = Builder.li () in
	Helper.append hlp.mbuf e ;
	Helper.append e fi ;
      	lex_line (Helper.dup hlp fi) lexbuf ;
	(* yeah, finally refs are cool *)
	let step = ref (lex_olist (Helper.dup hlp e) 1 lexbuf) in
	  while !step = 0 do
	    let fi = Builder.li () in 
	      Helper.append e fi ;
	      lex_line (Helper.dup hlp fi) lexbuf; 
	      step := lex_olist (Helper.dup hlp e) 1 lexbuf ; 
	  done ;
	  lex_source hlp lexbuf 
      
  | white_space * "{|" ->
      alert "lex table" ; 
      let e = Builder.table () in 
	Helper.append hlp.mbuf e; 
	lex_table_attrs (Helper.dup hlp e) lexbuf ; 
	alert "attr returned"; 
	lex_table (Helper.dup hlp e) lexbuf ; 
	lex_source hlp lexbuf 
 
  | white_space *  ->       
      alert "lex paragraph" ; 
      Ulexing.rollback lexbuf ;
      let e = Builder.paragraph () in
	lex_line (Helper.dup hlp e) lexbuf ;
	Helper.append hlp.mbuf e
	  

  | "" -> raise Done 
      
  

and lex_table_attrs hlp = lexer 
  | white_space * attr_chars + white_space * '=' white_space * attr_chars+  ->
      alert (Ulexing.utf8_lexeme lexbuf);
      let key, value = split_attrs (Ulexing.utf8_lexeme lexbuf) in
	set_attribute hlp.mbuf key value; 
	lex_table_attrs hlp lexbuf

  | white_space * line_break -> 
        alert "ended attrs on br" ; 

  | white_space * ('|' | '!') -> 
      alert "ended attrs on pipe" ; 
      ()   

and lex_col1 hlp = lexer 
  | "||" -> 
      () 

  | "|}" -> 
      alert "end in lex_col1"; 
      Ulexing.rollback lexbuf 
            
  | "|-" -> 
      Ulexing.rollback lexbuf

  | "|" -> 
      alert "rolling back on new row"  ;
      Ulexing.rollback lexbuf

  | "" -> 
      alert "about to lex the text";
      match (try lex_source hlp lexbuf ; true with _ -> false) with 
	  true -> 
	    lex_col1 hlp lexbuf
	| false -> () 

and lex_row1 hlp = lexer 
  | "|}" -> 
      alert "end in lex_row1" ;
      Ulexing.rollback lexbuf; () 
	
  | "" -> 
      let e = Builder.td () in 
	Helper.append hlp.mbuf e ; 
	lex_col1 (Helper.dup hlp e) lexbuf ;
	lex_row1 hlp lexbuf 
	  
  | '!' white_space * attr_chars + white_space * '=' white_space * attr_chars+ -> 
      
         alert (Ulexing.utf8_lexeme lexbuf); 
      let e = Builder.th () in
	Helper.append hlp.mbuf e ; 
	let nhlp = Helper.dup hlp e in
	  alert "time to fetch ! attrs" ; 
	  lex_table_attrs nhlp lexbuf ;
	  alert "attrs found";
	  lex_col1 nhlp lexbuf ; 
	  lex_row1 hlp lexbuf 

  | '!' -> 
      alert "simple !"; 
      let e = Builder.th () in
	Helper.append hlp.mbuf e ; 
	lex_col1 (Helper.dup hlp e) lexbuf ;
	lex_row1 hlp lexbuf
      	  
  | '|' white_space * attr_chars + white_space * '=' white_space * attr_chars+ -> 
         alert (Ulexing.utf8_lexeme lexbuf); 
      
      let e = Builder.td () in
	Helper.append hlp.mbuf e ; 
	let nhlp = Helper.dup hlp e in
	  lex_table_attrs nhlp lexbuf ;
	  alert "start of col" ; 
	  lex_col1 nhlp lexbuf ; 
	  alert "end of col"; 
	  lex_row1 hlp lexbuf 

  | '|' -> 
      alert "simple |"; 
      let e = Builder.td () in
	Helper.append hlp.mbuf e ; 
	alert "start of col" ; 
	lex_col1 (Helper.dup hlp e) lexbuf ;
	alert "end of col"; 
	lex_row1 hlp lexbuf

  | "|-" -> 
      Ulexing.rollback lexbuf 
	  
and lex_col_headings hlp = lexer 
 | "!!" -> 
     lex_col_headings hlp lexbuf 

 | col_headings_chars+ -> 
     let t = Builder.text (Ulexing.utf8_lexeme lexbuf) in
     let e = Builder.th () in
       Helper.append e t ; 
       Helper.append hlp.mbuf e ;
       lex_col_headings hlp lexbuf 
     
 | line_break -> () 

and lex_table hlp = lexer 
  | "|}" ->
      alert "end in lex_table" 

  | "|+" not_line_break* line_break -> 
      lex_table hlp lexbuf 

  | '!' -> 
      let e = Builder.tr () in 
	Helper.append hlp.mbuf e ; 
	lex_col_headings (Helper.dup hlp e) lexbuf ; 
	lex_table hlp lexbuf 
      
  | "|-" -> 
      alert "new_line"; 
      let e = Builder.tr () in 
	Helper.append hlp.mbuf e ; 
	let nhlp = Helper.dup hlp e in
	  lex_table_attrs nhlp lexbuf ; 
	  alert "tr attrs caught" ; 
	  lex_row1 nhlp lexbuf ; 
	  alert "row ended!"; 
	  lex_table hlp lexbuf 

  | '|' white_space * line_break ->
      let e = Builder.tr () in 
	Helper.append hlp.mbuf e ; 
	lex_row1 (Helper.dup hlp e) lexbuf ; 
	lex_table hlp lexbuf 
	  

and lex_ulist hlp cd = lexer 
  | white_space * '*'+ -> 
      let nd = count '*' (Ulexing.utf8_lexeme lexbuf) in
	if nd < cd then 
	  begin
	    cd - nd 
	  end
	else if (nd = cd) then 
	  begin
	    let fi = Builder.li () in 
	      Helper.append hlp.mbuf fi ;
	      lex_line (Helper.dup hlp fi) lexbuf ;  
	      lex_ulist hlp cd lexbuf 
	  end
	else if (nd = cd + 1) then
	  begin
	    let e = Builder.ul () in 
	    let fi = Builder.li () in
	      Helper.append hlp.mbuf e ;
	      Helper.append e fi ;
	      lex_line (Helper.dup hlp fi) lexbuf; 
	      let step = ref (lex_ulist (Helper.dup hlp e) nd lexbuf) in
		while !step = 0 do
		  let fi = Builder.li () in 
		    Helper.append e fi ;
		    lex_line (Helper.dup hlp fi) lexbuf; 
		    step := lex_ulist (Helper.dup hlp e) nd lexbuf ; 
		done ; 
		(!step - 1) 
	  end
	else 
	  begin
	    Reporting.alert hlp "List structure error nd:%d cd:%d" nd cd; 
	    0
	  end
	  
  | "" -> -1

and lex_olist hlp cd = lexer 
  | white_space * '#'+ -> 
      let nd = count '#' (Ulexing.utf8_lexeme lexbuf) in
	if nd < cd then 
	  begin
	    cd - nd 
	  end
	else if (nd = cd) then 
	  begin
	    let fi = Builder.li () in 
	      Helper.append hlp.mbuf fi ;
	      lex_line (Helper.dup hlp fi) lexbuf ;  
	      lex_olist hlp cd lexbuf 
	  end
	else if (nd = cd + 1) then
	  begin
	    let e = Builder.ol () in 
	    let fi = Builder.li () in
	      Helper.append hlp.mbuf e ;
	      Helper.append e fi ;
	      lex_line (Helper.dup hlp fi) lexbuf; 
	      let step = ref (lex_olist (Helper.dup hlp e) nd lexbuf) in
		while !step = 0 do
		  let fi = Builder.li () in 
		    Helper.append e fi ;
		    lex_line (Helper.dup hlp fi) lexbuf; 
		    step := lex_olist (Helper.dup hlp e) nd lexbuf ; 
		done ; 
		(!step - 1) 
	  end
	else 
	  begin
	    Reporting.alert hlp "List structure error nd:%d cd:%d" nd cd; 
	    0
	  end
	  
  | "" -> -1
      

	
	  
	

      
and lex_heading hlp = lexer 
 | heading_chars* -> 
     let t = Builder.text (Ulexing.utf8_lexeme lexbuf) in
       Helper.append hlp.mbuf t ; 
       lex_heading hlp lexbuf

 | ("=" | "==" | "===" | "====" | "=====" | "======") -> () 

and lex_quoted = lexer 
 | standard_char+ -> 
     Builder.text (Ulexing.utf8_lexeme lexbuf) :: lex_quoted lexbuf 

 | [ 8221 ] -> []

and lex_xml_val = lexer 
  xml_attr_val_chars+ -> 
    Ulexing.utf8_lexeme lexbuf

and remove_eq = lexer 
  white_space* '=' white_space* -> () 

and lex_xml_attrs hlp = lexer 
  white_space* '>' ->
    () 

| white_space* -> 
    lex_xml_attrs hlp lexbuf 

| xml_attr_label_chars* -> 
    let l = Ulexing.utf8_lexeme lexbuf in
      remove_eq lexbuf ; 
      let v = lex_xml_val lexbuf in
	Helper.set_attribute hlp.mbuf l v ; 
	lex_xml_attrs hlp lexbuf 
	
and lex_internal_link = lexer
| internal_link_chars+ -> 
    Ulexing.utf8_lexeme lexbuf :: lex_internal_link lexbuf 

| '|' ->
    lex_internal_link lexbuf 

| "]]" -> 
    [] 

and lex_external_link = lexer
| external_link_chars+ -> 
    Ulexing.utf8_lexeme lexbuf :: lex_external_link lexbuf 

| ' ' ->
    lex_external_link lexbuf 

| ']' -> 
    [] 


and lex_style hlp = lexer 
 | straight* -> 
     let t = Builder.text (Ulexing.utf8_lexeme lexbuf) in
       Helper.append hlp.mbuf t ; 
       lex_style hlp lexbuf
 | '\''* -> ()
	 
and lex_special hlp = lexer 
 | "}}" -> 
     () 
 | special_char+ ->      
     let e = Builder.text ("{{" ^ (Ulexing.utf8_lexeme lexbuf) ^ "}}") in 
       Helper.append hlp.mbuf e ; 
       lex_special hlp lexbuf
     
 
and lex_nowiki hlp = lexer
| "</nowiki>" -> () 
| _ -> 
    let e = Builder.text (Ulexing.utf8_lexeme lexbuf) in 
      Helper.append hlp.mbuf e ; 
      lex_nowiki hlp lexbuf

and lex_line hlp = lexer 
 | line_break -> 
     ()

 | "</ref>" -> ()

 | "</b>" -> () (* Not semantically correct .. *)

 | "</code>" -> () (* Not that great .. *)

 | "</i>" -> () 
 | "<ref" -> 
     let e = Builder.reference () in
     let ehlp = Helper.dup hlp e in
       lex_xml_attrs ehlp lexbuf ; 
       lex_line ehlp lexbuf ;
       Helper.append hlp.mbuf e ;
       lex_line hlp lexbuf

 | "<nowiki>" -> 
     let e = Builder.nowiki () in 
       Helper.append hlp.mbuf e ; 
       
       lex_nowiki (Helper.dup hlp e) lexbuf ;
       lex_line hlp lexbuf 

 | "<code" xml_chars* '>' ->      
     let e = Builder.code () in
       Helper.append hlp.mbuf e; 
       lex_line (Helper.dup hlp e) lexbuf ; 
       lex_line hlp lexbuf 

 | "<b>" ->
     let e = Builder.b () in
       Helper.append hlp.mbuf e ; 
       lex_line (Helper.dup hlp e) lexbuf 

 | "<i>" ->
     let e = Builder.i () in
       Helper.append hlp.mbuf e ; 
       lex_line (Helper.dup hlp e) lexbuf 

 | standard_char+ -> 
     let e = Builder.text (Ulexing.utf8_lexeme lexbuf) in 
       Helper.append hlp.mbuf e ; 
       lex_line hlp lexbuf

 | "[[" -> 
     let l = lex_internal_link lexbuf in   
     let e = Builder.internal_link l in 
       Helper.append hlp.mbuf e ;
       Helper.set_attribute e "kind" "internal"; 
       lex_line hlp lexbuf 

 | "'''" ->     
     let e = Builder.b () in 
       Helper.append hlp.mbuf e ; 
       lex_style (Helper.dup hlp e) lexbuf ; 
       lex_line hlp lexbuf

 | "''" ->
     let e = Builder.i () in 
       Helper.append hlp.mbuf e ; 
       lex_style (Helper.dup hlp e) lexbuf ; 
       lex_line hlp lexbuf

 | "'" -> 
     let t = Builder.text "'" in 
       Helper.append hlp.mbuf t ; 
       lex_line hlp lexbuf 
     
 | "[" -> 
     let l = lex_external_link lexbuf in 
     let e = Builder.internal_link l in 
       Helper.set_attribute e "kind" "external"; 
       Helper.append hlp.mbuf e ; 
       lex_line hlp lexbuf 

 | "{{" -> 
     alert "lexing special" ;
     lex_special hlp lexbuf ; 
     lex_line hlp lexbuf 

 | '{' -> 
     let t = Builder.text "{" in 
       Helper.append hlp.mbuf t ; 
       lex_line hlp lexbuf 

 | '|' -> 
     alert "pipe" 

 | "" -> raise ParagraphDone




let process (buf: Dom.element) origin article source = 
  let helper = create buf origin article in 
  let lexbuf = Ulexing.from_utf8_string source in 
    try 
      while true do 
	lex_source helper lexbuf 
	  
      done 
    with Done -> alert "Done signal caught" 
      | ParagraphDone -> alert "paragraph done; I hope it's the end .. "
      | _ as e -> alert "error during lexing .." ; raise e
