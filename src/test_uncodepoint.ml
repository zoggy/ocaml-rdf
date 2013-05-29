
if Array.length Sys.argv < 2 then
  (
   prerr_endline ("Usage: "^Sys.argv.(0)^" <utf8 string>");
   exit 1
  );;

let s = Rdf_sparql_lex.unescape_codepoints Sys.argv.(1);;
Printf.printf "Input: %s\nOutput: %s\n" Sys.argv.(1) s;;
exit 0;;