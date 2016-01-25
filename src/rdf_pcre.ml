
(** *)

let pcre_replace ~flags ~pat ~templ s =
  let rex = Pcre.regexp ~flags pat in
  Pcre.replace ~rex ~templ s

let () = Rdf_stubs.pcre_replace := pcre_replace

let pcre_pmatch ~flags ~pat s =
  let rex = Pcre.regexp ~flags pat in
  Pcre.pmatch ~rex s

let () = Rdf_stubs.pcre_pmatch := pcre_pmatch