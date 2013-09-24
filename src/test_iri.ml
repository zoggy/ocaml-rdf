let s = "http://éxample.net/coucou";;
let iri = Rdf_iri.iri s;;

let urn = "urn://uuid:2f302fb5-642e-4d3b-af19-29a8f6d894c9"
let iri_urn = Rdf_iri.iri urn;;

let pct = "http://foo.bar/%66o%61%66.net";;
let iri = Rdf_iri.iri pct ;;
print_endline (Rdf_iri.string iri);;

List.iter prerr_endline (Rdf_iri.path iri);;

let s2 = "Http://foo@example.net/ZazÀÖØöø˿Ͱͽ΄῾‌‍⁰↉Ⰰ⿕、ퟻ﨎ﷇﷰ￯";;
let iri2 = Rdf_iri.iri s2;;
prerr_endline (Printf.sprintf "user(iri2)=%s" (Rdf_misc.string_of_opt (Rdf_iri.user iri2)));;

let uri2 = Rdf_iri.to_uri iri2;;
let iri3 = Rdf_iri.of_uri uri2;;

prerr_endline
  (Printf.sprintf "iri2=%s\nuri2=%s\niri3=%s"
    (Rdf_iri.string iri2) (Rdf_uri.string uri2) (Rdf_iri.string iri3));;

