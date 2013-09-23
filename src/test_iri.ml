let s = "http://éxample.net/coucou";;
let iri = Rdf_iri.iri s;;

let urn = "urn://uuid:2f302fb5-642e-4d3b-af19-29a8f6d894c9"
let iri_urn = Rdf_iri.iri urn;;

let pct = "http://foo.bar/%66o%61%66.net";;
let iri = Rdf_iri.iri pct ;;
print_endline (Rdf_iri.string iri);;

List.iter prerr_endline (Rdf_iri.path iri);;

let s2 = "http://foo@example.net/ZazÀÖØöø˿Ͱͽ΄῾‌‍⁰↉Ⰰ⿕、ퟻ﨎ﷇﷰ￯";;
let iri2 = Rdf_iri.iri s2;;


