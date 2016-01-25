
let sha1 str =
  let hash = Cryptokit.Hash.sha1 () in
  hash#add_string str ;
  let t = Cryptokit.Hexa.encode () in
  t#put_string hash#result ;
  t#get_string

let () = Rdf_stubs.sha1 := sha1

let sha256 str =
  let hash = Cryptokit.Hash.sha256 () in
  hash#add_string str ;
  let t = Cryptokit.Hexa.encode () in
  t#put_string hash#result ;
  t#get_string

let () = Rdf_stubs.sha256 := sha256
