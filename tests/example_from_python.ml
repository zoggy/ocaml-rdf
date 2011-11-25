let fatal msg = prerr_endline msg; exit 1;;

let world = Rdf_init.new_world () ;;
Rdf_init.world_open world;;

(*
storage=RDF.Storage(storage_name="hashes",
                    name="test",
                    options_string="new='yes',hash-type='memory',dir='.'")
if storage is None:
  raise Exception("new RDF.Storage failed")

*)

let storage =
  match Rdf_storage.new_storage
    ~factory: "hashes" ~name: "test"
    ~options: "new='yes',hash-type='memory',dir='.'" world
  with
    None -> fatal "failed to create storage"
  | Some s -> s
;;

(*
model=RDF.Model(storage)
if model is None:
  raise Exception("new RDF.model failed")
*)

let model =
  match Rdf_model.new_model world storage with
    None -> fatal "Failed to create model"
  | Some m -> m
;;

(*
statement=RDF.Statement(RDF.Uri("http://www.dajobe.org/"),
                        RDF.Uri("http://purl.org/dc/elements/1.1/creator"),
                        RDF.Node("Dave Beckett"))
if statement is None:
  raise Exception("new RDF.Statement failed")
*)
let unopt = function None -> fatal "None" | Some x -> x;;
let statement =
  match Rdf_statement.new_statement_from_nodes world
    (unopt (Rdf_node.new_node_from_uri_string world "http://www.dajobe.org/"))
    (unopt (Rdf_node.new_node_from_uri_string world "http://purl.org/dc/elements/1.1/creator"))
    (unopt (Rdf_node.new_node_from_literal world "Dave Beckett"))
  with
    None -> fatal "Failed to create statement"
  | Some s -> s
;;



(*
model.add_statement(statement)

# Match against an empty statement - find everything
for s in model.find_statements(RDF.Statement()):
  print "found statement:",s

test_file='../data/dc.rdf'

print "Parsing URI (file)", test_file
uri=RDF.Uri(string="file:"+test_file)

parser=RDF.Parser('raptor')
if parser is None:
  raise Exception("Failed to create RDF.Parser raptor")

count=0
for s in parser.parse_as_stream(uri,uri):
  model.add_statement(s)
  count=count+1

print "Parsing added",count,"statements"

print "Printing all statements"
for s in model.as_stream():
  print "Statement:",s

q = RDF.Query("SELECT ?a ?c WHERE (?a dc:title ?c) USING dc FOR <http://purl.org/dc/elements/1.1/>")
print "Querying for dc:titles:"
for result in q.execute(model):
  print "{"
  for k in result:
    print "  "+k+" = "+str(result[k])
  print "}"

print "Writing model to test-out.rdf as rdf/xml"

# Use any rdf/xml parser that is available
serializer=RDF.Serializer()
serializer.set_namespace("dc", RDF.Uri("http://purl.org/dc/elements/1.1/"))
serializer.serialize_model_to_file("test-out.rdf", model)

print "Serialized to ntriples as a string size",len(model.to_string(name="ntriples", base_uri="http://example.org/base#")),"bytes"

print "Done"
*)