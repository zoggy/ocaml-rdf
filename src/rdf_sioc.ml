
let sioc_str = "http://rdfs.org/sioc/ns#";;
let sioc = Iri.of_string sioc_str ;;
let sioc_ s = Iri.of_string (sioc_str ^ s);;

let community = sioc_ "Community" ;;
let container = sioc_ "Container" ;;
let forum = sioc_ "Forum" ;;
let item = sioc_ "Item" ;;
let post = sioc_ "Post" ;;
let role = sioc_ "Role" ;;
let site = sioc_ "Site" ;;
let space = sioc_ "Space" ;;
let thread = sioc_ "Thread" ;;
let c_User = sioc_ "User" ;;
let userAccount = sioc_ "UserAccount" ;;
let usergroup = sioc_ "Usergroup" ;;

module Open = struct
  let sioc_community = community
  let sioc_container = container
  let sioc_forum = forum
  let sioc_item = item
  let sioc_post = post
  let sioc_role = role
  let sioc_site = site
  let sioc_space = space
  let sioc_thread = thread
  let sioc_c_User = c_User
  let sioc_userAccount = userAccount
  let sioc_usergroup = usergroup
end

class from ?sub g =
  let sub = match sub with None -> g.Rdf_graph.name() | Some iri -> iri in
  let sub = Rdf_term.Iri sub in
  object
  method community = Rdf_graph.iri_objects_of g ~sub ~pred: community
  method container = Rdf_graph.iri_objects_of g ~sub ~pred: container
  method forum = Rdf_graph.iri_objects_of g ~sub ~pred: forum
  method item = Rdf_graph.iri_objects_of g ~sub ~pred: item
  method post = Rdf_graph.iri_objects_of g ~sub ~pred: post
  method role = Rdf_graph.iri_objects_of g ~sub ~pred: role
  method site = Rdf_graph.iri_objects_of g ~sub ~pred: site
  method space = Rdf_graph.iri_objects_of g ~sub ~pred: space
  method thread = Rdf_graph.iri_objects_of g ~sub ~pred: thread
  method userAccount = Rdf_graph.iri_objects_of g ~sub ~pred: userAccount
  method usergroup = Rdf_graph.iri_objects_of g ~sub ~pred: usergroup
  end
