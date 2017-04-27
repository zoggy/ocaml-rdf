
let acl_str = "http://www.w3.org/ns/auth/acl#";;
let acl = Iri.of_string acl_str ;;
let acl_ s = Iri.of_string (acl_str ^ s);;

let c_Access = acl_ "Access" ;;
let c_Append = acl_ "Append" ;;
let c_Authorization = acl_ "Authorization" ;;
let c_Control = acl_ "Control" ;;
let c_Read = acl_ "Read" ;;
let c_Write = acl_ "Write" ;;
let accessControl = acl_ "accessControl" ;;
let accessTo = acl_ "accessTo" ;;
let accessToClass = acl_ "accessToClass" ;;
let agent = acl_ "agent" ;;
let agentClass = acl_ "agentClass" ;;
let agentGroup = acl_ "agentGroup" ;;
let defaultForNew = acl_ "defaultForNew" ;;
let delegates = acl_ "delegates" ;;
let mode = acl_ "mode" ;;
let owner = acl_ "owner" ;;

module Open = struct
  let acl_c_Access = c_Access
  let acl_c_Append = c_Append
  let acl_c_Authorization = c_Authorization
  let acl_c_Control = c_Control
  let acl_c_Read = c_Read
  let acl_c_Write = c_Write
  let acl_accessControl = accessControl
  let acl_accessTo = accessTo
  let acl_accessToClass = accessToClass
  let acl_agent = agent
  let acl_agentClass = agentClass
  let acl_agentGroup = agentGroup
  let acl_defaultForNew = defaultForNew
  let acl_delegates = delegates
  let acl_mode = mode
  let acl_owner = owner
end

class from ?sub g =
  let sub = match sub with None -> g.Rdf_graph.name() | Some iri -> iri in
  let sub = Rdf_term.Iri sub in
  object(self)
  method accessControl = Rdf_graph.iri_objects_of g ~sub ~pred: accessControl
  method accessControl_opt = match self#accessControl with [] -> None | x::_ -> Some x
  method accessTo = Rdf_graph.iri_objects_of g ~sub ~pred: accessTo
  method accessTo_opt = match self#accessTo with [] -> None | x::_ -> Some x
  method accessToClass = Rdf_graph.iri_objects_of g ~sub ~pred: accessToClass
  method accessToClass_opt = match self#accessToClass with [] -> None | x::_ -> Some x
  method agent = Rdf_graph.iri_objects_of g ~sub ~pred: agent
  method agent_opt = match self#agent with [] -> None | x::_ -> Some x
  method agentClass = Rdf_graph.iri_objects_of g ~sub ~pred: agentClass
  method agentClass_opt = match self#agentClass with [] -> None | x::_ -> Some x
  method agentGroup = Rdf_graph.iri_objects_of g ~sub ~pred: agentGroup
  method agentGroup_opt = match self#agentGroup with [] -> None | x::_ -> Some x
  method defaultForNew = Rdf_graph.iri_objects_of g ~sub ~pred: defaultForNew
  method defaultForNew_opt = match self#defaultForNew with [] -> None | x::_ -> Some x
  method delegates = Rdf_graph.iri_objects_of g ~sub ~pred: delegates
  method delegates_opt = match self#delegates with [] -> None | x::_ -> Some x
  method mode = Rdf_graph.iri_objects_of g ~sub ~pred: mode
  method mode_opt = match self#mode with [] -> None | x::_ -> Some x
  method owner = Rdf_graph.iri_objects_of g ~sub ~pred: owner
  method owner_opt = match self#owner with [] -> None | x::_ -> Some x
  end
