
let pim_str = "http://www.w3.org/ns/pim/space#";;
let pim = Iri.of_string pim_str ;;
let pim_ s = Iri.of_string (pim_str ^ s);;

let c_ConfigurationFile = pim_ "ConfigurationFile" ;;
let c_ControlledStorage = pim_ "ControlledStorage" ;;
let c_MasterWorkspace = pim_ "MasterWorkspace" ;;
let c_PersonalStorage = pim_ "PersonalStorage" ;;
let c_PreferencesWorkspace = pim_ "PreferencesWorkspace" ;;
let c_PrivateWorkspace = pim_ "PrivateWorkspace" ;;
let c_PublicStorage = pim_ "PublicStorage" ;;
let c_PublicWorkspace = pim_ "PublicWorkspace" ;;
let c_SharedWorkspace = pim_ "SharedWorkspace" ;;
let c_Storage = pim_ "Storage" ;;
let c_Workspace = pim_ "Workspace" ;;
let masterWorkspace = pim_ "masterWorkspace" ;;
let preferencesFile = pim_ "preferencesFile" ;;
let storage = pim_ "storage" ;;
let uriPrefix = pim_ "uriPrefix" ;;
let workspace = pim_ "workspace" ;;

module Open = struct
  let pim_c_ConfigurationFile = c_ConfigurationFile
  let pim_c_ControlledStorage = c_ControlledStorage
  let pim_c_MasterWorkspace = c_MasterWorkspace
  let pim_c_PersonalStorage = c_PersonalStorage
  let pim_c_PreferencesWorkspace = c_PreferencesWorkspace
  let pim_c_PrivateWorkspace = c_PrivateWorkspace
  let pim_c_PublicStorage = c_PublicStorage
  let pim_c_PublicWorkspace = c_PublicWorkspace
  let pim_c_SharedWorkspace = c_SharedWorkspace
  let pim_c_Storage = c_Storage
  let pim_c_Workspace = c_Workspace
  let pim_masterWorkspace = masterWorkspace
  let pim_preferencesFile = preferencesFile
  let pim_storage = storage
  let pim_uriPrefix = uriPrefix
  let pim_workspace = workspace
end

class from ?sub g =
  let sub = match sub with None -> g.Rdf_graph.name() | Some iri -> iri in
  let sub = Rdf_term.Iri sub in
  object(self)
  method masterWorkspace = g.Rdf_graph.objects_of ~sub ~pred: masterWorkspace
  method masterWorkspace_opt = match self#masterWorkspace with [] -> None | x::_ -> Some x
  method masterWorkspace_iris = Rdf_graph.only_iris (self#masterWorkspace)
  method masterWorkspace_opt_iri = match self#masterWorkspace_iris with [] -> None | x::_ -> Some x
  method preferencesFile = g.Rdf_graph.objects_of ~sub ~pred: preferencesFile
  method preferencesFile_opt = match self#preferencesFile with [] -> None | x::_ -> Some x
  method preferencesFile_iris = Rdf_graph.only_iris (self#preferencesFile)
  method preferencesFile_opt_iri = match self#preferencesFile_iris with [] -> None | x::_ -> Some x
  method storage = g.Rdf_graph.objects_of ~sub ~pred: storage
  method storage_opt = match self#storage with [] -> None | x::_ -> Some x
  method storage_iris = Rdf_graph.only_iris (self#storage)
  method storage_opt_iri = match self#storage_iris with [] -> None | x::_ -> Some x
  method uriPrefix = g.Rdf_graph.objects_of ~sub ~pred: uriPrefix
  method uriPrefix_opt = match self#uriPrefix with [] -> None | x::_ -> Some x
  method uriPrefix_iris = Rdf_graph.only_iris (self#uriPrefix)
  method uriPrefix_opt_iri = match self#uriPrefix_iris with [] -> None | x::_ -> Some x
  method workspace = g.Rdf_graph.objects_of ~sub ~pred: workspace
  method workspace_opt = match self#workspace with [] -> None | x::_ -> Some x
  method workspace_iris = Rdf_graph.only_iris (self#workspace)
  method workspace_opt_iri = match self#workspace_iris with [] -> None | x::_ -> Some x
  end
