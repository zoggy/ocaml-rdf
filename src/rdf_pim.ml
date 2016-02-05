
let pim_str = "http://www.w3.org/ns/pim/space#";;
let pim = Iri.of_string pim_str ;;
let pim_ s = Iri.of_string (pim_str ^ s);;

let c_ControlledStorage = pim_ "ControlledStorage" ;;
let masterWorkspace = pim_ "masterWorkspace" ;;
let c_PersonalStorage = pim_ "PersonalStorage" ;;
let preferencesFile = pim_ "preferencesFile" ;;
let c_PublicStorage = pim_ "PublicStorage" ;;
let c_Storage = pim_ "Storage" ;;
let storage = pim_ "storage" ;;
let uriPrefix = pim_ "uriPrefix" ;;
let c_Workspace = pim_ "Workspace" ;;
let workspace = pim_ "workspace" ;;

module Open = struct
  let pim_c_ControlledStorage = c_ControlledStorage
  let pim_masterWorkspace = masterWorkspace
  let pim_c_PersonalStorage = c_PersonalStorage
  let pim_preferencesFile = preferencesFile
  let pim_c_PublicStorage = c_PublicStorage
  let pim_c_Storage = c_Storage
  let pim_storage = storage
  let pim_uriPrefix = uriPrefix
  let pim_c_Workspace = c_Workspace
  let pim_workspace = workspace
end
