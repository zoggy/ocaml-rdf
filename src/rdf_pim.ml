
let pim_str = "http://www.w3.org/ns/pim/space#";;
let pim = Iri.of_string pim_str ;;
let pim_ s = Iri.of_string (pim_str ^ s);;

let controlledStorage = pim_ "ControlledStorage" ;;
let personalStorage = pim_ "PersonalStorage" ;;
let publicStorage = pim_ "PublicStorage" ;;
let storage = pim_ "Storage" ;;
let workspace = pim_ "Workspace" ;;

module Open = struct
  let pim_controlledStorage = controlledStorage
  let pim_personalStorage = personalStorage
  let pim_publicStorage = publicStorage
  let pim_storage = storage
  let pim_workspace = workspace
end
