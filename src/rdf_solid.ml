
let solid_str = "http://www.w3.org/ns/solid/terms#";;
let solid = Iri.of_string solid_str ;;
let solid_ s = Iri.of_string (solid_str ^ s);;

let c_Account = solid_ "Account" ;;
let c_Inbox = solid_ "Inbox" ;;
let c_Notification = solid_ "Notification" ;;
let c_Timeline = solid_ "Timeline" ;;
let c_TypeIndex = solid_ "TypeIndex" ;;
let account = solid_ "account" ;;
let inbox = solid_ "inbox" ;;
let notification = solid_ "notification" ;;
let read = solid_ "read" ;;
let timeline = solid_ "timeline" ;;
let typeIndex = solid_ "typeIndex" ;;

module Open = struct
  let solid_c_Account = c_Account
  let solid_c_Inbox = c_Inbox
  let solid_c_Notification = c_Notification
  let solid_c_Timeline = c_Timeline
  let solid_c_TypeIndex = c_TypeIndex
  let solid_account = account
  let solid_inbox = inbox
  let solid_notification = notification
  let solid_read = read
  let solid_timeline = timeline
  let solid_typeIndex = typeIndex
end

class from ?sub g =
  let sub = match sub with None -> g.Rdf_graph.name() | Some iri -> iri in
  let sub = Rdf_term.Iri sub in
  object(self)
  method account = Rdf_graph.iri_objects_of g ~sub ~pred: account
  method account_opt = match self#account with [] -> None | x::_ -> Some x
  method inbox = Rdf_graph.iri_objects_of g ~sub ~pred: inbox
  method inbox_opt = match self#inbox with [] -> None | x::_ -> Some x
  method notification = Rdf_graph.iri_objects_of g ~sub ~pred: notification
  method notification_opt = match self#notification with [] -> None | x::_ -> Some x
  method read = Rdf_graph.iri_objects_of g ~sub ~pred: read
  method read_opt = match self#read with [] -> None | x::_ -> Some x
  method timeline = Rdf_graph.iri_objects_of g ~sub ~pred: timeline
  method timeline_opt = match self#timeline with [] -> None | x::_ -> Some x
  method typeIndex = Rdf_graph.iri_objects_of g ~sub ~pred: typeIndex
  method typeIndex_opt = match self#typeIndex with [] -> None | x::_ -> Some x
  end
