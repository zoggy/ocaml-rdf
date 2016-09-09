
let solid_str = "http://www.w3.org/ns/solid/terms#";;
let solid = Iri.of_string solid_str ;;
let solid_ s = Iri.of_string (solid_str ^ s);;

let c_Account = solid_ "Account" ;;
let account = solid_ "account" ;;
let c_Inbox = solid_ "Inbox" ;;
let inbox = solid_ "inbox" ;;
let c_Notification = solid_ "Notification" ;;
let notification = solid_ "notification" ;;
let read = solid_ "read" ;;
let c_Timeline = solid_ "Timeline" ;;
let timeline = solid_ "timeline" ;;
let c_TypeIndex = solid_ "TypeIndex" ;;
let typeIndex = solid_ "typeIndex" ;;

module Open = struct
  let solid_c_Account = c_Account
  let solid_account = account
  let solid_c_Inbox = c_Inbox
  let solid_inbox = inbox
  let solid_c_Notification = c_Notification
  let solid_notification = notification
  let solid_read = read
  let solid_c_Timeline = c_Timeline
  let solid_timeline = timeline
  let solid_c_TypeIndex = c_TypeIndex
  let solid_typeIndex = typeIndex
end
