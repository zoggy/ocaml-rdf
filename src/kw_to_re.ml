let s = Sys.argv.(1);;

let len = String.length s ;;
let b = Buffer.create 256 ;;
for i = 0 to len - 1 do
  Printf.bprintf b "('%c'|'%c') "
    (Char.lowercase s.[i]) (Char.uppercase s.[i])
done;;
Printf.printf "| %s -> %s\n" (Buffer.contents b) (String.uppercase s);;
