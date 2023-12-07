open Unix
open Thread

let countdown n =
  let rec loop remaining =
    if remaining > 0 then (
      print_endline (string_of_int remaining);
      Unix.sleep 1;
      loop (remaining - 1))
    else print_endline "Time's up!"
  in

  let countdown_thread = Thread.create loop n in
  Thread.join countdown_thread

let () = countdown 5
