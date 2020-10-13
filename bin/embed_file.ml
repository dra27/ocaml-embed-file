module ActualUnix = Unix
open Core_kernel

module Command = struct
  include Command

  module For_unix = Private.For_unix (struct
    module Signal = struct type t end
    module Thread = struct
      type t = unit
      let create ~on_uncaught_exn f arg =
        let exit = (match on_uncaught_exn with `Kill_whole_process -> true | `Print_to_stderr -> false) in
        Exn.handle_uncaught ~exit (fun () -> f arg)
      let join () = ()
    end
    module Time = struct
      include Time
      let sexp_of_t t = failwith "For_unix.Time.sexp_of_t: not implemented"
    end
    module Unix = struct
      include ActualUnix

      type wait_on = [ `Any | `Group of Pid.t | `My_group | `Pid of Pid.t ]
      let wait ?restart wait_on = failwith "For_unix.wait: not implemented"
      module Exit = struct
        type error = [ `Exit_non_zero of int ]
        type t = (unit, error) Result.t
      end
      module Exit_or_signal = struct
        type error =
          [ Exit.error
          | `Signal of Signal.t
          ]

        type t = (unit, error) Result.t
      end
      module File_descr = struct
        type t = file_descr
      end
      module Process_info = struct
        type t =
          { pid : Pid.t
          ; stdin : File_descr.t
          ; stdout : File_descr.t
          ; stderr : File_descr.t
          }
      end
      let exec ~prog ~argv ?use_path ?env () = failwith "For_unix.exec: not implemented"
      let unsetenv key = failwith "For_unix.unsetenv: not implemented"
      let getpid () = failwith "For_unix.getpid: not implemented"
      let close ?restart fd = close fd
      let close_process_in _ = failwith "For_unix.close_process_in: not implemented"
      let putenv ~key ~data = failwith "For_unix.putenv: not implemented"
      let unsafe_getenv key = try Some (unsafe_getenv key) with Not_found -> None
      let create_process_env ?working_dir ?prog_search_path ?argv0 ~prog ~args ~env () =
        failwith "For_unix.create_process_env: not implemented"
      type env =
        [ `Replace of (string * string) list
        | `Extend of (string * string) list
        | `Override of (string * string option) list
        | `Replace_raw of string list
        ]
    end
    module Version_util = Version_util
  end)

  let run = For_unix.run
end

let () = Command.run Embed_file_lib.command
