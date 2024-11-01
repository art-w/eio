module Fiber_context = Eio.Private.Fiber_context
module Trace = Eio.Private.Trace
module Lf_queue = Eio_utils.Lf_queue
module Suspended = Eio_utils.Suspended  (* Adds tracing to continuations *)

exception Deadlock_detected

(* The scheduler could just return [unit], but this is clearer. *)
type exit = [`Exit_scheduler]

type stdenv = <
  clock : Clock.t;
  mono_clock : Clock.Mono.t;
  debug : Eio.Debug.t;
  backend_id: string;
>

type t = {
  (* Suspended fibers waiting to run again.
     [Lf_queue] is like [Stdlib.Queue], but is thread-safe (lock-free) and
     allows pushing items to the head too, which we need. *)
  run_q : (unit -> exit) Lf_queue.t;

  mono_clock : Clock.Mono.t;

  scheduler_tag : Fiber_context.scheduler_tag;

  stuck : ((unit, exn) result -> unit) option Atomic.t;
}

type Fiber_context.scheduler_tag += Mock_scheduler of t

module Wall_clock = struct
  type t = Clock.Mono.t
  type time = float
    
  let wall_of_mtime m = Int64.to_float (Mtime.to_uint64_ns m) /. 1e9
  let wall_to_mtime w = Mtime.of_uint64_ns (Int64.of_float (w *. 1e9))

  let now t = wall_of_mtime (Eio.Time.Mono.now t)
  let sleep_until t time = Eio.Time.Mono.sleep_until t (wall_to_mtime time)
end

let wall_clock =
  let handler = Eio.Time.Pi.clock (module Wall_clock) in
  fun mono_clock -> Eio.Resource.T (mono_clock, handler)

(* Resume the next runnable fiber, if any. *)
let rec schedule t : exit =
  match Lf_queue.pop t.run_q with
  | Some f -> f ()
  | None ->
    (* Nothing is runnable. Try advancing the clock. *)
    if Clock.Mono.try_advance t.mono_clock then schedule t
    else `Exit_scheduler      (* Finished (or deadlocked) *)

let rec io_yield t finished () =
  if Lf_queue.is_empty t.run_q
  then
    if finished () then `Exit_scheduler
    else (
      Effect.perform (Eio.Private.Effects.Suspend (fun _ctx enqueue ->
        let stuck = Some enqueue in
        if not (Atomic.compare_and_set t.stuck None stuck)
        then invalid_arg "nested scheduler is already stuck";
        if not (Lf_queue.is_empty t.run_q)
        && Atomic.compare_and_set t.stuck stuck None
        then enqueue (Ok ())
      ));
      Lf_queue.push t.run_q (io_yield t finished);
      schedule t
    )
  else (
    Eio.Fiber.yield ();
    Lf_queue.push t.run_q (io_yield t finished);
    schedule t
  )

(* Run [main] in an Eio main loop. *)
let run_full main =
  let mono_clock = Clock.Mono.make () in
  let clock = wall_clock mono_clock in
  let stdenv = object (_ : stdenv)
    method clock = clock
    method mono_clock = mono_clock
    method debug = Eio.Private.Debug.v
    method backend_id = "mock"
  end in
  let rec t = { run_q = Lf_queue.create (); mono_clock; stuck = Atomic.make None; scheduler_tag = Mock_scheduler t } in
  let rec fork ~t ~new_fiber:fiber fn =
    Trace.fiber (Fiber_context.tid fiber);
    (* Create a new fiber and run [fn] in it. *)
    Effect.Deep.match_with fn ()
      { retc = (fun () -> Fiber_context.destroy fiber; schedule t);
        exnc = (fun ex ->
            let bt = Printexc.get_raw_backtrace () in
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex bt
          );
        effc = fun (type a) (e : a Effect.t) : ((a, exit) Effect.Deep.continuation -> exit) option ->
          match e with
          | Eio.Private.Effects.Suspend f -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              (* Ask [f] to register whatever callbacks are needed to resume the fiber.
                 e.g. it might register a callback with a promise, for when that's resolved. *)
              f fiber (fun result ->
                  (* The fiber is ready to run again. Add it to the queue. *)
                  Lf_queue.push t.run_q (fun () ->
                      (* Resume the fiber. *)
                      Fiber_context.clear_cancel_fn fiber;
                      match result with
                      | Ok v -> Suspended.continue k v
                      | Error ex -> Suspended.discontinue k ex
                    );
                  if Atomic.get t.stuck <> None
                  then match Atomic.exchange t.stuck None with
                    | None -> ()
                    | Some unstuck -> unstuck (Ok ())
                );
              (* Switch to the next runnable fiber while this one's blocked. *)
              schedule t
            )
          | Eio.Private.Effects.Fork (new_fiber, f) ->
            if Fiber_context.scheduler_tag new_fiber != t.scheduler_tag
            then None
            else Some (fun k ->
              let k = { Suspended.k; fiber } in
              (* Arrange for the forking fiber to run immediately after the new one. *)
              Lf_queue.push_head t.run_q (Suspended.continue k);
              (* Create and run the new fiber (using fiber context [new_fiber]). *)
              fork ~t ~new_fiber f
            )
          | Eio.Private.Effects.Nest ->
              Some (fun k -> Effect.Deep.continue k (with_nested_sched ~t))
          | Eio.Private.Effects.Get_context -> Some (fun k ->
              Effect.Deep.continue k fiber
            )
          | _ -> None
      }
  and with_nested_sched : type a. t:t -> (unit -> a) -> a = fun ~t:parent fn ->
    let rec t =
      { parent with scheduler_tag = Mock_scheduler t;
                    stuck = Atomic.make None;
                    run_q = Lf_queue.create () } in
    let new_fiber = Fiber_context.make_root t.scheduler_tag in
    let result = ref None in
    let finished () = !result <> None in
    Lf_queue.push t.run_q (io_yield t finished);
    let `Exit_scheduler =
      fork ~t ~new_fiber @@ fun () ->
      match fn () with
      | x -> result := Some (Ok x)
      | exception ex ->
        let bt = Printexc.get_raw_backtrace () in
        result := Some (Error (ex, bt))
    in
    match !result with
    | None -> assert false
    | Some (Ok v) -> v
    | Some (Error (e, bt)) -> Printexc.raise_with_backtrace e bt
  in
  let new_fiber = Fiber_context.make_root t.scheduler_tag in
  let result = ref None in
  let `Exit_scheduler =
    Domain_local_await.using
      ~prepare_for_await:Eio_utils.Dla.prepare_for_await
      ~while_running:(fun () ->
        fork ~t ~new_fiber (fun () -> result := Some (main stdenv))) in
  match !result with
  | None -> raise Deadlock_detected
  | Some x -> x

let run fn =
  run_full (fun _ -> fn ())
