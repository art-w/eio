```ocaml
# #require "eio";;
# #require "eio.mock";;
# #require "eio_main";;
```

Given a unique identifier generator effect handler:

```ocaml
type _ Effect.t += Uid : int Effect.t

let with_uid f =
  Effect.Deep.match_with f ()
    {
      retc = (fun x _ -> x);
      exnc = (fun e _ -> raise e);
      effc =
        (fun (type effect) (effect : effect Effect.t) ->
          match effect with
          | Uid ->
              Some (fun (k : (effect, _) Effect.Deep.continuation) ->
                      fun (state:int) -> Effect.Deep.continue k state (state + 1))
          | _ -> None);
    }
    0

let uid () = Effect.perform Uid
```

This is broken because the forked Uid effects are performed in the wrong scope, outside of their handler:

```ocaml
open Eio

let test env =
  with_uid @@ fun () ->
  Format.printf "first uid = %i@." (uid ());
  Fiber.both
    (fun () ->
      Time.sleep env#clock 0.001;
      Format.printf " left uid = %i@." (uid ());
      Time.sleep env#clock 0.001)
    (fun () ->
      Time.sleep env#clock 0.001;
      Format.printf "right uid = %i@." (uid ());
      Time.sleep env#clock 0.001);
  Format.printf " last uid = %i@." (uid ())
```
```ocaml
# Eio_main.run test ;;
first uid = 0
Exception: Stdlib.Effect.Unhandled(Uid)
```

But this works because the added `Fiber.nest` avoids Eio effects escaping the `with_uid` scope:

```ocaml
open Eio

let test env =
     with_uid @@ fun () ->
(**) Fiber.nest @@ fun () ->
     Format.printf "first uid = %i@." (uid ());
     Fiber.both
       (fun () ->
         Time.sleep env#clock 0.001;
         Format.printf " left uid = %i@." (uid ());
         Time.sleep env#clock 0.001)
       (fun () ->
         Time.sleep env#clock 0.001;
         Format.printf "right uid = %i@." (uid ());
         Time.sleep env#clock 0.001);
     Format.printf " last uid = %i@." (uid ())
```
```ocaml
# Eio_main.run test ;;
first uid = 0
 left uid = 1
right uid = 2
 last uid = 3
- : unit = ()
# Eio_mock.Backend.run_full test ;;
first uid = 0
+mock time is now 0.001
 left uid = 1
right uid = 2
+mock time is now 0.002
 last uid = 3
- : unit = ()
```

Note that Eio fibers that are forked with an **outer** switch are executed outside of the nested scheduler:

```ocaml
open Eio

let test () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun outer ->
  with_uid @@ fun () ->
  Fiber.nest @@ fun () ->
  let () =
    Switch.run @@ fun inner ->
    Fiber.fork ~sw:inner (fun () -> Format.printf "ok: %i@." (uid ()));
    Fiber.fork ~sw:outer (fun () -> Format.printf "not ok: %i@." (uid ()));
    Fiber.fork ~sw:inner (fun () -> Format.printf "ok: %i@." (uid ()))
  in
  Format.printf "inner terminated@."
```
```ocaml
# test () ;;
ok: 0
ok: 1
inner terminated
Exception: Stdlib.Effect.Unhandled(Uid)
```

On one end, this makes sense because the `outer` forked fiber could survive past the `with_uid` handler so the unhandled effect could be expected, without special care. But truth is, I just don't know what's needed to make it work or if it's even possible.

On the positive side, this can allow for multiple `with_uid` to run at different levels:

```ocaml
open Eio

let test env =
  Switch.run @@ fun outer ->
  let p, r = Promise.create () in
  with_uid @@ fun () ->
  Fiber.nest @@ fun () ->
  Switch.run @@ fun lvl0 ->
  with_uid @@ fun () ->
  Fiber.nest @@ fun () ->
  let () =
    Switch.run @@ fun lvl1 ->
    Fiber.fork ~sw:lvl1 (fun () ->
      Time.sleep env#clock 0.001;
      Format.printf " left lvl1: %i@." (uid ());
      Time.sleep env#clock 0.001;
      Fiber.fork ~sw:lvl0 (fun () ->
        Time.sleep env#clock 0.001;
        Format.printf " left lvl0: %i@." (uid ());
        Time.sleep env#clock 0.001;
        Promise.resolve r ();
        Time.sleep env#clock 0.009;
        Format.printf " left lvl0: %i@." (uid ());
      );
      Time.sleep env#clock 0.001;
      Format.printf " left lvl1: %i@." (uid ());
      Time.sleep env#clock 0.001;
      );
    Fiber.fork ~sw:lvl0 (fun () ->
      Format.printf "right lvl0: %i@." (uid ());
      Time.sleep env#clock 0.001;
      Format.printf "right lvl0: %i@." (uid ());
      Time.sleep env#clock 0.001;
      Promise.await p;
      Time.sleep env#clock 0.009;
      Format.printf "right lvl0: %i@." (uid ());
      );
    Promise.await p
  in
  Format.printf "lvl1 terminated@."
```
```ocaml
# Eio_main.run test ;;
right lvl0: 0
 left lvl1: 0
right lvl0: 1
 left lvl0: 2
 left lvl1: 1
lvl1 terminated
 left lvl0: 3
right lvl0: 4
- : unit = ()
# Eio_mock.Backend.run_full test ;;
right lvl0: 0
+mock time is now 0.001
 left lvl1: 0
right lvl0: 1
+mock time is now 0.002
+mock time is now 0.003
 left lvl0: 2
 left lvl1: 1
+mock time is now 0.004
lvl1 terminated
+mock time is now 0.013
 left lvl0: 3
right lvl0: 4
- : unit = ()
```

An unforeseen side-effect of `nest` is that it restricts how fibers can be forked. Eio allows for complex switch forking patterns:

```ocaml
open Eio

let test env =
  let p, r = Promise.create () in
  Switch.run @@ fun outer ->
  Switch.run @@ fun inner ->
  let () =
    Fiber.fork ~sw:outer @@ fun () ->
      Time.sleep env#clock 0.001;
      Fiber.fork ~sw:inner (fun () -> Time.sleep env#clock 0.001; Format.printf "this is fine@.");
      Promise.resolve r ()
  in
  Promise.await p (* comment this line to allow [inner] to terminate too soon *)
```
```ocaml
# Eio_main.run test ;;
this is fine
- : unit = ()
# Eio_mock.Backend.run_full test ;;
+mock time is now 0.001
+mock time is now 0.002
this is fine
- : unit = ()
```

Note that this would generally fail if done without care, since the `inner` switch could terminate before all of its forked fibers had a change to be created (resulting in a `"Switch finished!"` exception). If we add a call to `nest`, then this fork pattern results in an unhandled fork effect since the `inner` scope is unreachable from the `outer`:

```ocaml
open Eio

let test env =
     let p, r = Promise.create () in
     Switch.run @@ fun outer ->
(**) Fiber.nest @@ fun () ->
     Switch.run @@ fun inner ->
     let () =
       Fiber.fork ~sw:outer @@ fun () ->
         Time.sleep env#clock 0.001;
         (* it's now forbidden to fork from an [outer] fiber to an [inner] fiber! *)
         Fiber.fork ~sw:inner (fun () -> Time.sleep env#clock 0.001; Format.printf "this is not fine@.");
         Promise.resolve r ()
     in
     Promise.await p
```
```ocaml
# Eio_main.run test ;;
Exception: Stdlib.Effect.Unhandled(Eio__core__Fiber.Fork(_, _))
# Eio_mock.Backend.run_full test ;;
+mock time is now 0.001
Exception: Eio_mock__Backend.Deadlock_detected.
```

The error message could be improved, but otherwise I also don't know how to fix this. Help is welcome if you have ideas!

Anyway, with this minor limitations in mind, we can do some fancy stuff and define a scheduler to control the execution of Eio computations:

```ocaml
type _ Effect.t += Yield : unit Effect.t
type 'a coroutine = Return of 'a | Failed of exn | Suspended of (unit -> 'a coroutine)
let with_coroutine (type a) (f : unit -> a) : a coroutine =
  Effect.Deep.match_with f ()
    {
      retc = (fun x -> Return x);
      exnc = (fun e -> Failed e);
      effc =
        (fun (type effect) (effect : effect Effect.t) ->
          match effect with
          | Yield ->
              Some (fun (k : (effect, _) Effect.Deep.continuation) ->
                      Suspended (fun () -> Effect.Deep.continue k ()))
          | _ -> None);
    }

let yield () = Effect.perform Yield

let rec run_coroutine = function
  | Return v -> v
  | Failed e -> raise e
  | Suspended f -> Format.printf "."; run_coroutine (f ())
```
```ocaml
open Eio

let test () =
  Eio_main.run @@ fun env ->

  Switch.run @@ fun sw ->
  let fd = Path.open_in ~sw Path.(env#cwd / "nest.md") in

  let c =
    with_coroutine @@ fun () ->
    with_uid @@ fun () ->
    Fiber.nest @@ fun () ->
    Format.printf "first uid = %i@." (uid ());
    yield ();
    let p, u = Promise.create () in
    Fiber.both
      (fun () ->
        Format.printf " left uid = %i@." (uid ());
        Fiber.yield ();
        yield ();
        Format.printf "left read file@.";
        let s = Flow.read_all fd in
        yield ();
        Format.printf "left resolve promise@.";
        Promise.resolve u s;
        Format.printf " left uid = %i@." (uid ());
        yield ()
      )
      (fun () ->
        Format.printf "right uid = %i@." (uid ());
        yield ();
        Format.printf "right waiting for promise@.";
        let s = Promise.await p in
        yield ();
        Format.printf "right length = %#i@." (String.length s)
      );
    Format.printf " last uid = %i@." (uid ())
  in

  Format.printf "run coroutine:@." ;
  run_coroutine c
```
```ocaml
# test () ;;
first uid = 0
run coroutine:
. left uid = 1
right uid = 2
.right waiting for promise
.left read file
.left resolve promise
 left uid = 3
..right length = 12_870
 last uid = 4
- : unit = ()
```

This suggests that we could interop with arbitrary schedulers, for example Miou:

```ocaml
# #require "miou" ;;
# #require "miou.unix" ;;
```
```ocaml
open Eio

let test () =
  Eio_main.run @@ fun env ->
  Miou_unix.run @@ fun () ->

  let miou_p =
    Miou.async @@ fun () ->
    Switch.run @@ fun sw ->
    Miou_unix.sleep 0.0001;
    Fiber.fork_promise ~sw @@ fun () ->
    let fd = Path.open_in ~sw Path.(env#cwd / "nest.md") in
    Format.printf "ok until here@.";
    Miou_unix.sleep 0.0001;
    Flow.read_all fd
  in

  let eio_p = Miou.await_exn miou_p in
  let s = Promise.await_exn eio_p in
  Format.printf "This file length = %#i@." (String.length s)
```
```ocaml
# test () ;;
ok until here
Exception: Stdlib.Effect.Unhandled(Miou.Self)
```

By adding `Fiber.nest` every time we want to call Eio code, we avoid Eio fiber forks escaping above the Miou handler:

```ocaml
open Eio

let test () =
  Eio_main.run @@ fun env ->
  Miou_unix.run @@ fun () ->

  let miou_p =
     Miou.async @@ fun () ->
(**) Fiber.nest @@ fun () ->
     Switch.run @@ fun sw ->
     Miou_unix.sleep 0.0001;
     Fiber.fork_promise ~sw @@ fun () ->
     let fd = Path.open_in ~sw Path.(env#cwd / "nest.md") in
     Miou_unix.sleep 0.0001;
     Flow.read_all fd
  in

  let eio_p = Miou.await_exn miou_p in
  let s = Promise.await_exn eio_p in
  Format.printf "This file length = %#i@." (String.length s)
```
```ocaml
# test () ;;
This file length = 12_870
- : unit = ()
```

This is not 100% foolproof however, in general one needs to sandwich the `Miou.run` between two Eio schedulers to avoid deadlocks:

```ocaml
open Eio

let test () =
  Eio_main.run @@ fun env ->
  Miou_unix.run @@ fun () ->
  (* The purpose of this top-level [nest] is NOT to protect against unhandled effects.
     It's only there to ensure that we don't accidentally starve the Miou scheduler
     from the ability to run. *)
  Fiber.nest @@ fun () ->
  (* If you comment this line, then the program will hang on [Promise.await promise]
     below, as Eio will be unaware at this point that there's a Miou fiber which will
     resolve the promise (since Miou wasn't able to reach it).
     With the [nest] present, we ensure that the top-level Eio scheduler will cooperate
     with Miou and give it the ability to make progress once in a while. *)

  let promise, resolver = Promise.create () in

  let miou_p =
    Miou.async @@ fun () ->
    Format.printf "Miou.async started@.";

    Eio.Fiber.yield ();
    (* This is a bit weird to explain, but this [yield] performs an Eio effect,
       which is actually intercepted by Miou and prevents it from continuing right
       away the execution of this fiber...
       At some point Miou will propagate the performed Eio effects to Eio, starting
       with the [Promise.await] effect (the first one that was performed).
       Eio would then block, unaware of any ways for the promise to ever be resumed,
       unless the top-level [nest] is present and knows that it still has the option
       to cooperate with the [Miou.run] scheduler. If Miou is resumed, then it'll
       be able to continue: *)

    Format.printf "resolve promise@.";
    Miou_unix.sleep 0.001;
    Promise.resolve resolver ();
    Miou_unix.sleep 0.001;
  in

  Format.printf "await promise@.";
  Promise.await promise;
  Miou.await_exn miou_p
```
```ocaml
# test () ;;
await promise
Miou.async started
resolve promise
- : unit = ()
```

I believe a similar hanging bug could happen if Miou is never calling Eio routines (= preventing Eio from making progress on its own tasks), so one would also want to start a top-level Miou fiber to ensure that it yields control back to Eio once in a while (? needs more tests)

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Miou.run @@ fun () ->
  Fiber.nest @@ fun () ->
  let cooperate = Miou.async @@ fun () -> while true do Eio.Fiber.yield () done in
  (* ... main program ... *)
  Miou.cancel cooperate
```
