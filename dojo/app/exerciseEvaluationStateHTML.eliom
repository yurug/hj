(* -*- tuareg -*- *)

{shared{
open Lwt
open Eliom_content.Html5
open Eliom_content.Html5.D
open Html5_types
open Eliom_service
}}

open Identifier
open ExerciseHTTP

{client{
exception Done

let display_evaluation_state =
    fun exo_str answers_str name_str grade_div console_write ->
      let score_box = fun score criteria ->
        div ~a:[a_class ["score_box"]] [
          p ~a:[a_class ["score"]] [pcdata score];
          p ~a:[a_class ["criteria"]] criteria
        ]
      in
      let open ExerciseHTTP in
          let criteria_as_html = function
            | Automatic -> pcdata "Dojo"
            | UserDefined s -> pcdata s
          in
          let scores_as_html scores =
            List.map (fun (c, (i, o)) ->
            score_box (Printf.sprintf "%d/%d" i o) [criteria_as_html c]
            ) scores
          in
          let update_grade_div h =
            Manip.replaceChildren grade_div h
          in
          let write_trace_on_console trace =
            match console_write with
              | None -> ()
              | Some write ->
                List.iter (function Message s -> write [p [pcdata s]]) trace
          in
          let process_command cmd =
            Js.Unsafe.eval_string cmd
          in
          lwt on_update = AnswersHTTP.on_each_update answers_str in
          let show _ =
            %exercise_evaluation_state_server_function (exo_str, name_str)
            >>= function
              | EvaluationBeingProcessed ->
                return (update_grade_div [score_box "..." []])
              | EvaluationDone (_, _, _, grade, commands) ->
                write_trace_on_console grade.trace;
                List.iter process_command commands;
                update_grade_div (scores_as_html grade.scores);
                raise_lwt Done
              | EvaluationFailed ->
                update_grade_div [score_box "!" []];
		raise_lwt Done
              | NoEvaluation ->
                update_grade_div [score_box "?" []];
		raise_lwt Done
          in
          try_lwt
            update_grade_div [score_box "..." []];
	    let rec loop k =
	      if k > 500 then raise_lwt Done else Lwt_js.sleep 1. >> show () >> loop (succ k) in
	    loop 0
          with Done ->
	    return () 

}}
