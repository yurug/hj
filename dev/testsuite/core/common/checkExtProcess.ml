open ExtProcess
open Testsuite
open Lwt

exception ShellCommandExecutionFailed of string

let test_shell_command s () =
  test ("Blind execution of shell command `" ^ s ^ "'.") (fun () ->
    blind_exec (!% s) >>= function
      | Unix.WEXITED 0 -> return ()
      | _ -> raise (ShellCommandExecutionFailed s)
  )

let run () =
  scenario "Shell command execution" (fun () ->
    !=> (test_shell_command "echo 'Hello'")
    >=> (test_shell_command "ls")
  )
