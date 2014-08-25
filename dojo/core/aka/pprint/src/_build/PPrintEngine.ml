(**************************************************************************)
(*                                                                        *)
(*  PPrint                                                                *)
(*                                                                        *)
(*  Francois Pottier, INRIA Paris-Rocquencourt                            *)
(*  Nicolas Pouillard, IT University of Copenhagen                        *)
(*                                                                        *)
(*  Copyright 2007-2013 INRIA. All rights reserved. This file is          *)
(*  distributed under the terms of the CeCILL-C license, as described     *)
(*  in the file LICENSE.                                                  *)
(*                                                                        *)
(**************************************************************************)

(* ------------------------------------------------------------------------- *)

(* The last element of a non-empty list. *)

let rec last x xs =
  match xs with
  | [] ->
      x
  | x :: xs ->
      last x xs

let last = function
  | [] ->
      assert false
  | x :: xs ->
      last x xs

(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* A uniform interface for output channels. *)

module type OUTPUT = sig
  type channel
  val char: channel -> char -> unit
  val substring: channel -> string -> int (* offset *) -> int (* length *) -> unit
end

(* ------------------------------------------------------------------------- *)

(* Three implementations of the above interface, respectively based on output
   channels, memory buffers, and formatters. This compensates for the fact
   that ocaml's standard library does not allow creating an output channel
   that feeds into a memory buffer (a regrettable omission). *)

module ChannelOutput : OUTPUT with type channel = out_channel = struct
  type channel = out_channel
  let char = output_char
  let substring = output
end

module BufferOutput : OUTPUT with type channel = Buffer.t = struct
  type channel = Buffer.t
  let char = Buffer.add_char
  let substring = Buffer.add_substring
end

module FormatterOutput : OUTPUT with type channel = Format.formatter = struct
  type channel = Format.formatter
  let char = Format.pp_print_char
  let substring fmt = fst (Format.pp_get_formatter_output_functions fmt ())
end

(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* Here is the algebraic data type of documents. It is analogous to Daan
   Leijen's version, but the binary constructor [Union] is replaced with
   the unary constructor [Group], and the constant [Line] is replaced with
   more general constructions, namely [IfFlat], which provides alternative
   forms depending on the current flattening mode, and [HardLine], which
   represents a newline character, and causes a failure in flattening mode. *)

type document =

    (* [Empty] is the empty document. *)

  | Empty

    (* [Char c] is a document that consists of the single character [c]. We
       enforce the invariant that [c] is not a newline character. *)

  | Char of char

    (* [String (s, ofs, len)] is a document that consists of the portion of
       the string [s] delimited by the offset [ofs] and the length [len]. We
       assume, but do not check, that this portion does not contain a newline
       character. *)

  | String of string * int * int

  (* [FancyString (s, ofs, len, apparent_length)] is a (portion of a) string
     that may contain fancy characters: color escape characters, UTF-8 or
     multi-byte characters, etc. Thus, the apparent length (which corresponds
     to what will be visible on screen) differs from the length (which is a
     number of bytes, and is reported by [String.length]). We assume, but do
     not check, that fancystrings do not contain a newline character. *)

  | FancyString of string * int * int * int

  (* [Blank n] is a document that consists of [n] blank characters. *)

  | Blank of int

    (* When in flattening mode, [IfFlat (d1, d2)] turns into the document
       [d1]. When not in flattening mode, it turns into the document [d2]. *)

  | IfFlat of document * document

  (* When in flattening mode, [HardLine] causes a failure, which requires
     backtracking all the way until the stack is empty. When not in flattening
     mode, it represents a newline character, followed with an appropriate
     number of indentation. A common way of using [HardLine] is to only use it
     directly within the right branch of an [IfFlat] construct. *)

  | HardLine

  (* [Cat doc1 doc2] is the concatenation of the documents [doc1] and
     [doc2]. *)

  | Cat of document * document

    (* [Nest (j, doc)] is the document [doc], in which the indentation level
       has been increased by [j], that is, in which [j] blanks have been
       inserted after every newline character. *)

  | Nest of int * document

    (* [Group doc] represents an alternative: it is either a flattened form of
       [doc], in which occurrences of [Group] disappear and occurrences of
       [IfFlat] resolve to their left branch, or [doc] itself. *)

  | Group of document

    (* [Column f] is the document obtained by applying [f] to the current
       column number. *)

  | Column of (int -> document)

    (* [Nesting f] is the document obtained by applying [f] to the current
       indentation level, that is, the number of blanks that were printed
       at the beginning of the current line. *)

  | Nesting of (int -> document)

(* ------------------------------------------------------------------------- *)

(* The above algebraic data type is not exposed to the user. Instead, we
   expose the following functions. *)

let empty =
  Empty

let char c =
  assert (c <> '\n');
  Char c

let substring s ofs len =
  if len = 0 then
    Empty
  else
    String (s, ofs, len)

let string s =
  substring s 0 (String.length s)

let fancysubstring s ofs len apparent_length =
  if len = 0 then
    Empty
  else
    FancyString (s, ofs, len, apparent_length)

let fancystring s apparent_length =
  fancysubstring s 0 (String.length s) apparent_length

(* The following function was stolen from [Batteries]. *)
let utf8_length s =
  let rec length_aux s c i =
    if i >= String.length s then c else
    let n = Char.code (String.unsafe_get s i) in
    let k =
      if n < 0x80 then 1 else
      if n < 0xe0 then 2 else
      if n < 0xf0 then 3 else 4
    in
    length_aux s (c + 1) (i + k)
  in
  length_aux s 0 0

let utf8string s =
  fancystring s (utf8_length s)

let hardline =
  HardLine

let blank n =
  match n with
  | 0 ->
      Empty
  | 1 ->
      Blank 1
  | _ ->
      Blank n

let internal_break i =
  IfFlat (blank i, HardLine)

let break0 =
  internal_break 0

let break1 =
  internal_break 1

let break i =
  match i with
  | 0 ->
      break0
  | 1 ->
      break1
  | _ ->
      internal_break i

let (^^) x y =
  match x, y with
  | Empty, x
  | x, Empty ->
      x
  | _, _ ->
      Cat (x, y)

let nest i x =
  assert (i >= 0);
  Nest (i, x)

let group x =
  Group x

let column f =
  Column f

let nesting f =
  Nesting f

let ifflat x y =
  IfFlat (x, y)

(* ------------------------------------------------------------------------- *)

(* The pretty rendering algorithm: preliminary declarations. *)

(* The renderer is supposed to behave exactly like Daan Leijen's, although its
   implementation is quite radically different. Instead of relying on
   Haskell's lazy evaluation mechanism, we implement an abstract machine with
   mutable current state, forking, backtracking (via an explicit stack of
   choice points), and cut (disposal of earlier choice points). *)

(* The renderer's input consists of an ordered sequence of documents. Each
   document carries an extra indentation level, akin to an implicit [Nest]
   constructor, and a ``flattening'' flag, which, if set, means that this
   document should be printed in flattening mode. *)

(* An alternative coding style would be to avoid decorating each input
   document with an indentation level and a flattening mode, and allow
   the input sequence to contain instructions that set the current
   nesting level or reset the flattening mode. That would perhaps be
   slightly more readable, and slightly less efficient. *)

type input =
  | INil
  | ICons of int * bool * document * input

(* When possible (that is, when the stack is empty), the renderer writes
   directly to the output channel. Otherwise, output is buffered until either
   a failure point is reached (then, the buffered output is discarded) or a
   cut is reached (then, all buffered output is committed to the output
   channel). At all times, the length of the buffered output is at most one
   line. *)

(* The buffered output consists of a list of characters and strings. It is
   stored in reverse order (the head of the list should be printed last). *)

type output =
  | OEmpty
  | OChar of char * output
  | OString of string * int * int * output
  | OBlank of int * output

(* The renderer maintains the following state record. For efficiency, the
   record is mutable; it is copied when the renderer forks, that is, at
   choice points. *)

type 'channel state = {

    (* The line width and ribbon width. *)

    width: int;
    ribbon: int;

    (* The output channel. *)

    channel: 'channel;

    (* The current indentation level. This is the number of blanks that
       were printed at the beginning of the current line. *)

    mutable indentation: int;

    (* The current column. *)

    mutable column: int;

    (* The renderer's input. For efficiency, the input is assumed to never be
       empty, and the leading [ICons] constructor is inlined within the state
       record. In other words, the fields [nest1], [flatten1], and [input1]
       concern the first input document, and the field [input] contains the
       rest of the input sequence. *)

    mutable indent1: int;
    mutable flatten1: bool;
    mutable input1: document;
    mutable input: input;

    (* The renderer's buffer output. *)

    mutable output: output;

  }

(* The renderer maintains a stack of resumptions, that is, states in which
   execution should be resumed if the current thread of execution fails by
   lack of space on the current line. *)

(* It is not difficult to prove that the stack is empty if and only if
   flattening mode is off. Furthermore, when flattening mode is on,
   all groups are ignored, so no new choice points are pushed onto the
   stack. As a result, the stack has height one at most at all times,
   so that the stack height is zero when flattening mode is off and
   one when flattening mode is on. *)

type 'channel stack =
    'channel state list

(* ------------------------------------------------------------------------- *)

(* The pretty rendering algorithm: code. *)

(* The renderer is parameterized over an implementation of output channels. *)

module Renderer (Output : OUTPUT) = struct

  type channel =
      Output.channel

  type dummy =
      document
  type document =
      dummy

  (* Printing blank space (indentation characters). *)

  let blank_length =
    80

  let blank_buffer =
    String.make blank_length ' '

  let rec blanks channel n =
    if n <= 0 then
      ()
    else if n <= blank_length then
      Output.substring channel blank_buffer 0 n
    else begin
      Output.substring channel blank_buffer 0 blank_length;
      blanks channel (n - blank_length)
    end

  (* Committing buffered output to the output channel. The list is printed in
     reverse order. The code is not tail recursive, but there is no risk of
     stack overflow, since the length of the buffered output cannot exceed one
     line. *)

  let rec commit channel = function
    | OEmpty ->
	()
    | OChar (c, output) ->
	commit channel output;
	Output.char channel c
    | OString (s, ofs, len, output) ->
	commit channel output;
	Output.substring channel s ofs len
    | OBlank (n, output) ->
	commit channel output;
	blanks channel n

  (* The renderer's abstract machine. *)

  (* The procedures [run], [shift], [emit_char], [emit_string], and
     [emit_blanks] are mutually recursive, and are tail recursive. They
     maintain a stack and a current state. The states in the stack, and the
     current state, are pairwise distinct, so that the current state can be
     mutated without affecting the contents of the stack. *)

  (* An invariant is: the buffered output is nonempty only when the stack is
     nonempty. The contrapositive is: if the stack is empty, then the buffered
     output is empty. Indeed, the fact that the stack is empty means that no
     choices were made, so we are not in a speculative mode of execution: as a
     result, all output can be sent directly to the output channel. On the
     contrary, when the stack is nonempty, there is a possibility that we
     might backtrack in the future, so all output should be held in a
     buffer. *)

  (* [run] is allowed to call itself recursively only when no material is
     printed.  In that case, the check for failure is skipped -- indeed,
     this test is performed only within [shift]. *)

  let rec run (stack : channel stack) (state : channel state) : unit =

    (* Examine the first piece of input, as well as (in some cases) the
       current flattening mode. *)

    match state.input1, state.flatten1 with

    (* The first piece of input is an empty document. Discard it
       and continue. *)

    | Empty, _ ->
	shift stack state

    (* The first piece of input is a character. Emit it and continue. *)

    | Char c, _ ->
	emit_char stack state c

    (* The first piece of input is a string. Emit it and continue. *)

    | String (s, ofs, len), _ ->
	emit_string stack state s ofs len len
    | FancyString (s, ofs, len, apparent_length), _ ->
	emit_string stack state s ofs len apparent_length
    | Blank n, _ ->
	emit_blanks stack state n

    (* The first piece of input is a hard newline instruction. *)

    (* If flattening mode is off, then we behave as follows. We emit a newline
       character, followed by the prescribed amount of indentation. We update
       the current state to record how many indentation characters were
       printed and to to reflect the new column number. Then, we discard the
       current piece of input and continue. *)

    | HardLine, false ->
	assert (stack = []);     (* since flattening mode is off, the stack must be empty. *)
	Output.char state.channel '\n';
	let i = state.indent1 in
	blanks state.channel i;
	state.column <- i;
	state.indentation <- i;
	shift stack state

    (* If flattening mode is on, then [HardLine] causes an immediate failure. We
       backtrack all the way to the state found at the bottom of the stack.
       (Indeed, if we were to backtrack to the state found at the top of the stack,
       then we would come back to this point in flattening mode, and fail again.)
       This will take us back to non-flattening mode, so that, when we come back
       to this [HardLine], we will be able to honor it. *)

    | HardLine, true ->
        assert (stack <> []); (* since flattening mode is on, the stack must be non-empty. *)
        run [] (last stack)

    (* The first piece of input is an [IfFlat] conditional instruction. *)

    | IfFlat (doc, _), true
    | IfFlat (_, doc), false ->
	state.input1 <- doc;
	run stack state

    (* The first piece of input is a concatenation operator. We take it
       apart and queue both documents in the input sequence. *)

    | Cat (doc1, doc2), _ ->
	state.input1 <- doc1;
	state.input <- ICons (state.indent1, state.flatten1, doc2, state.input);
	run stack state

    (* The first piece of input is a [Nest] operator. We increase the amount
       of indentation to be applied to the first input document. *)

    | Nest (j, doc), _ ->
	state.indent1 <- state.indent1 + j;
	state.input1 <- doc;
	run stack state

    (* The first piece of input is a [Group] operator, and flattening mode
       is currently off. This introduces a choice point: either we flatten
       this whole group, or we don't. We try the former possibility first:
       this is done by enabling flattening mode. Should this avenue fail,
       we push the current state, in which flattening mode is disabled,
       onto the stack. *)

    (* Note that the current state is copied before continuing, so that
       the state that is pushed on the stack is not affected by future
       modifications. This is a fork. *)

    | Group doc, false ->
	state.input1 <- doc;
	run (state :: stack) { state with flatten1 = true }

    (* The first piece of input is a [Group] operator, and flattening mode
       is currently on. The operator is ignored. *)

    | Group doc, true ->
	state.input1 <- doc;
	run stack state

    (* The first piece of input is a [Column] operator. The current column
       is fed into it, so as to produce a document, with which we continue. *)

    | Column f, _ ->
	state.input1 <- f state.column;
	run stack state

    (* The first piece of input is a [Column] operator. The current
       indentation level is fed into it, so as to produce a document, with
       which we continue. *)

    | Nesting f, _ ->
	state.input1 <- f state.indentation;
	run stack state

  (* [shift] discards the first document in the input sequence, so that the
     second input document, if there is one, becomes first. The renderer stops
     if there is none. *)

  and shift stack state =

    assert (state.output = OEmpty || stack <> []);
    assert (state.flatten1 = (stack <> []));

    (* If the stack is nonempty and we have exceeded either the width or the
       ribbon width parameters, then fail. Backtracking is implemented by
       discarding the current state, popping a state off the stack, and making
       it the current state. *)

    match stack with
    | resumption :: stack
      when state.column > state.width
        || state.column - state.indentation > state.ribbon ->
	run stack resumption
    | _ ->

	match state.input with
	| INil ->

	    (* End of input. Commit any buffered output and stop. *)

	    commit state.channel state.output

	| ICons (indent, flatten, head, tail) ->

	    (* There is an input document. Move it one slot ahead and
	       check if we are leaving flattening mode. *)

	    state.indent1 <- indent;
	    state.input1 <- head;
	    state.input <- tail;
	    if state.flatten1 && not flatten then begin

	      (* Leaving flattening mode means success: we have flattened
		 a certain group, and fitted it all on a line, without
		 reaching a failure point. We would now like to commit our
		 decision to flatten this group. This is a Prolog cut. We
		 discard the stack of choice points, replacing it with an
		 empty stack, and commit all buffered output. *)

	      state.flatten1 <- flatten; (* false *)
	      commit state.channel state.output;
	      state.output <- OEmpty;
	      run [] state

	    end
	    else
	      run stack state

  (* [emit_char] prints a character (either to the output channel or to the
     output buffer), increments the current column, discards the first piece
     of input, and continues. *)

  and emit_char stack state c =
    begin match stack with
    | [] ->
	Output.char state.channel c
    | _ ->
	state.output <- OChar (c, state.output)
    end;
    state.column <- state.column + 1;
    shift stack state

  (* [emit_string] prints a string (either to the output channel or to the
     output buffer), updates the current column, discards the first piece of
     input, and continues. *)

  and emit_string stack state s ofs len apparent_length =
    begin match stack with
    | [] ->
	Output.substring state.channel s ofs len
    | _ ->
	state.output <- OString (s, ofs, len, state.output)
    end;
    state.column <- state.column + apparent_length;
    shift stack state

  (* [emit_blanks] prints a blank string (either to the output channel or to
     the output buffer), updates the current column, discards the first piece
     of input, and continues. *)

  and emit_blanks stack state n =
    begin match stack with
    | [] ->
	blanks state.channel n
    | _ ->
	state.output <- OBlank (n, state.output)
    end;
    state.column <- state.column + n;
    shift stack state

  (* This is the renderer's main entry point. *)

  let pretty rfrac width channel document =
    run [] {
      width = width;
      ribbon = max 0 (min width (truncate (float_of_int width *. rfrac)));
      channel = channel;
      indentation = 0;
      column = 0;
      indent1 = 0;
      flatten1 = false;
      input1 = document;
      input = INil;
      output = OEmpty;
    }

(* ------------------------------------------------------------------------- *)

(* The compact rendering algorithm. *)

  let compact channel document =

    let column =
      ref 0
    in

    let rec scan = function
      | Empty ->
	  ()
      | Char c ->
	  Output.char channel c;
	  column := !column + 1
      | String (s, ofs, len) ->
	  Output.substring channel s ofs len;
	  column := !column + len
      | FancyString (s, ofs, len, apparent_length) ->
	  Output.substring channel s ofs len;
	  column := !column + apparent_length
      | Blank n ->
	  blanks channel n;
	  column := !column + n
      | HardLine ->
	  Output.char channel '\n';
	  column := 0
      | Cat (doc1, doc2) ->
	  scan doc1;
	  scan doc2
      | IfFlat (doc, _)
      | Nest (_, doc)
      | Group doc ->
	  scan doc
      | Column f ->
	  scan (f !column)
      | Nesting f ->
	  scan (f 0)
    in

    scan document

end

(* ------------------------------------------------------------------------- *)

(* Instantiating the renderers for the three kinds of output channels. *)

module ToChannel =
  Renderer(ChannelOutput)

module ToBuffer =
  Renderer(BufferOutput)

module ToFormatter =
  Renderer(FormatterOutput)

