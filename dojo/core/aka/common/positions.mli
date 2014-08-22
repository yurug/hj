(**************************************************************************)
(* Adapted from:                                                          *)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Extension of standard library's positions. *)

(** {2 Extended lexing positions} *)

(** Abstract for position in the lexing stream. It does
    represents non empty interval in the stream. *)
type position

(** This value is used when an object does not from
    a particular input location. *)
val undefined_position: position

(** {2 Accessors} *)

(** [column p] returns the number of characters from the
    beginning of the line of the Lexing.position [p]. *)
val column : Lexing.position -> int

(** [column p] returns the line number of to the Lexing.position [p]. *)
val line : Lexing.position -> int

(** [characters p1 p2] returns the character interval
    between [p1] and [p2] assuming they are located in the same
    line.
*)
val characters : Lexing.position -> Lexing.position -> int * int

val start_of_position: position -> Lexing.position

val end_of_position: position -> Lexing.position

(** {2 Position handling} *)

(** [join p1 p2] returns a position that starts where [p1]
    starts and stops where [p2] stops. *)
val join : position -> position -> position

val lex_join : Lexing.position -> Lexing.position -> position

val ljoinf : ('a -> position) -> 'a list -> position

val joinf : ('a -> position) -> 'a -> 'a -> position

(** [string_of_characters (c1,c2)] returns the standard (Emacs-like)
    representation of the character interval (c1,c2). *)
val string_of_characters : int * int -> string

(** [string_of_lex_pos p] returns a string representation for
    the lexing position [p]. *)
val string_of_lex_pos : Lexing.position -> string

(** [string_of_pos p] returns the standard (Emacs-like) representation
    of the position [p]. *)
val string_of_pos : position -> string

(** [pos_or_undef po] is the identity function except if po = None,
    in that case, it returns [undefined_position]. *)
val pos_or_undef : position option -> position

(** {2 Interaction with the lexer runtime} *)

(** [cpos lexbuf] returns the current position of the lexer. *)
val cpos : Lexing.lexbuf -> position

(** [string_of_cpos p] returns a string representation of
    the lexer's current position. *)
val string_of_cpos : Lexing.lexbuf -> string
