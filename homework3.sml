(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1. *)
val only_capitals = List.filter (fn str => (Char.isUpper o String.sub) (str, 0))

(* 2. *)
val longest_string1 =
    List.foldl (fn (str, acc) =>
		   if (String.size str) > (String.size acc)
		   then str
		   else acc) ""
	       
(* 3. *)
val longest_string2 =
    List.foldl (fn (str, acc) =>
		   if (String.size str) >= (String.size acc)
		   then str
		   else acc) ""
	       
(* 4. *)
fun longest_string_helper f =
    List.foldl (fn (str, acc) =>
		   if f (String.size str, String.size acc)
		   then str
		   else acc) ""

val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

(* 5. *)
val longest_capitalized = longest_string1 o only_capitals

(* 6. *)
val rev_string = String.implode o rev o String.explode

(* 7. *)
fun first_answer f lst = 
    case lst of
	[] => raise NoAnswer
      | x :: xs => case f x of
		       SOME v => v
		     | NONE => first_answer f xs

(* 8. *)
fun all_answers f lst =
    let
	fun all_answers_helper remaining acc = 
	    case (remaining, acc) of
		([], _) => acc
	      | (x :: xs, SOME v) => (case f x of
					  NONE => NONE
					| SOME xv => all_answers_helper xs (SOME (xv @ v)))
	      | _ => NONE
    in
	all_answers_helper lst (SOME [])
    end

(* 9. *)
(* a. *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)
(* b. *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size
(* c. *)
fun count_some_var (str, p) = g (fn _ => 0) (fn x =>
						if String.isSubstring str x
						then 1
						else 0) p

(* 10. *)
fun check_pat p =
    let
	fun filterString pat acc = case pat of
				       Variable x => x :: acc
				     | ConstructorP (_, p) => filterString p acc
				     | TupleP ps =>
				       List.foldl
					   (fn (p, acc) => (filterString p []) @ acc) [] ps
				     | _ => []
    in
	let
	    val strList = filterString p []
	    fun checkDuplicate remList = 
		case remList of
		    [] => true
		  | x :: xs => if List.exists (fn item => item = x) xs
			       then false
			       else checkDuplicate xs
	in
	    checkDuplicate strList
	end
    end

(* 11. *)
fun match (v, p) =
    case p of
	Wildcard => SOME []
      | UnitP => (case v of Unit => SOME []
			  | _ => NONE)
      | Variable str => SOME [(str, v)]
      | ConstP i => (case v of Const j => if i = j then SOME [] else NONE
			     | _ => NONE)
      | TupleP plst => (case v of
			    Tuple vlst => if List.length plst = List.length vlst
					  then all_answers match (ListPair.zip (vlst, plst))
					  else NONE
			  | _ => NONE)
      | ConstructorP (str, pt) => (case v of
				       Constructor (vstr, vval) => if str = vstr
								   then match (vval, pt)
								   else NONE
				     | _ => NONE)

(* 12. *)
fun first_match v plst =
    SOME (first_answer (fn p => match (v, p)) plst)
    handle NoAnswer => NONE
