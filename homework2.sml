(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a. *)
fun all_except_option (inputString, identifyStringList) =
    let
	fun recurse_with_two_lists (currentStringList, pastStringList) = 
	    case currentStringList of
		[] => NONE
	      | x :: x' => if same_string(inputString, x)
			   then SOME (pastStringList @ x')
			   else recurse_with_two_lists(x', x :: pastStringList)
    in
	recurse_with_two_lists (identifyStringList, [])
    end

(* b. *)
fun get_substitutions1 (substitutionStringListList, str) = 
    case substitutionStringListList of
	[] => []
      | x :: x' => case all_except_option (str, x) of 
		       NONE => get_substitutions1 (x', str)
		     | SOME i  => i @ get_substitutions1 (x', str)

(* c. *)
fun get_substitutions2 (substitution, str) = 
    let
	fun get_substitutions_sub (currentStringList, pastStringList) = 
	    case currentStringList of
		[] => pastStringList
	      | x :: x' => case all_except_option (str, x) of
			       NONE => get_substitutions_sub (x', pastStringList)
			     | SOME i => get_substitutions_sub (x', i @ pastStringList)
    in
	get_substitutions_sub (substitution, [])
    end

(* d. *)
(* name of record: {first:string, middle:string, last:string} *)
fun similar_names (substitutions, fullName:{first:string, middle:string, last:string}) =
    let
	val subFirstNames = get_substitutions2 (substitutions, (#first fullName))
    in
	let
	    fun substituteNames (subFirstNameList, allNamesList) =
		case subFirstNameList of
		    [] => allNamesList
		  | x :: x' => substituteNames (x', {first = x, middle = #middle fullName, last = #last fullName} :: allNamesList)
	in
	    substituteNames (subFirstNames, [fullName])
	end
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a. *)
fun card_color (givenCard:card) =
    case (#1 givenCard) of
	Clubs => Black
      | Diamonds => Red
      | Hearts => Red
      | Spades => Black

(* b. *)
fun card_value (givenCard:card) =
    case (#2 givenCard) of
	Num x => x
      | Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11

(* c. *)
fun remove_card (cs, c, e) =
    let
	fun remove_card_sub (curCards, pastCards) =
	    case curCards of
		[] => raise e
	      | x :: x' => if x = c
			   then pastCards @ x'
			   else remove_card_sub (x', x :: pastCards)
    in
	remove_card_sub (cs, [])
    end

(* d. *)
(* It's not a very elegent implementation now, and there should be a better use of pattern matching. *)
fun all_same_color lst =
    let
	val color = case lst of [] => Red 
			      | x :: x' => card_color x
	fun all_same_color_sub (restOfList, commonColor) =
	    case restOfList of
		[] => true
	      | x :: x' => if card_color (x) = commonColor
			   then all_same_color_sub (x', commonColor)
			   else false
    in
	all_same_color_sub (lst, color)
    end

(* e. *)
fun sum_cards lst = 
    let
	fun sum_cards_sub (lst, total) = 
	    case lst of
		[] => total
 	      | x :: x' => sum_cards_sub (x', total + card_value x)
    in
	sum_cards_sub (lst, 0)
    end

(* f. *)
fun score (lst, goal) = 
    let
	val value = sum_cards lst
	val pre_score = if value > goal 
			then 3 * (value - goal) 
			else (goal - value)
    in
	if all_same_color lst
	then pre_score div 2
	else pre_score
    end

(* g. *)
fun officiate (cardList, moveList, goal) =
    let
	fun process_next_move (heldCards, nextMove, nextCards) = 
	    if sum_cards heldCards > goal
	    then score (heldCards, goal)
	    else
		case nextMove of
		    [] => score (heldCards, goal)
		  | x :: x' => case x of
				   Discard i => process_next_move (remove_card (heldCards, i, IllegalMove), x', nextCards)
				 | Draw => case nextCards of 
					       [] => score (heldCards, goal)
					     | y :: y' => process_next_move(y :: heldCards, x', y')
    in
	process_next_move ([], moveList, cardList)
    end
