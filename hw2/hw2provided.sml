(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option(str, str_list) = 
  case str_list of
       [] => NONE
     | x::xs =>
         if same_string(str, x)
         then SOME xs
         else
           case all_except_option(str, xs) of
                 NONE => NONE
               | SOME xl => SOME (x::xl)

(* val get_substitutions1 = fn : string list list * string -> string list *)
fun get_substitutions1(substitutions, s) =
  case substitutions of
       [] => []
     |  x::xs =>
         case all_except_option(s, x) of
              NONE => get_substitutions1(xs, s)
            | SOME l => l @ get_substitutions1(xs, s)

fun get_substitutions2(substitutions, s) = get_substitutions1(substitutions, s)

fun similar_names(substitutions, {first:string,middle:string,last:string}) =
  let
    fun aux(subs) =
      case subs of
        [] => []
      | f::fs => {first=f, middle=middle, last=last}::aux(fs)
  in
     {first=first, middle=middle, last=last} :: aux(get_substitutions1(substitutions, first))
  end

fun card_color(suit, rank) =
  case suit of
    Spades => Black
     | Clubs => Black
     | Diamonds => Red
     | Hearts => Red

fun card_value(suit, rank) =
  case rank of
       Num i => i
     | Ace => 11
     | _ => 10

fun remove_card(cards, card, e) =
  case cards of
       [] => []
     | c::cs =>
         if c = card
         then cs
         else c::remove_card(cs, card, e)

fun all_same_color(cards) =
  case cards of
       [] => true
     | x::[] => true
     | x::x1::xs =>
         if card_color(x) = card_color(x1)
         then all_same_color(x1::xs)
         else false

fun sum_cards(cards) =
  let
    fun aux(cards, sum)=
      case cards of
           [] => 0
         | x::xs => aux(xs, sum + card_value(x)) + card_value(x)
  in 
    aux(cards, 0)
  end

fun score(cards, goal) =
  let
    val sum = sum_cards(cards)
  in
    let
      val pre = 
        if sum > goal
        then 3 * (sum - goal)
        else (goal - sum)
    in
      if all_same_color(cards)
      then pre div 2
      else pre
    end
  end

fun officiate(cards_list, moves_list, goal) =
  let 
    fun run(cards, moves, held) =
      if sum_cards(held) > goal
      then score(held, goal)
      else 
        case moves of
             [] => score(held, goal)
           | Draw::ms =>
               let
                 val c::cs = cards
               in
                 run(cs, ms, c::held)
               end
           | (Discard c)::ms => run(cards, ms, remove_card(held, c, IllegalMove))
  in
    run(cards_list, moves_list, [])
  end

