(* 
Coursera, Programming Languages, Part A, Homework 2
Alexander Moskalev, 2017 
*)


fun same_string(s1 : string, s2 : string) =
    s1 = s2

	     
fun all_except_option( str, str_list )=
  let fun aux( s_l, acc )=
	case s_l of
	    [] => NONE
	  | x::xs' => if same_string( x, str )
		     then SOME (acc @ xs')
		     else aux( xs' , x::acc )
  in
      aux( str_list, [])
  end

      
fun get_substitutions1( str_sub, s )=
  case str_sub of
      [] => []
    | xs'::ys' => (case all_except_option( s, xs' ) of
		      NONE => get_substitutions1( ys', s )
		    | SOME x => x @ get_substitutions1( ys', s ))			       

		      
fun get_substitutions2( str_sub, s )=
  let fun aux( s_sub, acc)=
	case s_sub of
	    [] => acc
	  | xs'::ys' => (case all_except_option( s, xs' ) of
			     NONE => aux( ys', acc )
			   | SOME x => aux( ys', acc @ x ))			       
  in
      aux( str_sub, [] )
  end
      

fun similar_names( str_sub, full_name)=
  case full_name of
      {first,middle,last} =>   let fun aux( str_list, acc, m, l )=
				     case str_list of
					 [] => acc
				       | x::xs' => aux( xs', acc @ [{first=x,middle=m,last=l}], m, l )
			       in
				   full_name::aux( get_substitutions2( str_sub, first ), [], middle, last)
			       end

				   
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      
fun card_color( mycard )=
  case mycard of
      (Clubs,_) => Black
    | (Spades,_) => Black
    | (Diamonds,_) => Red
    | (Hearts,_) => Red 

			
fun card_value( mycard )=
  case mycard of
      (_,Num(x)) => x
    | (_,Ace) => 11
    | (_,_) => 10

		   
fun remove_card( cs, c, e )=
  let fun aux( c_l, acc )=
	case c_l of
	    [] => raise e
	  | x::xs' => if x = c
		     then acc @ xs'
		     else aux( xs' , x::acc )
  in
      aux( cs, [])
  end

      
fun all_same_color( cs )=
  case cs of
      [] => true
    | head::[] => true
    | head::(neck::body) => card_color(head) = card_color(neck) andalso all_same_color(neck::body)

										      
fun sum_cards( cs )=
  let fun aux( c_l, acc )=
	case c_l of
	    [] => acc
	  | x::xs' => aux( xs', acc + card_value(x) )
  in
      aux( cs, 0 )
  end
      

fun score( cs , goal )=
  case (cs,goal) of
      ([],g) => g div 2
    | (xs',g) => let
	val sum = sum_cards(xs')
    in
	if sum > g
	then
	    if all_same_color(xs')
	    then 3 * (sum - g) div 2
	    else 3 * (sum - g)
	else
	    if all_same_color(xs')
	    then (g - sum) div 2
	    else (g - sum)
    end
		     
		
fun officiate( card_list, move_list, goal )=			    
  let
      fun aux( cards, moves, helds)=
	case moves of
	  [] => score( helds, goal )
	 | m::ms' => case m of
			Discard(c) => aux( cards, ms', remove_card( helds, c, IllegalMove ) )
		      | Draw => case cards of
				    [] => score( helds, goal )
				  | c::cs' => if sum_cards( c::helds ) > goal then score( c::helds, goal )
					      else aux( cs', ms', c::helds )
  in
      aux( card_list, move_list, [] )
  end
      
		      
