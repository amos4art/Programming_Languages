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
      
				     
      

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
