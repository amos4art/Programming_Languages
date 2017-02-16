(* 
Coursera, Programming Languages, Part A, Homework 3
Alexander Moskalev, 2017 
*)

    
fun only_capitals str_list =
  List.filter (fn x => Char.isUpper(String.sub(x, 0))) str_list 
			      
			       
fun longest_string1 str_list =
  List.foldl (fn (x,acc) => if String.size(x) > String.size(acc) then x else acc) "" str_list
			 

fun longest_string2 str_list =
  List.foldl (fn (x,acc) => if String.size(x) >= String.size(acc) then x else acc) "" str_list
     
  
fun longest_string_helper f = fn str_list => List.foldl ( fn (x,acc) => if f(String.size(x),String.size(acc)) then x else acc ) "" str_list

val longest_string3 = longest_string_helper (fn (x,y) => x > y )

val longest_string4 = longest_string_helper (fn (x,y) => x >= y )
						  
  
val longest_capitalized = longest_string_helper (fn (x,y) => x > y ) o (only_capitals)


val rev_string = fn str => (implode o List.rev o explode) str

							  
exception NoAnswer
							  
							  
val first_answer = fn f => fn a => let val result_list = ((List.filter isSome) o (List.map f)) a
				   in
				       if (not o null) result_list
				       then (valOf o hd) result_list
				       else raise NoAnswer
				   end
				       
											    

val all_answers = fn f => fn a => let  val result_list = (List.concat o (List.map valOf) o (List.filter isSome) o (List.map f)) a
				  in
				      if null result_list andalso (not o null) a
				      then NONE
				      else SOME result_list
				  end


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

				      
fun count_wildcards p = g (fn () => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p = count_wildcards p + g (fn _ => 0) (fn x => size x) p

fun count_some_var (s, p) = g (fn _ => 0) (fn x => if s = x then 1 else 0) p

fun check_pat p = let fun aux1 p' =
			case p' of
			    Wildcard          => []
			  | Variable x        => [x]
			  | TupleP ps         => List.foldl (fn (p,i) => (aux1 p) @ i) [] ps
			  | ConstructorP(_,p) => aux1 p
			  | _                 => []
		      fun aux2 str_list =
			case str_list of
			    [] => true
			  | x::xs' => not (List.exists (fn s => x = s) xs') andalso aux2 xs'  	    
		  in
		      aux2 (aux1 p)		  
		  end

		      
fun match (v, p) =
  case (v, p) of
     (Unit,UnitP) => SOME []
    | (Const i,ConstP v') => if i = v' then SOME []
			     else NONE
    | (Tuple vs, TupleP ps) => if List.length vs = List.length ps then all_answers match (ListPair.zip( vs, ps ))
			       else  NONE
    | (Constructor(s',v'),ConstructorP(cons_name,p')) => if s' = cons_name then match( v', p')
							 else NONE
    | (_,Wildcard) => SOME []
    | (_,Variable s) => SOME [(s,v)]
    | (_,_) => NONE 


val first_match = fn v => fn p_list =>
  SOME (first_answer (fn p => match(v,p)) p_list)
  handle NoAnswer => NONE
	
		      
  
  
  
