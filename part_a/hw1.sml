(* 
Coursera, Programming Languages, Part A, Homework 1
Alexander Moskalev, 2017 
*)


fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
  if #1 date1 = #1 date2
  then
      if #2 date1 = #2 date2
       then #3 date1 < #3 date2
       else #2 date1 < #2 date2
  else #1 date1 < #1 date2

	   
fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else
      if #2 (hd dates) = month
      then 1 + number_in_month( tl dates, month )
      else number_in_month( tl dates, month )


fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null dates orelse null months
  then 0
  else number_in_month( dates, hd months ) + number_in_months( dates, tl months )

	  
fun dates_in_month ( dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else
      if #2 (hd dates) = month
      then (hd dates) :: dates_in_month( tl dates, month )
      else dates_in_month( tl dates, month )


fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null dates orelse null months
  then []
  else dates_in_month( dates, hd months ) @ dates_in_months( dates, tl months )

    
fun get_nth ( strings : string list, n : int) =
  let
      fun get_n(s : string list, k : int) =
	if null s orelse n < 1
	then
	    ""  (* NONE  -- better solution *)
	else
	    if k = n
	    then hd s  (* then SOME (hd s)  -- better solution*)
	    else get_n( tl s, k + 1 )
  in
    get_n(strings, 1)
  end

    
fun date_to_string ( date: (int * int * int) ) =
  let
      val monthes = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth( monthes, #2 date ) ^ " " ^ Int.toString( #3 date ) ^ ", " ^ Int.toString( #1 date )  (* use valOf in better solution*)
  end
  

fun number_before_reaching_sum ( sum : int, int_list: int list) =
  let
      fun get_list_number ( s : int, int_l: int list, k: int) =
	if s < sum  andalso  s + hd int_l >= sum
	then
	    k
	else
	    get_list_number( s + hd int_l, tl int_l, k + 1)
  in
      get_list_number( 0, int_list, 0) 
  end

      
fun what_month ( day: int ) =
  let
      val days_in_monthes = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
  in
      number_before_reaching_sum( day, days_in_monthes ) + 1
  end


fun month_range ( day1: int, day2: int) =
  if day1 > day2
  then []
  else if day1 = day2
  then [ what_month( day1 ) ]
  else [ what_month( day1 ) ] @ month_range( day1 + 1, day2 )
      

fun oldest ( dates: (int * int * int) list) =
  if null dates
  then
      NONE
  else
      let
	  fun find_oldest_date( d: (int * int * int) list, old_date: (int * int * int) ) =
	    if null d
	    then SOME old_date
	    else if is_older( old_date, hd d )
	    then find_oldest_date( tl d, old_date)
	    else find_oldest_date( tl d, hd d)  		      
      in
	  find_oldest_date( tl dates, hd dates )
      end

	  
(* ADDITIONAL *)
(* add number to already sorted array -> return sorted array and result of operation,
 won't work properly if input array is not sorted *)
fun put_to_sort (array : int list, value : int) = 
  if null array
  then ( [value], true )
  else
      if hd array = value
      then ( array, false )
      else
	  if hd array > value
	  then ( value :: array, true )
	  else
	      let
		  val temp_result = put_to_sort( tl array, value )
	      in ( hd array :: #1 temp_result, #2 temp_result )
	      end

		  
(* added check for month repetitions - they will be ignored *)
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
  let
    fun get_number (d : (int * int * int) list, m : int list, done_m : int list) =
      if null d orelse null m
      then 0
      else
	  let
	      val temp_result =  put_to_sort( done_m, hd m)
	      val done_m = #1 temp_result
	  in
	      if #2 temp_result
	      then
		  number_in_month( d, hd m ) + get_number( d, tl m, done_m )
	      else
		  get_number( d, tl m, done_m )
	  end
  in
    get_number( dates, months, [] )
  end


(* added check for month repetitions - they will be ignored *)
fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
  let
    fun get_list (d : (int * int * int) list, m : int list, done_m : int list) =
      if null d orelse null m
      then []
      else
	  let
	      val temp_result =  put_to_sort( done_m, hd m)
	      val done_m = #1 temp_result
	  in
	      if #2 temp_result
	      then
		  dates_in_month( d, hd m ) @ get_list( d, tl m, done_m )
	      else
		  get_list( d, tl m, done_m )
	  end
  in
    get_list( dates, months, [] )
  end
