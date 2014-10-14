(* Author: Marjori Pomarole *)

fun is_older(date1:(int*int*int), date2:(int*int*int)) =
  if #1 date1 = #1 date2 andalso #2 date2 = #2 date1 andalso #3 date1 = #3 date2
  then false
  else if #1 date1 > #1 date2 (* year *)
  then false
  else if #1 date1 = #1 date2 andalso #2 date1 > #2 date2
  then false
  else if #1 date1 = #1 date2 andalso #2 date1 > #2 date2 andalso #3 date1 > #3 date2
  then false
  else true


fun number_in_month(dates:(int * int * int) list, month:int) =
  let fun count(acc:int, ds:(int * int * int) list) =
    if null ds
    then acc
    else if #2 (hd ds) = month
    then count(acc + 1, tl ds)
    else count(acc, tl ds)
  in
    count(0, dates)
  end

fun number_in_months(dates:(int * int * int) list, months:int list) =
  let fun count(acc:int, ms:int list) =
    if null ms
    then acc
    else count(acc + number_in_month(dates, hd ms), tl ms)
  in
    count(0, months)
  end

fun dates_in_month(dates:(int * int * int) list, month:int) =
  let fun listmaker(return:(int * int * int) list, ds:(int * int * int) list) =
    if null ds
    then return
    else if #2 (hd ds) = month
    then listmaker((hd ds) :: return, tl ds)
    else listmaker(return, tl ds)
  in
    listmaker([], dates)
  end

fun dates_in_months(dates:(int * int * int) list, months:int list) =
  let fun listmaker(return:(int * int * int) list, ms:int list) =
    if null ms
    then return
    else listmaker(return @ dates_in_month(dates, hd ms), tl ms)
  in
    listmaker([], months)
  end

fun get_nth(strings:string list, n:int) =
  let fun count(acc: int, ss: string list) = 
    if null ss
    then ""
    else if acc = n
    then hd ss
    else count(acc + 1, tl ss)
  in
    count(1, strings)
  end

val months = ["January", "February", "March", "April", "May", "June", "July",
"August", "September", "October", "November", "December"]

fun date_to_string(date:(int * int * int)) =
  get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^
  Int.toString(#1 date)

fun number_before_reaching_sum(max_sum: int, numbers: int list) = 
  let fun sum(list_sum:int, count:int, ns: int list) =
    if null ns
    then 0
    else if list_sum + (hd ns) >= max_sum
    then count - 1
    else sum(list_sum + (hd ns), count + 1, tl ns)
  in
    sum(0, 1, numbers)
  end

fun what_month(day:int) =
  let 
   val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_month) + 1
  end

fun month_range(day1:int, day2:int) =
  let fun countdown(result: int list, current: int) =
    if current = day1
    then what_month(current) :: result
    else countdown(what_month(current)::result, current - 1)
  in
    countdown([], day2)
  end

fun oldest(dates:(int*int*int) list)=
  if null dates
  then NONE
  else
    let val rest = oldest(tl dates)
    in
      if isSome rest
      then
        if is_older(valOf rest, (hd dates))
        then rest
        else SOME(hd dates)
      else SOME(hd dates)
    end
  
