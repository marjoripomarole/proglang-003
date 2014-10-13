(* Author: Marjori Pomarole *)

fun is_older(date1:(int*int*int), date2:(int*int*int)) =
  if #1 date1 < #1 date2 (* year *)
  then true
  else if #2 date1 < #2 date2 (* month *)
  then true
  else if #3 date1 < #3 date2 (* day *)
  then true
  else false

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
