(*
   Shuyang Li
   Programming Languages
   October 12, 2013
*)

(* Helper functions *)
(* Since we're dealing with many dates in this assignment, I'm binding them at the very beginning, so that they are visible throughout the whole program. *)

fun getYear(date: int * int * int) = #1 date
fun getMonth(date: int * int * int) = #2 date
fun getDate(date: int * int * int) = #3 date

(* 1. *)
fun is_older(date1: int * int * int, date2: int * int * int) = 
    if getYear(date1) < getYear(date2) then true
    else if getYear(date1) > getYear(date2) then false
    else if getMonth(date1) < getMonth(date2) then true
    else if getMonth(date1) > getMonth(date2) then false
    else if getDate(date1) < getDate(date2) then true
    else false

(* 2. *)
fun number_in_month(dates: (int * int * int) list, month: int) = 
    let
	fun traverse_dates_list(datesRest: (int * int * int) list, acc: int) = 
	    if null datesRest then acc
	    else
		if getMonth(hd datesRest) = month
		then traverse_dates_list(tl datesRest, acc + 1)
		else traverse_dates_list(tl datesRest, acc)
    in
	traverse_dates_list(dates, 0)
    end

(* 3. *)
fun number_in_months(dates: (int * int * int) list, months: int list) = 
    let
	fun compare_months(givenMonth: int, currentMonths: int list) = 
	    if null currentMonths then false
	    else
		if givenMonth = (hd currentMonths) then true
		else compare_months(givenMonth, tl currentMonths)

	fun traverse_dates_list(datesRest: (int * int * int) list, acc: int) = 
	    if null datesRest then acc
	    else
		if compare_months(getMonth(hd datesRest), months)
		then traverse_dates_list(tl datesRest, acc + 1)
		else traverse_dates_list(tl datesRest, acc)
    in
	traverse_dates_list(dates, 0)
    end

(* 4. *)
fun dates_in_month(dates: (int * int * int) list, month: int) = 
    let
	fun traverse_dates_list(datesRest: (int * int * int) list, acc: (int * int * int) list) = 
	    if null datesRest then acc
	    else
		if getMonth(hd datesRest) = month
		then traverse_dates_list(tl datesRest, hd datesRest :: acc)
		else traverse_dates_list(tl datesRest, acc)
    in
	traverse_dates_list(dates, [])
    end

(* 5. *)
fun dates_in_months(dates: (int * int * int) list, months: int list) = 
    let
	fun traverse_months_list(remainingMonths: int list, acc: (int * int * int) list) = 
	    if null remainingMonths then acc
	    else traverse_months_list(tl remainingMonths, dates_in_month(dates, hd remainingMonths) @ acc)
    in
	traverse_months_list(months, [])
    end

(* 6. *)
fun get_nth(stringsList: string list, index: int) = 
    if index > 1
    then get_nth(tl stringsList, index - 1)
    else hd stringsList

(* 7. *)
fun date_to_string(date: int * int * int) = 
    let
	val list_of_months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(list_of_months, getMonth(date)) ^ " " ^  Int.toString(getDate(date)) ^ ", " ^ Int.toString(getYear(date))
    end

(* 8. *)
fun number_before_reaching_sum(sum: int, integerList: int list) = 
    let
	fun find_pivot_index(remainingList: int list, acc: int, index: int) = 
	    if acc >= sum
	    then index - 1
	    else find_pivot_index(tl remainingList, (hd remainingList) + acc, index + 1)
    in
	find_pivot_index(integerList, 0, 0)
    end

(* 9. *)
fun what_month(day: int) = 
    let
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, days_in_month) + 1
    end

(* 10. *)
fun month_range(day1: int, day2: int) = 
    if day1 > day2
    then []
    else [what_month(day1)] @ month_range(day1 + 1, day2)

(* 11. *)
fun oldest(dates: (int * int * int) list) = 
    if null dates
    then NONE
    else
	let
	    fun traverse_dates_list(oldestDate: (int * int * int), remainingDates: (int * int * int) list) =
		if null remainingDates
		then SOME oldestDate
		else
		    if is_older(oldestDate, hd remainingDates)
		    then traverse_dates_list(oldestDate, tl remainingDates)
		    else traverse_dates_list(hd remainingDates, tl remainingDates)
	in
	    traverse_dates_list(hd dates, tl dates)
	end
