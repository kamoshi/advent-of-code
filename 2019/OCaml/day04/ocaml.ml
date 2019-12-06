let start = 109165;;
let finish = 576723;;

let range start finish =
	let rec generator curr fin acc =
		if (curr=fin) then List.rev(fin::acc)
		else generator (curr+1) fin (curr::acc)
	in generator start finish [];;

let domain = range 109165 576723;;

let digits2list number =
	let rec aux acc x =
		if x <= 0 then acc
		else aux ((x mod 10)::acc) (x/10)
	in aux [] number;;

let isNotDecreasing number =
	let list = digits2list number
	in fst (List.fold_left (fun (b,p) n -> if n<p then (false,n) else (b,n)) (true, (List.hd list)) (List.tl list));;

let hasAnyPair number =
	let list = digits2list number
	in fst (List.fold_left (fun (b,p) n -> if p=n then (true,n) else (b,n)) (false, (List.hd list)) (List.tl list));;

let predicate number = isNotDecreasing number && hasAnyPair number;;

let filtered = List.filter (fun x -> predicate x) domain;;

let result1 = List.length filtered;; (* part 1 result *)

(* ====================== PART 2 ====================== *)

let predicatePart2 number =
	let list = List.rev(-1::digits2list number)
	in let (_,_,comboList) = List.fold_left (fun (prev, count, acc) next -> 
			if prev=next then (next, count+1, acc)
			else (next, 1, count::acc)
		) ((List.hd list), 1, []) (List.tl list)
		in List.mem 2 comboList;;
		
let lol = predicatePart2 11010343;;	
		
		
let filtered2 = List.filter (fun x -> predicatePart2 x) filtered;;

let result2 = List.length filtered2;;
		
