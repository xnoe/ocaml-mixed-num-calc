open Str;;

type frac = { i : int; 
                   num : int; 
                   den : int; };;

let mf2nf m = {i=0;num=m.num+(m.den*m.i);den=m.den};;
let sameDen a b =
  let ta,tb = !a,!b in 
  a := {i=0;num=ta.num*tb.den;den=ta.den*tb.den};
  b := {i=0;num=tb.num*ta.den;den=tb.den*ta.den}
;;

let (|+) f1 f2 =
  match (f1.i + f2.i) with 
    0 -> (
      if f1.den <> f2.den then raise (Failure "Fractions must have same denominator");
      {i=0;num=f1.num+f2.num;den=f1.den}
    )
    | _ -> raise (Failure "Cannot process mixed fractions");
;;
let (|-) f1 f2 =
  match (f1.i + f2.i) with 
    0 -> (
      if f1.den <> f2.den then raise (Failure "Fractions must have same denominator");
      {i=0;num=f1.num-f2.num;den=f1.den}
    )
    | _ -> raise (Failure "Cannot process mixed fractions");
;;
let (|*) f1 f2 =
  match (f1.i + f2.i) with 
    0 -> (
      {i=0;num=f1.num*f2.num;den=f1.den*f2.den}
    )
    | _ -> raise (Failure "Cannot process mixed fractions");
;;
let (|/) f1 f2 =
  let flipfrac f = {i=0;num=f.den;den=f.num} in
  f1 |* (flipfrac f2);
;;
let (|^) f1 f2 =
  let result = ref f1 in
  for i = 1 to f2.num -1 do
    result := !result |* f1
  done;
  !result
;;

let rec gcd g l = let m = g mod l in match m with 0 -> l |_ -> gcd l m;;

let fracsimp f = 
  let nf2mf f= {i=(f.num-(f.num mod f.den))/f.den;num=(f.num mod f.den);den=f.den} in
  let f = nf2mf f in
  {i=f.i;num=f.num/(gcd f.num f.den);den=f.den/(gcd f.num f.den)}
;;

let frac i n d = {i=i;num=n;den=d};;

type tokens =
  | Number of frac
  | TAdd
  | TSub
  | TMul
  | TDiv
  | TPow
  | LParen
  | RParen
  | EOF
;;

let tokenise prog = 
  let nump1 = regexp "\\([0-9]+\\)" in
  let nump2 = regexp "\\.\\([0-9]+\\)/\\([0-9]+\\)" in
  let index = ref 0 in
  let tokens = ref [] in
  while !index < String.length prog do
    (match prog.[!index] with
      ('0'..'9') -> (
        ignore (search_forward nump1 prog !index);
        let i = int_of_string (matched_group 1 prog) in
        index := match_end ();
        if match_end () < String.length prog && prog.[match_end ()] = '.' then (
          ignore (search_forward nump2 prog !index);
          let num,den = int_of_string (matched_group 1 prog),int_of_string (matched_group 2 prog) in
          tokens := !tokens @ [Number (frac i num den)]
        ) else tokens := !tokens @ [Number (frac i 0 1)];
        index := match_end ()-1;
      )
      | '+' -> tokens := !tokens @ [TAdd]
      | '-' -> tokens := !tokens @ [TSub]
      | '*' -> tokens := !tokens @ [TMul]
      | '/' -> tokens := !tokens @ [TDiv]
      | '^' -> tokens := !tokens @ [TPow]
      | '(' -> tokens := !tokens @ [LParen]
      | ')' -> tokens := !tokens @ [RParen]
      | _ -> ()
    );
    incr index;
  done;
  !tokens @ [EOF]
;;

type oper =
  | Blank
  | OpNum of frac
  | OpAdd of oper * oper
  | OpSub of oper * oper
  | OpMul of oper * oper
  | OpDiv of oper * oper
  | OpPow of oper * oper
;;

type tid =
  | Num | Add | Sub | Mul | Div | Pow | LPa | RPa | EOF
;;

let parse tokens =
  let index = ref 0 in
  let pre () = List.nth tokens (!index-1) in
  let cur () = List.nth tokens !index in
  (*let nex () = List.nth tokens (!index+1) in*)

  let typeOf t =
    match t with
      | Number n -> Num
      | TAdd -> Add
      | TSub -> Sub
      | TMul -> Mul
      | TDiv -> Div
      | TPow -> Pow
      | LParen -> LPa
      | RParen -> RPa
      | EOF -> EOF
  in

  let eatr t = 
    if typeOf (cur ()) = t then (incr index; pre ())
    else raise (Failure "Syntax Error!")
  in

  let eat t =
    if typeOf (cur ()) = t then incr index
    else raise (Failure "Syntax Error!")
  in

  let test t = 
    typeOf (cur ()) = t;
  in

  let rec base () = 
    let result = ref Blank in
    if test Num then result := match eatr Num with Number n -> OpNum n |_->Blank
    else (eat LPa; result := front (); eat RPa; ());
    !result
  and pow () =
    let result = ref (base ()) in
    while test Pow do
      eat Pow;
      result := OpPow(!result,base ());
    done;
    !result
  and inter () =
    let result = ref (pow ()) in
    while test Mul || test Div do
      let t = typeOf (cur ()) in
      eat t;
      if t = Mul then result := OpMul(!result,pow ())
      else result := OpDiv(!result,pow ())
    done;
    !result
  and front () =
    let result = ref (inter ()) in
    while test Add || test Sub do
      let t = typeOf (cur ()) in
      eat t;
      if t = Add then result := OpAdd(!result,inter ())
      else result := OpSub(!result,inter ())
    done;
    !result
  in
  let result = front () in
  eat EOF;
  result
;;

let rec evaltree t = 
  let rec eval = function 
    OpNum f -> mf2nf f
    | OpAdd (a,b) -> (
      let ta,tb = ref (eval a),ref (eval b) in
      sameDen ta tb;
      !ta |+ !tb;
    )
    | OpSub (a,b) -> (
      let ta,tb = ref (eval a),ref (eval b) in
      sameDen ta tb;
      !ta |- !tb;
    )
    | OpMul (a,b) -> (eval a) |* (eval b)
    | OpPow (a,b) -> (eval a) |^ (eval b)
    | OpDiv (a,b) -> (eval a) |/ (eval b)
    | Blank -> raise (Failure "If you are seeing this something has gone seriously wrong")
  in
  fracsimp (eval t)
;;

let () =
  print_endline "Please enter the program you wish to run (`end` to end)";
  print_string "> ";
  let program = ref (read_line ()) in
  while !program <> "end" do
    (try 
      let romeo = evaltree (parse (tokenise !program)) in

      print_string "Answer: ";
      print_int romeo.i;
      print_string ".";
      print_int romeo.num;
      print_string "/";
      print_int romeo.den;
      print_endline ""
    with 
      Failure f -> print_endline ("Failure: " ^ f)
      | Not_found -> print_endline "Elsie you should have fixed this";  
    );
    
    print_endline "Please enter the program you wish to run (`end` to end)";
    print_string "> ";
    program := read_line ()
  done
;;