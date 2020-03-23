type frac = { i : int; 
                   num : int; 
                   den : int; };;

let mf2nf m = {i=0;num=m.num+(m.den*m.i);den=m.den};;
(*This will end up returning A converted.*)
(*Only takes normal fraction*)
let sameDen a b = 
  {i=0;num=a.num*b.den;den=a.den*b.den}
;;

let (|+) f1 f2 =
  match (f1.i + f2.i) with 
    0 -> (
      if f1.den <> f2.den then raise (Failure "Fractions must have same denominator");
      {i=0;num=f1.num+f2.num;den=f1.den}
    )
    | _ -> raise (Failure "Cannot process mixed fractions");
and (|-) f1 f2 =
  match (f1.i + f2.i) with 
    0 -> (
      if f1.den <> f2.den then raise (Failure "Fractions must have same denominator");
      {i=0;num=f1.num-f2.num;den=f1.den}
    )
    | _ -> raise (Failure "Cannot process mixed fractions");
;;

let fracsimp f = 
  let rec gcd g l = let m = g mod l in match m with 0 -> l |_ -> gcd l m in
  let nf2mf f= {i=(f.num-(f.num mod f.den))/f.den;num=(f.num mod f.den);den=f.den} in
  let f = nf2mf f in
  {i=f.i;num=f.num/(gcd f.num f.den);den=f.den/(gcd f.num f.den)}
;;

let () =
  let num1 = ref {i=4;num=3;den=5}
  and num2 = ref {i=1;num=9;den=10} in

  num1 := mf2nf !num1;
  num2 := mf2nf !num2;

  let sdnum1 = ref (sameDen !num1 !num2) in
  let sdnum2 = ref (sameDen !num2 !num1) in

  let balthasar = ((!sdnum1 |- !sdnum2)) in

  let romeo = fracsimp balthasar in

  print_string "Whole: ";
  print_int romeo.i;
  print_endline "";
  print_string "Num: ";
  print_int romeo.num;
  print_endline "";
  print_string "Den: ";
  print_int romeo.den;
  print_endline "";
;;