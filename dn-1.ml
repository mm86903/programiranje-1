(*----------------------------------------------------------------------------*
 # 1. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Ogrevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Collatzovo zaporedje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Collatzovo zaporedje se začne s pozitivnim naravnim številom $a_0$ ter
 nadaljuje kot:

 $$a_{n + 1} = \begin{cases} a_n / 2, & \text{če je } a_n \text{ sodo} \\ 3 a_n
 + 1, & \text{če je } a_n \text{ liho} \end{cases}$$

 Sestavite funkcijo `collatz : int -> int list`, ki sprejme začetni člen
 zaporedja in vrne seznam vseh členov, dokler zaporedje ne doseže $1$.
[*----------------------------------------------------------------------------*)

let rec collatz n = 
  if n = 1 then [1]          (*moramo vrniti seznam!*)
  else
    let naslednji = 
      if n mod 2 = 0 then n / 2
      else 3 * n + 1 
    in
    n :: (collatz naslednji)  

let primer_ogrevanje_1 = collatz 1024
(* val primer_ogrevanje_1 : int list =
  [1024; 512; 256; 128; 64; 32; 16; 8; 4; 2; 1] *)

let primer_ogrevanje_2 = collatz 27
(* val primer_ogrevanje_2 : int list =
  [27; 82; 41; 124; 62; 31; 94; 47; 142; 71; 214; 107; 322; 161; 484; 242;
   121; 364; 182; 91; 274; 137; 412; 206; 103; 310; 155; 466; 233; 700; 350;
   175; 526; 263; 790; 395; 1186; 593; 1780; 890; 445; 1336; 668; 334; 167;
   502; 251; 754; 377; 1132; 566; 283; 850; 425; 1276; 638; 319; 958; 479;
   1438; 719; 2158; 1079; 3238; 1619; 4858; 2429; 7288; 3644; 1822; 911;
   2734; 1367; 4102; 2051; 6154; 3077; 9232; 4616; 2308; 1154; 577; 1732;
   866; 433; 1300; 650; 325; 976; 488; 244; 122; 61; 184; 92; 46; 23; 70; 35;
   106; 53; 160; 80; 40; 20; 10; 5; 16; 8; 4; 2; 1] *)

(*----------------------------------------------------------------------------*
 ### Fiksne točke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `fiksne_tocke : ('a -> 'a) -> 'a list -> 'a list`, ki za
 dano funkcijo in seznam vrne podseznam vseh elementov, ki so fiksne točke.
[*----------------------------------------------------------------------------*)

let fiksne_tocke funkcija sez = 
  List.filter (fun x -> funkcija x = x) sez

let primer_ogrevanje_3 = fiksne_tocke (fun x -> x * x) [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
(* val primer_ogrevanje_3 : int list = [0; 1] *)

let primer_ogrevanje_4 = fiksne_tocke List.rev [[3]; [1; 4; 1]; [5; 9; 2; 6]; [5; 3; 5]; []; [8; 9; 7; 9; 3; 2; 3]]
(* val primer_ogrevanje_4 : int list list = [[3]; [1; 4; 1]; [5; 3; 5]; []] *)

(*----------------------------------------------------------------------------*
 ### Združevanje z ločilom
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sep_concat : 'a -> 'a list list -> 'a list`, ki združi
 seznam seznamov, pri čemer med elemente različnih seznamov ter na začetek in
 konec vstavi dano ločilo.
[*----------------------------------------------------------------------------*)

let sep_concat locilo seznam_seznamov =  
  let dodano = List.map (fun x -> x @ [locilo]) seznam_seznamov in   (*locilo ni seznam!! zato []*)
  locilo :: (List.flatten(dodano)) 
  
  


let primer_ogrevanje_5 = sep_concat 42 [[1; 2; 3]; [4; 5]; []; [6]] 
(* val primer_ogrevanje_5 : int list = [42; 1; 2; 3; 42; 4; 5; 42; 42; 6; 42] *)

(*let primer_ogrevanje_6 = sep_concat 42 [] *)
(* val primer_ogrevanje_6 : int list = [42] *)

(*----------------------------------------------------------------------------*
 ### Razbitje seznama
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `partition : int -> 'a list -> 'a list`, ki sprejme pozitivno
 naravno število $k$ in seznam $[a_0, \dots, a_n]$ ter ga razdeli na zaporedne
 podsezname $[a_0, \dots, a_{k - 1}], [a_k, \dots, a_{2 k - 1}], \dots$, pri
 čemer je zadnji podseznam lahko tudi krajši.
[*----------------------------------------------------------------------------*)

let partition k seznam =
  let rec aux acc trenutni preostali = 
    match preostali with
    | [] -> List.rev (List.rev trenutni :: acc)
    | x :: xs ->
        if List.length trenutni < k then
          aux acc (x :: trenutni) xs
        else
          aux (List.rev trenutni :: acc) [x] xs
  in
  if k <= 0 then [seznam]
  else 
    match seznam with
    | [] -> []
    | _ -> aux [] [] seznam


(*let primer_ogrevanje_7 = partition 3 [1; 2; 3; 4; 5; 6; 7; 8; 9]*)
(* val primer_ogrevanje_7 : int list list = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] *)

(*let primer_ogrevanje_8 = partition 3 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]*)
(* val primer_ogrevanje_8 : int list list =
  [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10]] *)

(*----------------------------------------------------------------------------*
 ## Izomorfizmi množic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Na predavanjih smo videli, da funkciji `curry : ('a * 'b -> 'c) -> ('a -> ('b
 -> 'c))` in `uncurry : ('a -> ('b -> 'c)) -> ('a * 'b -> 'c)` predstavljata
 izomorfizem množic $C^{A \times B} \cong (C^B)^A$, če kartezični produkt
 predstavimo s produktnim, eksponent pa s funkcijskim tipom.

 Podobno velja tudi za ostale znane izomorfizme, če disjunktno unijo
   $$A + B = \{ \mathrm{in}_1(a) \mid a \in A \} \cup \{ \mathrm{in}_2(b) \mid b
 \in B \}$$
 predstavimo s tipom `('a, 'b) sum`, definiranim z:
[*----------------------------------------------------------------------------*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(*----------------------------------------------------------------------------*
 Napišite pare funkcij `phi1` & `psi1`, …, `phi7` & `psi7`, ki predstavljajo
 spodnje izomorfizme množic. Tega, da so si funkcije inverzne, ni treba
 dokazovati.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### $A \times B \cong B \times A$
[*----------------------------------------------------------------------------*)
(*komutativnost produkta*)

let phi1 (a, b) = 
  (b, a)
let psi1 = phi1

(*----------------------------------------------------------------------------*
 ### $A + B \cong B + A$
[*----------------------------------------------------------------------------*)
(*komutativnost sestevanja*)

let phi2 nekaj_iz_a_plus_b =
  match nekaj_iz_a_plus_b with
  | In1 a -> In2 a 
  | In2 b -> In1 b


let psi2 = phi2

(*----------------------------------------------------------------------------*
 ### $A \times (B \times C) \cong (A \times B) \times C$
[*----------------------------------------------------------------------------*)


let phi3 (a, (b, c)) = ((a, b), c)

let psi3 ((a, b), c) = (a, (b, c))

(*----------------------------------------------------------------------------*
 ### $A + (B + C) \cong (A + B) + C$
[*----------------------------------------------------------------------------*)

let phi4 = 
  function
  | In1 a -> In1 (In1 a)
  | In2 (In1 b) -> In1 (In2 b)
  | In2 (In2 c) -> In2 c


let psi4 = 
  function
  | In1 (In1 a) -> In1 a
  | In1 (In2 b) -> In2 (In1 b)
  | In2 c -> In2 (In2 c)


(*----------------------------------------------------------------------------*
 ### $A \times (B + C) \cong (A \times B) + (A \times C)$
[*----------------------------------------------------------------------------*)

let phi5 (x, s) = 
  match s with
  | In1 b -> In1 (x, b)
  | In2 c -> In2 (x, c)

let psi5 = 
  function
  | In1 (x, b) -> (x, In1 b)
  | In2 (x, c) -> (x, In2 c)

(*----------------------------------------------------------------------------*
 ### $A^{B + C} \cong A^B \times A^C$
[*----------------------------------------------------------------------------*)

let psi6 ((f1, f2) : ('b -> 'a) * ('c -> 'a)) : ('b, 'c) sum -> 'a = 
  function
  | In1 b -> f1 b
  | In2 c -> f2 c

let phi6 (f : ('b, 'c) sum -> 'a) : ('b -> 'a) * ('c -> 'a) =
  let f1 b = f (In1 b) in
  let f2 c = f (In2 c) in
  (f1, f2)
(*----------------------------------------------------------------------------*
 ### $(A \times B)^C \cong A^C \times B^C$
[*----------------------------------------------------------------------------*)

let phi7 (f : 'c -> 'a * 'b) : ('c -> 'a) * ('c -> 'b) =
  let f1 c = fst (f c) in
  let f2 c = snd (f c) in
  (f1, f2)


let psi7 ((f1, f2) : ('c -> 'a) * ('c -> 'b)) : 'c -> ('a * 'b) =
  fun c -> (f1 c, f2 c)

(*----------------------------------------------------------------------------*
 ## Permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Permutacije so preureditve elementov $\{0, 1, \dots, n-1\}$, torej bijektivne
 preslikave $$p \colon \{0, 1, \dots, n-1\} \to \{0, 1, \dots, n-1\}.$$ V nalogi
 bomo permutacije predstavili s seznamom števil, v katerem je na $i$-tem mestu
 seznama zapisana slika elementa $i$.
 Na primer, permutaciji $0 \, 1 \, 2 \, 3 \, 4 \, 5 \choose 5 \, 3 \, 2 \, 1 \,
 4 \, 0$ in $0 \, 1 \, 2 \, 3 \, 4 \, 5 \, 6 \, 7 \, 8 \, 9 \choose 3 \, 9 \, 1
 \, 7 \, 5 \, 4 \, 6 \, 8 \, 2 \, 0$ bi zapisali s seznamoma:
[*----------------------------------------------------------------------------*)

let permutacija_1 = [5; 3; 2; 1; 4; 0]
let permutacija_2 = [3; 9; 1; 7; 5; 4; 6; 8; 2; 0]
(* val permutacija_1 : int list = [5; 3; 2; 1; 4; 0] *)
(* val permutacija_2 : int list = [3; 9; 1; 7; 5; 4; 6; 8; 2; 0] *)

(*----------------------------------------------------------------------------*
 ### Kompozitum
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `kompozitum : int list -> int list -> int list`, ki sprejme
 dve permutaciji in vrne njun kompozitum. Za permutaciji $p$ in $q$, je njun
 kompozitum funkcija

 $$ p \circ q \colon i \mapsto p ( q ( i ) ). $$

 Predpostavite lahko, da sta seznama ENAKIH dolžin.
[*----------------------------------------------------------------------------*)

let kompozitum sez1 sez2 = 
  List.init (List.length sez1) (fun i -> List.nth sez1 (List.nth sez2 i))

let primer_permutacije_1 = kompozitum permutacija_1 permutacija_1
(* val primer_permutacije_1 : int list = [0; 1; 2; 3; 4; 5] *)

let primer_permutacije_2 = kompozitum permutacija_2 permutacija_2 
(* val primer_permutacije_2 : int list = [7; 0; 9; 8; 4; 5; 6; 2; 1; 3] *)

(*----------------------------------------------------------------------------*
 ### Inverz
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napiši funkcijo `inverz : int list -> int list`, ki vrne inverz dane
 permutacije $p$, torej tako permutacijo $p^{-1}$, da velja $$p \circ p^{-1} =
 \mathrm{id},$$ kjer je $\mathrm{id}$ indentiteta.
[*----------------------------------------------------------------------------*)

let inverz per = 
  let n = List.length per in
  let urejen_seznam = List.init n (fun i -> i) in
  List.init n (fun i -> List.find (fun x -> List.nth per x = i ) urejen_seznam)

let primer_permutacije_3 = inverz permutacija_1
(* val primer_permutacije_3 : int list = [5; 3; 2; 1; 4; 0] *)

let primer_permutacije_4 = inverz permutacija_2
(* val primer_permutacije_4 : int list = [9; 2; 8; 0; 5; 4; 6; 3; 7; 1] *)

(*let primer_permutacije_5 = kompozitum permutacija_2 (inverz permutacija_2)*)
(* val primer_permutacije_5 : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)

(*----------------------------------------------------------------------------*
 ### Razcep na cikle
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `cikli : int list -> int list list`, ki za dano permutacijo
 vrne seznam ciklov, ki to permutacijo sestavljajo. Vsak element $\{0, 1, \dots,
 n-1\}$ naj se pojavi v natanko enem ciklu.
[*----------------------------------------------------------------------------*)

let cikli per =
  let n = List.length per in
  let per_array = Array.of_list per in
  let visited = Array.make n false in
  let rec aux i acc =
    if i = n then
      List.rev acc
    else if visited.(i) then
      aux (i + 1) acc
    else
      let rec naredi_cikel current cikel =
        if visited.(current) then
          cikel
        else begin
          visited.(current) <- true;
          naredi_cikel per_array.(current) (current :: cikel)
        end
      in
      let cikel = naredi_cikel i [] in
      aux (i + 1) (List.rev cikel :: acc)
  in
  aux 0 []


let primer_permutacije_6 = cikli permutacija_1
(* val primer_permutacije_6 : int list list = [[0; 5]; [1; 3]; [2]; [4]] *)

let primer_permutacije_7 = cikli permutacija_2
(* val primer_permutacije_7 : int list list =
  [[0; 3; 7; 8; 2; 1; 9]; [4; 5]; [6]] *)

let primer_permutacije_8 = cikli (inverz permutacija_2)
(* val primer_permutacije_8 : int list list =
  [[0; 9; 1; 2; 8; 7; 3]; [4; 5]; [6]] *)

(*----------------------------------------------------------------------------*
 ### Transpozicije permutacije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Vsako permutacijo lahko zapišemo kot produkt transpozicij, torej menjav dveh
 elementov. Na primer, permutacijo $0 \, 1 \, 2 \, 3 \choose 1 \, 0 \, 3 \, 2$
 dobimo kot produkt transpozicij $(0, 1) \circ (2, 3)$.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `iz_transpozicij : int -> (int * int) list -> int list`, ki
 sprejme dolžino permutacije in seznam transpozicij ter vrne permutacijo, ki jim
 ustreza.
[*----------------------------------------------------------------------------*)
(*let zamenjaj_element seznam i nova_vrednost =
  List.mapi (fun indeks element ->
    if indeks = i then
      nova_vrednost
    else
      element
  ) seznam*)

let iz_transpozicij n (seznam_transpozicij : (int * int) list) = 
  let urejena_per = Array.init n (fun i -> i) in
  List.iter (fun (a,b) ->
    let vrednost = urejena_per.(a) in             (* ce brez tega naredimo, izgubimo podatek*)
    urejena_per.(a) <- urejena_per.(b);
    urejena_per.(b) <- vrednost) seznam_transpozicij;
  Array.to_list urejena_per


let primer_permutacije_9 = iz_transpozicij 4 [(0, 1); (2, 3)]
(* val primer_permutacije_9 : int list = [1; 0; 3; 2] *)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `v_transpozicije : int list -> (int * int) list`, ki zapiše
 permutacijo kot produkt transpozicij, torej menjav dveh elementov. Možnih
 produktov je več, veljati mora le, da je kompozicija dobljenih ciklov enaka
 prvotni permutaciji.

 *Namig: Pomagate si lahko z lastnostjo, da poljubni cikel razpade na
 transpozicije po naslednji formuli*
 $$(i_1, i_2, i_3, \ldots, i_{k-1}, i_k) = (i_1, i_k)\circ(i_1,
 i_{k-1})\circ(i_1, i_3)\circ(i_1, i_2).$$
[*----------------------------------------------------------------------------*)

let v_transpozicije per =
  let razbij_cikel_na_transpozicije cikel =
    match cikel with
    | [] | [_] -> [] 
    | i1 :: ostalo ->
        List.rev_map (fun x -> (i1, x)) (List.tl cikel)
  in
  per
  |> cikli                          
  |> List.map razbij_cikel_na_transpozicije  
  |> List.flatten                   
  
let primer_permutacije_10 = v_transpozicije permutacija_1
(* val primer_permutacije_10 : (int * int) list = [(0, 5); (1, 3)] *)

let primer_permutacije_11 = v_transpozicije permutacija_2
(* val primer_permutacije_11 : (int * int) list =
  [(0, 9); (0, 1); (0, 2); (0, 8); (0, 7); (0, 3); (4, 5)] *)

(*----------------------------------------------------------------------------*
 ## Sudoku
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sudoku je igra, v kateri mrežo $9 \times 9$ dopolnimo s števili od $1$ do $9$,
 tako da se nobeno število v nobeni vrstici, stolpcu ali eni od devetih škatel
 velikosti $3 \times 3$ ne ponovi. Primer začetne postavitve in ustrezne rešitve
 je:

 ```plaintext
 +-------+-------+-------+       +-------+-------+-------+
 | 5 4 . | . 7 . | . . . |       | 5 4 3 | 6 7 8 | 9 1 2 |
 | 6 . . | 1 9 5 | . . . |       | 6 7 2 | 1 9 5 | 3 4 8 |
 | . 9 8 | . . . | . 6 . |       | 1 9 8 | 3 4 2 | 5 6 7 |
 +-------+-------+-------+       +-------+-------+-------+
 | 8 . . | . 6 . | . . 3 |       | 8 1 9 | 7 6 4 | 2 5 3 |
 | 4 . . | 8 . 3 | . . 1 |       | 4 2 6 | 8 5 3 | 7 9 1 |
 | 7 . . | . 2 . | . . 6 |       | 7 3 5 | 9 2 1 | 4 8 6 |
 +-------+-------+-------+       +-------+-------+-------+
 | . 6 . | . . 7 | 8 . . |       | 9 6 1 | 5 3 7 | 8 2 4 |
 | . . . | 4 1 9 | . . 5 |       | 2 8 7 | 4 1 9 | 6 3 5 |
 | . . . | . 8 . | . 7 9 |       | 3 5 4 | 2 8 6 | 1 7 9 |
 +-------+-------+-------+       +-------+-------+-------+
 ```
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Delno izpolnjen sudoku bomo predstavili s tabelo tabel tipa `int option array
 array`, kjer bomo prazna mesta označili z `None`, rešen sudoku pa s tabelo
 tabel običajnih števil.
[*----------------------------------------------------------------------------*)

type mreza = int option array array
type resitev = int array array

(*----------------------------------------------------------------------------*
 Na primer, zgornjo mrežo in rešitev bi predstavili s seznamoma:
[*----------------------------------------------------------------------------*)

let primer_mreze : mreza = [|
  [|Some 5; Some 4; None;   None;   Some 7; None;   None;   None;   None|];
  [|Some 6; None;   None;   Some 1; Some 9; Some 5; None;   None;   None|];
  [|None;   Some 9; Some 8; None;   None;   None;   None;   Some 6; None|];

  [|Some 8; None;   None;   None;   Some 6; None;   None;   None;   Some 3|];
  [|Some 4; None;   None;   Some 8; None;   Some 3; None;   None;   Some 1|];
  [|Some 7; None;   None;   None;   Some 2; None;   None;   None;   Some 6|];
  [|None;   Some 6; None;   None;   None;   Some 7; Some 8; None;   None|];
  [|None;   None;   None;   Some 4; Some 1; Some 9; None;   None;   Some 5|];
  [|None;   None;   None;   None;   Some 8; None;   None;   Some 7; Some 9|]
|]

let primer_resitve : resitev = [|
  [|5; 4; 3; 6; 7; 8; 9; 1; 2|];
  [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
  [|1; 9; 8; 3; 4; 2; 5; 6; 7|];
  [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
  [|4; 2; 6; 8; 5; 3; 7; 9; 1|];
  [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
  [|9; 6; 1; 5; 3; 7; 8; 2; 4|];
  [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
  [|3; 5; 4; 2; 8; 6; 1; 7; 9|];
|]
(* val primer_mreze : mreza =
  [|[|Some 5; Some 4; None; None; Some 7; None; None; None; None|];
    [|Some 6; None; None; Some 1; Some 9; Some 5; None; None; None|];
    [|None; Some 9; Some 8; None; None; None; None; Some 6; None|];
    [|Some 8; None; None; None; Some 6; None; None; None; Some 3|];
    [|Some 4; None; None; Some 8; None; Some 3; None; None; Some 1|];
    [|Some 7; None; None; None; Some 2; None; None; None; Some 6|];
    [|None; Some 6; None; None; None; Some 7; Some 8; None; None|];
    [|None; None; None; Some 4; Some 1; Some 9; None; None; Some 5|];
    [|None; None; None; None; Some 8; None; None; Some 7; Some 9|]|] *)
(* val primer_resitve : resitev =
  [|[|5; 4; 3; 6; 7; 8; 9; 1; 2|]; [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
    [|1; 9; 8; 3; 4; 2; 5; 6; 7|]; [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
    [|4; 2; 6; 8; 5; 3; 7; 9; 1|]; [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
    [|9; 6; 1; 5; 3; 7; 8; 2; 4|]; [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
    [|3; 5; 4; 2; 8; 6; 1; 7; 9|]|] *)

(*----------------------------------------------------------------------------*
 ### Dopolnitev mreže
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `dodaj : int -> int -> int -> mreza -> mreza` tako da `dodaj
 i j n m` vrne mrežo, ki je povsod enaka mreži `m`, le na mestu v vrstici `i` in
 stolpcu `j` ima zapisano število `n`.

 **Pozor:** OCaml dopušča spreminjanje tabel (o tem se bomo učili kasneje). Vaša
funkcija naj te možnosti ne uporablja, temveč naj sestavi in vrne novo tabelo.!!!!!!
[*----------------------------------------------------------------------------*)

(* Poizkus z spreminjanjm tabele
let dodaj i j n m = 
  let nova_mreza = Array.map Array.copy m in
  nova_mreza.(i).(j) <- Some n;               (*to vrne unit*)
  nova_mreza                                                                       *)

let dodaj i j n m = 
  Array.mapi (fun indeks_vrstice vrstica -> 
    Array.mapi (fun indeks_stoplca x ->
      if indeks_vrstice = i && indeks_stoplca = j then Some n
      else x
    ) vrstica
  )m
 

let primer_sudoku_1 = primer_mreze |> dodaj 0 8 2
(* val primer_sudoku_1 : mreza =
  [|[|Some 5; Some 4; None; None; Some 7; None; None; None; Some 2|];
    [|Some 6; None; None; Some 1; Some 9; Some 5; None; None; None|];
    [|None; Some 9; Some 8; None; None; None; None; Some 6; None|];
    [|Some 8; None; None; None; Some 6; None; None; None; Some 3|];
    [|Some 4; None; None; Some 8; None; Some 3; None; None; Some 1|];
    [|Some 7; None; None; None; Some 2; None; None; None; Some 6|];
    [|None; Some 6; None; None; None; Some 7; Some 8; None; None|];
    [|None; None; None; Some 4; Some 1; Some 9; None; None; Some 5|];
    [|None; None; None; None; Some 8; None; None; Some 7; Some 9|]|] *)

(*----------------------------------------------------------------------------*
 ### Izpiši mrežo
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkciji `izpis_mreze : mreza -> string` in `izpis_resitve : resitev
 -> string`, ki sprejmeta mrežo oziroma rešitev in vrneta niz, ki predstavlja
 izpis v zgornji obliki.
[*----------------------------------------------------------------------------*)

let string_of_option  = function
  | Some n -> string_of_int n
  | None -> "."

let naredi_celico element =
  (string_of_option element) ^ " " 
  

let naredi_vrstico vrstica =
  let elementi = Array.mapi (fun j element -> naredi_celico element) vrstica in
  String.concat "" [
    "| ";
    elementi.(0); elementi.(1); elementi.(2); "| ";
    elementi.(3); elementi.(4); elementi.(5); "| ";
    elementi.(6); elementi.(7); elementi.(8); "| ";
  ]


let izpis_mreze mreza = 
  let prva_vrstica = "+-------+-------+-------+\n" in
  let vrstice = Array.mapi (fun i vrstica -> naredi_vrstico vrstica ^ "\n") mreza in
  String.concat "" [
    prva_vrstica;
    vrstice.(0); vrstice.(1); vrstice.(2);
    prva_vrstica;
    vrstice.(3); vrstice.(4); vrstice.(5);
    prva_vrstica;
    vrstice.(6); vrstice.(7); vrstice.(8);
    prva_vrstica
  ]


let primer_sudoku_2 = primer_mreze |> izpis_mreze |> print_endline
(* 
  +-------+-------+-------+
  | 5 4 . | . 7 . | . . . |
  | 6 . . | 1 9 5 | . . . |
  | . 9 8 | . . . | . 6 . |
  +-------+-------+-------+
  | 8 . . | . 6 . | . . 3 |
  | 4 . . | 8 . 3 | . . 1 |
  | 7 . . | . 2 . | . . 6 |
  +-------+-------+-------+
  | . 6 . | . . 7 | 8 . . |
  | . . . | 4 1 9 | . . 5 |
  | . . . | . 8 . | . 7 9 |
  +-------+-------+-------+
  
  val primer_sudoku_2 : unit = ()
*)

let naredi_celico_2 element =
  (string_of_int element) ^ " " 
  

let naredi_vrstico_2 vrstica =
  let elementi = Array.mapi (fun j element -> naredi_celico_2 element) vrstica in
  String.concat "" [
    "| ";
    elementi.(0); elementi.(1); elementi.(2); "| ";
    elementi.(3); elementi.(4); elementi.(5); "| ";
    elementi.(6); elementi.(7); elementi.(8); "| ";
  ]


let izpis_resitve mreza = 
  let prva_vrstica = "+-------+-------+-------+\n" in
  let vrstice = Array.mapi (fun i vrstica -> naredi_vrstico_2 vrstica ^ "\n") mreza in
  String.concat "" [
    prva_vrstica;
    vrstice.(0); vrstice.(1); vrstice.(2);
    prva_vrstica;
    vrstice.(3); vrstice.(4); vrstice.(5);
    prva_vrstica;
    vrstice.(6); vrstice.(7); vrstice.(8);
    prva_vrstica
  ]


let primer_sudoku_3 = primer_resitve |> izpis_resitve |> print_endline
(*
  +-------+-------+-------+
  | 5 4 3 | 6 7 8 | 9 1 2 |
  | 6 7 2 | 1 9 5 | 3 4 8 |
  | 1 9 8 | 3 4 2 | 5 6 7 |
  +-------+-------+-------+
  | 8 1 9 | 7 6 4 | 2 5 3 |
  | 4 2 6 | 8 5 3 | 7 9 1 |
  | 7 3 5 | 9 2 1 | 4 8 6 |
  +-------+-------+-------+
  | 9 6 1 | 5 3 7 | 8 2 4 |
  | 2 8 7 | 4 1 9 | 6 3 5 |
  | 3 5 4 | 2 8 6 | 1 7 9 |
  +-------+-------+-------+

  val primer_sudoku_3 : unit = ()
*)

(*----------------------------------------------------------------------------*
 ### Preveri, ali rešitev ustreza mreži
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `ustreza : mreza -> resitev -> bool`, ki preveri, ali rešitev
 ustreza dani mreži. Rešitev ustreza mreži, če se na vseh mestih, kjer je v
 mreži podana številka, v rešitvi nahaja enaka številka.
[*----------------------------------------------------------------------------*)


let ustreza (m : mreza) (r : resitev) =                                                (*prisilimo tipe*)
  let sploscen_m = Array.of_list (List.concat_map Array.to_list (Array.to_list m)) in
  let sploscen_r = Array.of_list (List.concat_map (fun vrstica -> Array.to_list (Array.map (fun x -> Some x) vrstica)) (Array.to_list r)) in
  Array.for_all2 (fun prvi drugi ->                                            
    match prvi with
    | Some n -> drugi = Some n
    | None -> true)
    sploscen_m sploscen_r

let primer_sudoku_4 = ustreza primer_mreze primer_resitve
(* val primer_sudoku_4 : bool = true *)

(*----------------------------------------------------------------------------*
 ### Kandidati za dano prazno mesto
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcije `ni_v_vrstici`, `ni_v_stolpcu` in `ni_v_skatli`, vse tipa
 `mreza * int -> int -> bool`, ki preverijo, ali se v določeni vrstici, stolpcu
 oziroma škatli mreže ne nahaja dano število. Vrstice, stolpci in škatle so
 indeksirani kot:

 ```plaintext
     0 1 2   3 4 5   6 7 8
   +-------+-------+-------+
 0 |       |       |       |
 1 |   0   |   1   |   2   |
 2 |       |       |       |
   +-------+-------+-------+
 3 |       |       |       |
 4 |   3   |   4   |   5   |
 5 |       |       |       |
   +-------+-------+-------+
 6 |       |       |       |
 7 |   6   |   7   |   8   |
 8 |       |       |       |
   +-------+-------+-------+
 ```
[*----------------------------------------------------------------------------*)

let ni_v_vrstici ((m : mreza), (v : int)) (n : int) : bool =
  let vrstica = m.(v) in
  Array.for_all (fun element -> element <> Some n) vrstica



let primer_sudoku_5 = ni_v_vrstici (primer_mreze, 0) 1
(* val primer_sudoku_5 : bool = true *)

let primer_sudoku_6 = ni_v_vrstici (primer_mreze, 1) 1
(* val primer_sudoku_6 : bool = false *)


let ni_v_stolpci ((m : mreza), (s : int)) (n : int) : bool =
   Array.for_all (fun vrstica -> 
    let element = vrstica.(s) in
    element <> Some n
    ) m


let ni_v_skatli ((m : mreza), (indeks_skatle : int)) (n : int) : bool =
  let i = (indeks_skatle / 3) * 3 in
  let j = (indeks_skatle mod 3) * 3 in
  let skatla = Array.init 9 (fun k -> 
    let ri = i + (k / 3) in
    let rj = j + (k mod 3) in
    m.(ri).(rj)
    ) in
    Array.for_all (fun x -> 
      match x with
      | Some v -> v <> n
      | None -> true) skatla

(*----------------------------------------------------------------------------*
 Napišite funkcijo `kandidati : mreza -> int -> int -> int list option`, ki
 sprejme mrežo in indeksa vrstice in stolpca praznega mesta ter vrne seznam vseh
 številk, ki se lahko pojavijo na tem mestu. Če je polje že izpolnjeno, naj
 funkcija vrne `None`.
[*----------------------------------------------------------------------------*)

let kandidati (m : mreza) (i: int) (j : int) : int list option =
  match m.(i).(j) with
  | Some _ -> None
  | None ->
    let indeks_skatle = (i / 3) * 3 + (j / 3) in
    let vsi_kandidati = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let veljavni_kandidati = List.filter (fun x ->
      ni_v_vrstici (m, i) x &&
      ni_v_stolpci (m, j) x &&
      ni_v_skatli (m, indeks_skatle) x
      ) vsi_kandidati in
      Some veljavni_kandidati
      


let primer_sudoku_7 = kandidati primer_mreze 0 2
(* val primer_sudoku_7 : int list option = Some [1; 2; 3] *)

let primer_sudoku_8 = kandidati primer_mreze 0 0
(* val primer_sudoku_8 : int list option = None *)

(*----------------------------------------------------------------------------*
 ### Iskanje rešitve
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `resi : mreza -> resitev option`, ki izpolni mrežo sudokuja.
 Če je dana mreža rešljiva, mora funkcija najti rešitev, ki ustreza začetni
 mreži in jo vrniti v obliki `Some resitev`, sicer naj vrne `None`.
 Predpostavite lahko, da je rešitev enolična, zato lahko funkcija vrne prvo, ki
 jo najde.

 *Namig: Poiščite celico mreže z najmanj kandidati in rekurzivno preizkusite vse
 možnosti.*
[*----------------------------------------------------------------------------*)

let rec resi (m : mreza) : resitev option =
  let poisci_prvo_prazno m =
    let rec loop i j =
      if i = 9 then None   (* smo ze presegli vse vrstice *)
      else if j = 9 then loop (i + 1) 0
      else if m.(i).(j) = None then Some (i, j)
      else loop i (j + 1)
    in
    loop 0 0
  in

  match poisci_prvo_prazno m with
  | None ->  (* mreza je polna *)
      let resitev =
        Array.map (fun vrstica ->
          Array.map (function
            | Some x -> x
            | None -> failwith "Nemogoče"
          ) vrstica ) m
      in
      Some resitev

  | Some (i, j) ->   (* imamo neko mesto *)
      match kandidati m i j with
      | None -> None
      | Some [] -> None
      | Some mozni ->
          let rec poskusi = function
            | [] -> None
            | x :: xs ->
                let nova_mreza = dodaj i j x m in   (* neko novo mrezo naredimo*)
                match resi nova_mreza with
                | Some r -> Some r
                | None -> poskusi xs
          in
          poskusi mozni


let primer_sudoku_9 = resi primer_mreze
(* val primer_sudoku_9 : resitev option =
  Some
   [|[|5; 4; 3; 6; 7; 8; 9; 1; 2|]; [|6; 7; 2; 1; 9; 5; 3; 4; 8|];
     [|1; 9; 8; 3; 4; 2; 5; 6; 7|]; [|8; 1; 9; 7; 6; 4; 2; 5; 3|];
     [|4; 2; 6; 8; 5; 3; 7; 9; 1|]; [|7; 3; 5; 9; 2; 1; 4; 8; 6|];
     [|9; 6; 1; 5; 3; 7; 8; 2; 4|]; [|2; 8; 7; 4; 1; 9; 6; 3; 5|];
     [|3; 5; 4; 2; 8; 6; 1; 7; 9|]|] *)
