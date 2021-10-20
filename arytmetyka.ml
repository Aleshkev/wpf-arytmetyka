(*        Autor: Jonasz Aleszkiewicz *)
(* Weryfikujący: Jakub Adamiak       *)

(* Plik jest formatowany automatycznie przez OCamlFormat. *)

(** [Some (a, b)] gdzie [a], [b] to odpowiednio min. i maks. wartość z [l], lub
    [None] jeśli [l] jest puste. *)
let rec min_max l =
  match l with
  | [] -> None
  | [ x ] -> Some (x, x)
  | x :: tail ->
      let a, b = Option.get (min_max tail) in
      Some (min x a, max x b)

(** Lista z wartościami wygenerowanymi przez [f x y] dla wszystkich [x] w [a]
    oraz [y] w [b]. *)
let map_cartesian_product f a b =
  List.flatten (List.map (fun x -> List.map (fun y -> f x y) b) a)

module Set = struct
  type t = (float * float) list
  (** Zbiór przechowujemy jako listę przedziałów liczb -- jeżeli liczba należy
      do któregoś z przedziałów, należy do zbioru. Wszystkie operacje na zbiorze
      ostatecznie zwracają listę, w której przedziały są posortowane i się nie
      przecinają, ale ten sam typ jest używany do pośrednich obliczeń, gdzie
      niekoniecznie tak jest.

      Treść zadania zezwala tylko na takie operacje, że w zbiorze są najwyżej
      dwa spójne przedziały liczb. *)

  let to_string (s : t) =
    let string_of_interval (a, b) =
      if a == b
      then "{" ^ string_of_float a ^ "}"
      else "[" ^ string_of_float a ^ ", " ^ string_of_float b ^ "]"
    in
    if s == [] then "Ø" else String.concat " ∪ " (List.map string_of_interval s)

  (** Zbiór z jednym elementem. *)
  let singleton x : t = [ (x, x) ]

  (** Spójny przedział liczb. *)
  let interval a b : t = [ (a, b) ]

  (** Sprawdza czy [x] należy do zbioru. *)
  let contains (s : t) x = List.exists (fun (a, b) -> a <= x && x <= b) s

  (** Minimalny element zbioru. *)
  let minimum (s : t) = if s = [] then nan else fst (List.hd s)

  (** Maksymalny element zbioru. *)
  let maximum (s : t) = if s = [] then nan else snd (List.hd (List.rev s))

  (** Dzieli przedziały tak, żeby nie istniał przedział z końcami o różnych
      znakach. *)
  let _segregate_signs (s : t) : t =
    let split_interval (a, b) =
      assert (a <= b);
      if a <= 0. && 0. <= b then [ (a, -0.); (0., b) ] else [ (a, b) ]
    in
    List.flatten (List.map split_interval s)

  (** Łączy przedziały tak, żeby nie istniała para przedziałów zawierających tę
      samą liczbę. *)
  let rec _join_overlapping_intervals (s : t) =
    assert (s = List.sort Stdlib.compare s);
    match s with
    | (a, b) :: (c, d) :: tail ->
        (* jeśli przedziały się przecinają, łączymy je w ich sumę *)
        if a <= d && c <= b
        then _join_overlapping_intervals ((min a c, max b d) :: tail)
        else (a, b) :: _join_overlapping_intervals ((c, d) :: tail)
    | s -> s

  (** Zwraca zbiór z wartościami [f x y] dla [x] ∈ [s], [y] ∈ [z]. Poprawność
      jest udowodniona dla [f] będącego [( +. )], [( -. )], [( *. )], [( /. )];
      [is_div] należy ustawić na [true] jeśli [f] to [( /. )]. *)
  let operation f is_div (s : t) (z : t) =
    (* Zwraca przedział z wartościami [f x y] dla a ≤ x ≤ b, c ≤ y ≤ d; lub
       [None] jeśli nie ma ani jednej takiej wartości. *)
    let intervals_f (a, b) (c, d) =
      if is_div && (c, d) = (0., 0.) (* dzielenie przez 0 nie ma wyniku *)
      then None
      else
        (* Na pałę liczymy wartość f dla wszystkich kombinacji końców, usuwamy
           niezdefiniowane wartości, i mamy wynik. Działa bo a,b mają takie same
           znaki i tak samo c,d, więc można pokazać, że można uzyskać jako
           wartość f wszystkie liczby pomiędzy f(a,c) i f(a,d), pomiędzy f(a,c)
           i f(b,c), pomiędzy f(a,d) i f(b,d) oraz pomiędzy f(b,c) i f(b,d).
           Więc zbiór wartości f(x,y) dla a ≤ x ≤ b ∧ c ≤ y ≤ d jest
           przedziałem, nawet dla dzielenia, z końcami wśród f(a,c), f(a,d), f(a,c), f(b,c).*)
        map_cartesian_product f [ a; b ] [ c; d ]
        |> List.filter (fun x -> not (Float.is_nan x))
        (* bierzemy min. i maks. wartość, czyli końce wynikowego przedziału;
           jeśli z operacji wyszły same [nan]y, teraz usunięte, to będzie to
           [None] *)
        |> min_max
    in
    (* Robimy operację na każdej parze przedziałów liczb i bierzemy sumę
       wyników. *)
    map_cartesian_product intervals_f (_segregate_signs s) (_segregate_signs z)
    (* zamieniamy [Some x] na [x] *)
    |> List.filter Option.is_some
    |> List.map Option.get
    (* sprawdzamy czy wyjście ma sens *)
    |> List.map (fun (a, b) ->
           assert (a <= b);
           assert (Float.is_finite a || Float.is_finite b || a <> b);
           (a, b))
    (* zamieniamy na elegancką formę *)
    |> List.sort Stdlib.compare
    |> _join_overlapping_intervals
end

(* *)

let ( +$ ) = Set.operation ( +. ) false
let ( -$ ) = Set.operation ( -. ) false
let ( *$ ) = Set.operation ( *. ) false
let ( /$ ) = Set.operation ( /. ) true

type wartosc = Set.t

let wartosc_dokladnosc x p =
  let error = abs_float (x /. 100. *. p) in
  Set.interval (x -. error) (x +. error)

let wartosc_od_do = Set.interval
let wartosc_dokladna = Set.singleton
let in_wartosc = Set.contains
let min_wartosc = Set.minimum
let max_wartosc = Set.maximum
let sr_wartosc s = (Set.minimum s +. Set.maximum s) /. 2.
let plus = ( +$ )
let minus = ( -$ )
let razy = ( *$ )
let podzielic = ( /$ )
