(* ML interpreter / type reconstruction *)
type id = string
type tyvar = int (*型変数の識別子を整数型で表現*)

type binOp = Plus | Mult | Lt | And | Or

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp (* let  式 のコンストラクタの追加。識別子と、束縛する式1,評価する式2から成る。*)
  | FunExp of id * exp (* 関数式のコンストラクタの追加。識別子と式 から成る。*)
  | AppExp of exp * exp (* 関数適用式のコンストラクタの追加。式1と式2から成る。*)
  | LetRecExp of id * id * exp * exp (*let rec 式のコンストラクタの追加。識別子、仮引数、式本体1、評価する式2から成る。*)

type program =
      Exp of exp
    | Decl of id * exp (* let 宣言のコンストラクタの追加。識別子と式 から成る。*)
    | RecDecl of id * id * exp (*let rec　宣言のコンストラクタの追加。識別子、仮引数、式本体から成る。*)
    | Seq of program * program  (*複数の宣言を順次実行するためのSeq型の追加 *)

(* MiniML の型を表す OCaml の値の型 *)
type ty =
    TyInt
  | TyBool
  | TyList of ty 
  | TyVar of tyvar (*型変数を表すコンストラクタ*)
  | TyFun of ty * ty (*関数型 t1 -> t2 を表すコンストラクタ*)

(* ty 型の値を文字列に変換する関数 *)
let rec string_of_ty typ =
  match typ with
  (*TyInt TyBoolはそのまま文字列にする*)
    TyInt -> "int"
  | TyBool -> "bool"
  (*型変数はv=0なら'a', v=1なら'b'... となるようにアルファベットの文字コードを計算*)
  | TyVar v -> 
      let c = Char.chr (97 + (v mod 26)) in
      if v < 26 then 
        Printf.sprintf "'%c" c
      (*26を超えたら a1, b1 ...のように表示*)
      else 
        Printf.sprintf "'%c%d" c (v / 26)
  (* 関数型は t1 -> t2 の形で表示する。t1が関数型の場合は括弧で囲む *)
  | TyFun (t1, t2) ->
      let s1 = string_of_ty t1 in
      let s2 = string_of_ty t2 in
      (match t1 with
       | TyFun _ -> "(" ^ s1 ^ ") -> " ^ s2
       | _ -> s1 ^ " -> " ^ s2)
       | TyList t -> 
      let s = string_of_ty t in 
      
      (match t with
       | TyFun _ -> "(" ^ s ^ ") list"
      | _ -> s ^ " list") 

(* ty 型の値のための pretty printer *)
let pp_ty typ = print_string (string_of_ty typ)

(* 呼び出すたびに，他とかぶらない新しい tyvar 型の値を返す関数 *)
let fresh_tyvar =
  let counter = ref 0 in (* 次に返すべき tyvar 型の値を参照で持っておいて， *)
  let body () =
    let v = !counter in
      counter := v + 1; v (* 呼び出されたら参照をインクリメントして，古い counter の参照先の値を返す *)
  in body

let rec freevar_ty ty =
  match ty with
  (* TyInt TyBoolは型が確定しているため空集合を返す *)
    TyInt -> MySet.empty
  | TyBool -> MySet.empty
  | TyVar v -> MySet.singleton v (* 型変数を見つけたら、その識別子(v)だけが1つ入った集合を作って返す *)
  | TyFun (t1, t2) -> MySet.union (freevar_ty t1) (freevar_ty t2) (*関数型の場合、t1、t2に含まれる自由変数を再帰的に探し、のちに集合を合わせる。*)
  | TyList t -> freevar_ty t  