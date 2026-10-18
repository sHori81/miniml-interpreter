open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref (*パラメータ名、関数本体の式に加え、関数作成時の環境を含める。*)
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  (*論理積の演算*)
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  (*論理和の演算*)
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")
  

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))

  (*int,bool型の評価*)
  | ILit i -> IntV i
  | BLit b -> BoolV b

  (* 論理積(&&)のショートサーキット評価 *)
  | BinOp (And, exp1, exp2) ->
      (match eval_exp env exp1 with
         BoolV false -> BoolV false (* 左辺がfalseなら、右辺(exp2)は評価せずfalseを返す *)
       | BoolV true -> 
           (* 左辺がtrueだった時だけ、初めて右辺を評価する *)
           (match eval_exp env exp2 with
              BoolV b -> BoolV b
            | _ -> err ("Right argument must be boolean: &&"))
       | _ -> err ("Left argument must be boolean: &&"))

  (* 論理和(||)のショートサーキット評価 *)
  | BinOp (Or, exp1, exp2) ->
      (match eval_exp env exp1 with
         BoolV true -> BoolV true (* 左辺がtrueなら、右辺(exp2)は評価せずにtrueを返す *)
       | BoolV false -> 
           (* 左辺がfalseだった時だけ、初めて右辺を評価する *)
           (match eval_exp env exp2 with
              BoolV b -> BoolV b
            | _ -> err ("Right argument must be boolean: ||"))
       | _ -> err ("Left argument must be boolean: ||"))

  (*二項演算子の評価*)
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2

  (*if式の評価*)
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))

  (* let 式の評価*)
  | LetExp (id, exp1, exp2) -> 
     (* 現在の環境で exp1 を評価 *)
     let value = eval_exp env exp1 in
     (* exp1 の評価結果を id の値として環境に追加して exp2 を評価 *)
     eval_exp (Environment.extend id value env) exp2

  (* 関数式の評価*)
  | FunExp (id, exp) -> ProcV (id, exp, ref env) (*クロージャの作成*)

  (* 関数適用式 *)
  | AppExp (exp1, exp2) ->
      (* 関数 exp1 を現在の環境で評価 *)
      let funval = eval_exp env exp1 in
      (* 実引数 exp2 を現在の環境で評価 *)
      let arg = eval_exp env exp2 in
      (* 関数 exp1 の評価結果をパターンマッチで取り出す *)
      (match funval with
          ProcV (id, body, env') -> (* 評価結果が実際にクロージャであれば *)
              (* クロージャ内の環境を取り出して仮引数に対する束縛で拡張 *)
              let newenv = Environment.extend id arg !env' in
                eval_exp newenv body
        | _ ->
          (* 評価結果がクロージャでなければ，実行時型エラー *)
          err ("Non-function value is applied"))

  (*再帰関数の評価*)
  | LetRecExp (id, para, exp1, exp2) ->
    (* ダミーの環境への参照を作る *)
    let dummyenv = ref Environment.empty in
    (* 関数閉包を作り，idをこの関数閉包に写像するように現在の環境envを拡張 *)
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
    (* ダミーの環境への参照に，拡張された環境を破壊的代入してバックパッチ *)
        dummyenv := newenv;
        eval_exp newenv exp2
  
let rec eval_decl env = function (*再帰式に変更*)
    Exp e -> let v = eval_exp env e in ("-", env, v)
    (* let宣言の評価 *)
  | Decl (id, e) -> 
     let v = eval_exp env e in (id, Environment.extend id v env, v) (* 現在の環境で e を評価し、識別子、環境、値を返す *)
    (* let rec 宣言の評価 *)
  | RecDecl (id, para, e) ->
    (* LetRecExp と同様のバックパッチ処理を行う *)
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (para, e, dummyenv)) env in
      dummyenv := newenv;
      (id, newenv, ProcV (para, e, dummyenv)) (*識別子、新しい環境、関数閉包を返す*)
  | Seq (p1, p2) ->
      (* 最初の宣言を評価し、その新しい環境 (env1) を使って次の宣言を評価する *)
      let (_, env1, _) = eval_decl env p1 in
      eval_decl env1 p2  (* 最後に評価された結果（id, 環境, 値）を返す *)
