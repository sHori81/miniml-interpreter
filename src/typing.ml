open Syntax

exception Error of string

let err s = raise (Error s)

(* 型環境の定義 *)
type tyenv = ty Environment.t

(* 型変数(tyvar)と、置き換え先の型(ty)のペアのリスト *)
type subst = (tyvar * ty) list

let rec single_subst (tv, target_ty) ty =
  match ty with
  (*intやboolはそのまま返す*)
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyList t -> TyList (single_subst (tv, target_ty) t) 
  (* 型変数を見つけた場合 *)
  | TyVar v -> if v = tv then target_ty else TyVar v (* 見つけた型変数の値がtvと一致する場合、型をtarget_tyに置き換える *)
  (* 関数型の場合 *)
  (* 左辺と右辺の型を再帰的に置き換えていく *)
  | TyFun (t1, t2) ->
      TyFun (single_subst (tv, target_ty) t1, single_subst (tv, target_ty) t2)

(* 複数の置き換え規則のリストを、先頭から順に適用していく関数*)
let subst_type (subst : subst) (ty : ty) =
  List.fold_left (fun current_ty single_sub -> single_subst single_sub current_ty) ty subst

(* 型代入を型の等式集合に変換する。*)
let eqs_of_subst (s : subst) : (ty * ty) list =
  List.map (fun (tv, t) -> (TyVar tv, t)) s
(*代入 [(tv1,t1); (tv2,t2); ...] を等式の集まり [(TyVar tv1, t1); (TyVar tv2, t2); ...] とみなすだけ。
   ty_exp の中で、部分式からすでに得られている代入を、新しく生じた制約と合流させて unify に渡すために使う。 *)

(* 制約リストに一つの型代入 (tv, t) を適用する補助関数 *)
let subst_eqs (s : tyvar * ty) (eqs : (ty * ty) list) : (ty * ty) list =
  List.map (fun (t1, t2) -> (subst_type [s] t1, subst_type [s] t2)) eqs

(* Unifyの実装*)
let rec unify (eqs : (ty * ty) list) : subst =
  match eqs with
  | [] -> [] (* Unify(Φ) = Φ *)
  | (t1, t2) :: rest ->
      if t1 = t2 then unify rest (*τ=τ、残りの等式を処理する *)
      else
        match t1, t2 with
      (*関数型の場合*)
      | TyFun (t11, t12), TyFun (t21, t22) ->
        (* τ11→τ12 = τ21→τ22 のケース *)
        unify ((t11, t21) :: (t12, t22) :: rest)
      (*型変数の場合*)
      | TyVar tv, _ when not (MySet.member tv (freevar_ty t2)) ->
        (* α = τ (τ≠α, オカーチェック OK) のケース *)
        let s = (tv, t2) in
        s :: unify (subst_eqs s rest)
      | _, TyVar tv when not (MySet.member tv (freevar_ty t1)) ->
        (* τ = α (τ≠α, オカーチェック OK) のケース *)
        let s = (tv, t1) in
        s :: unify (subst_eqs s rest)
      | _ ->
        (* それ以外（型の頭が食い違う、またはオカーチェック失敗）はエラー *)
        err "unify: cannot unify types"

(* プリミティブな演算子の型処理。演算子が満たすべき制約と返り値のタプルを返す。*)
let ty_prim op ty1 ty2 = match op with
  | Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt   -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And  -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or   -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
(* ty1, ty2 は型変数を含みうるので、その場で比較するのではなく
   「この演算が成り立つために満たすべき制約」として返し、
   整合性チェックは unify に任せる *)


(* 式の型処理　*)
(* tyenv と exp を受け取り、(その式を型付けするのに使った代入, 式の型) を返す *)
let rec ty_exp tyenv = function

  (* 変数の場合 *)
    Var x ->
      (try ([], Environment.lookup x tyenv) with
          Environment.Not_bound -> err ("variable not bound: " ^ x)) (* 変数の場合、tyenvから型を取り出す。見つからなければエラー *)

  (*リテラルの場合*)
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  (*リテラルの場合、型は int または bool で固定されるので、代入は空集合*)

  (*二項演算子の場合*)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in 
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in (* 両辺の型を調べ、ty_prim が要求する制約を取得 *)
      (* s1, s2 (部分式からすでに分かっている代入) を等式とみなし、
         ty_prim が新たに要求する制約 eqs3 と合わせてeqsとする *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      (* それをunifyで解き、代入、型のタプルで返す。*)
      let s3 = unify eqs in
      (s3, subst_type s3 ty)

  (*if式の場合*)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      (*それぞれの代入と型を調べる。合わせて一つの制約とする。*)
      let eqs =
        (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3)
        @ [(ty1, TyBool); (ty2, ty3)] (*条件式はbool、then節とelse節は同じ型、という制約を課す*)
      in
      (* それをunifyで解き、代入、型のタプルで返す。*)
      let s4 = unify eqs in
      (s4, subst_type s4 ty2)

  (*let式の場合*)
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      (*exp1の型をidに束縛して、exp2をその環境の下で型推論する*)
      let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
      (*それぞれの代入と型を調べる。合わせて一つの制約とする。*)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)

  (*関数式の場合*)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in (*仮引数の型が分からないので、新しい型変数を割り当てる*)
      (*domtyで環境を拡張し、そのもとでrantyの型を推測する*)
      let (s, ranty) = ty_exp (Environment.extend id domty tyenv) exp in
      (* domty 自身は本体 exp の中で情報を得るので、
         最後に s を domty にも反映させてから関数型を組み立てる *)
      (s, TyFun (subst_type s domty, ranty)) (*引数の型 -> 返り値の型　を返す。*)

  (*関数適用の場合*)
  | AppExp (exp1, exp2) ->
      (*exp1,exp2について型を調べる*)
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      (*結果の型はまだわからないため型変数で置く*)
      let ranty = TyVar (fresh_tyvar ()) in
      (*すべての等式制約をまとめる*)
      let eqs =
        (eqs_of_subst s1) @ (eqs_of_subst s2)
        @ [(ty1, TyFun (ty2, ranty))] (*exp1 は「exp2の型 -> 結果の型」という関数型でなければならない*)
      in
      let s3 = unify eqs in
      (s3, subst_type s3 ranty)

  (* let rec式の場合 *)
  | LetRecExp (f, x, exp1, exp2) ->
      (* 引数xの型と、本体exp1の型(=関数の返り値の型)は、まだ分からないので、両方とも型変数にしておく *)
      let domty = TyVar (fresh_tyvar ()) in
      let ranty = TyVar (fresh_tyvar ()) in
      (*f : domty→rantyとx : domtyの両方を環境に追加。これにより、exp1の評価にfを使える。*)
      let tyenv1 =
        Environment.extend f (TyFun (domty, ranty)) (Environment.extend x domty tyenv)
      in
      (*tyenv1でexp1を型推論する*)
      let (s1, ty1) = ty_exp tyenv1 exp1 in
      (*fののみをtyenv2に加えて、exp2を型推論する*)
      let tyenv2 = Environment.extend f (TyFun (domty, ranty)) tyenv in
      let (s2, ty2) = ty_exp tyenv2 exp2 in
      (* 本体exp1の型ty1は、fの返り値の型ranty と一致していなければならない *)
      let eqs =
        (eqs_of_subst s1) @ (eqs_of_subst s2)
        @ [(ty1, ranty)] (*exp1の型はfの返り値の型rantyと一致しなければならない*)
      in
      let s3 = unify eqs in
      (s3, subst_type s3 ty2)

  | _ -> err ("Not Implemented!")


let current_env = ref None

(* 型宣言の処理 *)
let ty_decl tyenv decl =
  (*現在の環境を取得する*)
  let env = match !current_env with Some e -> e | None -> tyenv in
  match decl with

  (*単なる式の場合、そのまま式の型推論 (ty_exp) を行う*)
  | Exp e ->
      let (_, ty) = ty_exp env e in
      ty

  (*let宣言の場合*)
  | Decl (id, e) ->
      (*まず右辺の型を調べる*)
      let (_, ty) = ty_exp env e in
      (*環境に変数とその型を追加する*)
      current_env := Some (Environment.extend id ty env);
      ty

  (*トップレベルのlet rec宣言の場合 (4.3.6の範囲外だが、let rec式と対にして追加)*)
  | RecDecl (f, x, exp1) ->
     (* 引数xの型と、本体exp1の型(=関数の返り値の型)は、まだ分からないので、両方とも型変数にしておく *)
      let domty = TyVar (fresh_tyvar ()) in
      let ranty = TyVar (fresh_tyvar ()) in
     (*let rec式と同様に、f : domty→rantyとx : domtyの両方を環境に追加。これにより、exp1の評価にfを使える。*)
      let tyenv1 = Environment.extend f (TyFun (domty, ranty)) (Environment.extend x domty env) in
      let (s1, ty1) = ty_exp tyenv1 exp1 in
     (*exp1の型ty1は、fの返り値の型ranty と一致していなければならない。
     　この条件を加え、unifyを実行。 *)
      let s2 = unify ((eqs_of_subst s1) @ [(ty1, ranty)]) in
     (* unifyの結果を使って、fの型を更新する。*)
      let fty = subst_type s2 (TyFun (domty, ranty)) in
     (*結果を型環境に追加*)
      current_env := Some (Environment.extend f fty env);
      fty
  
  | _ -> err ("Not Implemented!")