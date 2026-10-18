open Syntax
open Eval
open Typing

let rec read_eval_print env tyenv = 
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let ty = ty_decl tyenv decl in 
  
    (* 【変更】Seqノードを再帰的に分解して各宣言を評価する補助関数 *)
    let rec eval_and_print current_env d =
      match d with
      | Seq (p1, p2) ->
          (* 前半部分(p1)を処理して得た環境を使って、後半部分(p2)を処理する *)
          let env' = eval_and_print current_env p1 in
          eval_and_print env' p2
      | _ ->
          (* 単一の宣言（Decl や Exp など）に到達したら、通常通り評価してcuiで出力する *)
          let (id, newenv, v) = eval_decl current_env d in 
          Printf.printf "val %s : " id;
          pp_ty ty;
          print_string " = ";
          pp_val v;
          print_newline();
          newenv (* 新しい環境を返す *)
    in
    (* 補助関数を実行して、すべての宣言を出力しつつ最終的な環境を取得する *)
    let next_env = eval_and_print env decl in 
    read_eval_print next_env tyenv

  with exn -> 
    (* 例外の種類に応じてエラーメッセージの文字列だけを作成する *)
      let msg = match exn with
        | Eval.Error s -> "Eval Error: " ^ s
        | Parser.Error -> "Syntax Error"
        | Typing.Error s -> "Type Error: " ^ s
        | _ -> "Error: " ^ Printexc.to_string exn
      in
      (* メッセージを出力し、同じ環境で再び実行をする。 *)
      Printf.printf "%s\n" msg;
      read_eval_print env tyenv

(* 実行時の値を持つ初期環境の定義 *)
  let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "ii" (IntV 2) (* [追加] 変数 ii に 整数値 2 を登録 *)
       (Environment.extend "iii" (IntV 3)  (* [追加] 変数 iii に 整数値 3 を登録 *)
          (Environment.extend "iv" (IntV 4)  (* [追加] 変数 iv に 整数値 4 を登録 *)
            (Environment.extend "v" (IntV 5)
              (Environment.extend "x" (IntV 10) Environment.empty)))))
              (*大域変数をここで追加する。環境中に追加する文字と対応する値を指定する*)

  (*初期の型環境の定義*)
  let initial_tyenv =
  Environment.extend "i" TyInt
    (Environment.extend "ii" TyInt 
       (Environment.extend "iii" TyInt  
          (Environment.extend "iv" TyInt  
            (Environment.extend "v" TyInt
              (Environment.extend "x" TyInt Environment.empty)))))