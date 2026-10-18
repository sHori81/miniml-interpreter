%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT
%token AND OR (*&& || に対応するトークンの追加*)
%token IF THEN ELSE TRUE FALSE

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel

%token LET IN EQ (*let in = に対応するトークンの追加。*)
%token RARROW FUN (*-> fun に対応するトークンの追加*)
%token REC (*rec に対応するトークンの追加*)

%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | decls=DeclList SEMISEMI { decls } (*宣言のトップレベルを DeclList にする*)

(* 複数のTopDeclを Seq で繋ぐ *)
DeclList :
    d=TopDecl { d }
  | d=TopDecl ds=DeclList { Seq (d, ds) } (*先頭に単一の宣言を追加する。*)

(* 単一の宣言 *)
TopDecl :
    LET x=ID EQ e=Expr { Decl (x, e) }
  | LET REC f=ID EQ FUN x=ID RARROW e=Expr { RecDecl (f, x, e) } 
  
Expr :
    e=IfExpr { e }
  | e=LetExpr { e } (* let式は一般の式として扱われ、結合もifと同程度。 *)
  | e=LetRecExpr { e }
  | e=OrExpr { e }
  | e=FunExpr { e } (* fun式は一般の式として扱われ、結合もifと同程度。 *)

(*論理和の定義*)
OrExpr :
  l=OrExpr OR r=AndExpr { BinOp (Or, l, r) }
  | e=AndExpr { e }

(*論理積の定義*)
AndExpr :
  l=AndExpr AND r=LTExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

(*大小比較*)
LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

(*足し算の定義*)
PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

(*掛け算の定義*)
MExpr :
    l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }
  | e=AExpr { e }
  | e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
  | e=AppExpr { e }
  (* ここで、関数適用式の構文を追加する。AppExpr は左結合であるため、MExpr の中で AppExpr を呼び出す。 *)

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) } (* let式の構文の追加。let <識別子> = <式1> in <式2> の形を LetExp で表現する *)

AppExpr : (* 関数適用式の構文 *)
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }
  (* e1 e2 の形を AppExp で表現する。関数適用式は左結合であるため、e1 は AppExpr としている。 
    ただの式 が来たときはAExprにする。*)

FunExpr : (* 関数式の構文 *)
    FUN x=ID RARROW e=Expr { FunExp (x, e) }
(* fun <識別子> -> <式> という形を FunExp で表現する *)

LetRecExpr :
    LET REC f=ID EQ FUN x=ID RARROW e1=Expr IN e2=Expr { LetRecExp (f, x, e1, e2) } (* let rec <識別子> = fun <識別子> -> <式1> in <式2> という形を LetRecExp で表現する *)
