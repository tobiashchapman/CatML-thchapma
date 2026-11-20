(*
   CatML Abstract Syntax
   ----------------------
*)
type var = string
type expr =
     (* boolean expression forms *)
     | Bool of bool | And of expr * expr | Or of expr * expr | Not of expr
     (* arithmetic expression forms *)
     | Int of int | Plus of expr * expr | Mult of expr * expr | Equal of expr * expr
     (* conditional expression form *)
     | If of expr * expr * expr
     (* Variables and let expressions *)
     | Var of var | Let of var * expr * expr

     (* TODO: Add pairs; functions; and recursive functions COMPLETED *)
     | Pair of expr * expr | First of expr | Second of expr
     | Fun of var * expr | Fix of var * var * expr | App of expr * expr
     


(*
  is_val : expr -> bool
  in : AST [[e]]
  out : true iff e is a value
*)
let rec is_val e = match e with 
     Int _ -> true
   | Bool _ -> true
   | Pair (e1, e2) -> is_val e1 && is_val e2
   | Fun _ -> true
   | Fix _ -> true
   | _ -> false

(*************************)
(* Operational Semantics *)
(*************************)

(* Define substitution *)
let rec subst (x, v, e) : expr =
  match e with
  | Var y -> if x = y then v else (Var y)
  | Let(y,e1,e2) -> Let(y,subst(x,v,e1), if x = y then e2 else subst(x,v,e2))
  | Bool _ | Int _ -> e
  | And(e1,e2) -> And(subst(x,v,e1), subst(x,v,e2))
  | Or(e1,e2) -> Or(subst(x,v,e1), subst(x,v,e2))
  | Not(e') -> Not(subst(x,v,e'))
  | Plus(e1,e2) -> Plus(subst(x,v,e1), subst(x,v,e2))
  | Mult(e1,e2) -> Mult(subst(x,v,e1), subst(x,v,e2))
  | Equal(e1,e2) -> Equal(subst(x,v,e1), subst(x,v,e2))
  | If(e0,e1,e2) -> If(subst(x,v,e0), subst(x,v,e1), subst(x,v,e2))
  | Pair(e1,e2) -> Pair(subst(x,v,e1), subst(x,v,e2))
  | First(e') -> First(subst(x,v,e'))
  | Second(e') -> Second(subst(x,v,e'))
  | Fun(y,body) -> Fun(y, if x = y then body else subst(x,v,body))
  | Fix(f,y,body) -> Fix(f, y, if x = f || x = y then body else subst(x,v,body))
  | App(e1,e2) -> App(subst(x,v,e1), subst(x,v,e2))

type step_result = Step of expr | NotReducible
(* 
   step : expr -> step_result
   in : An expression [[e]]
   out : An expression [[e']] if e -> e' in the operational semantics;
         if no such [[e']] exists, returns [[NotReducible]]
*)
let rec step (e : expr) : step_result = 
   match e with
   | Bool _ | Int _ | Fun _ | Fix _ -> NotReducible
   | Not (Bool b) -> Step (Bool (not b))
   | Not e1 ->
      begin match step e1 with
       | Step e1' -> Step (Not e1')
       | NotReducible -> NotReducible
      end
   | And (Bool b1, Bool b2) -> Step (Bool (b1 && b2))
   | And (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (And (e1', e2))
       | NotReducible ->
          begin match step e2 with
           | Step e2' -> Step (And (e1, e2'))
           | NotReducible -> NotReducible
          end
       end
   | Or (Bool b1, Bool b2) -> Step (Bool (b1 || b2))
   | Or (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Or (e1', e2))
       | NotReducible ->
          begin match step e2 with
           | Step e2' -> Step (Or (e1, e2'))
           | NotReducible -> NotReducible
          end
       end
   | Plus (Int n1, Int n2) -> Step (Int (n1 + n2))
   | Plus (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Plus (e1', e2))
       | NotReducible ->
          begin match step e2 with
           | Step e2' -> Step (Plus (e1, e2'))
           | NotReducible -> NotReducible
          end
       end
   | Mult (Int n1, Int n2) -> Step (Int (n1 * n2))
   | Mult (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Mult (e1', e2))
       | NotReducible ->
           begin match step e2 with
           | Step e2' -> Step (Mult (e1, e2'))
           | NotReducible -> NotReducible
          end
       end
   | Equal (Int n1, Int n2) -> Step (Bool (n1 = n2))
   | Equal (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Equal (e1', e2))
       | NotReducible ->
          begin match step e2 with
           | Step e2' -> Step (Equal (e1, e2'))
           | NotReducible -> NotReducible
          end
       end
   | If (Bool true, e1, _) -> Step e1
   | If (Bool false, _, e2) -> Step e2
   | If (e1, e2, e3) ->
      begin match step e1 with
       | Step e1' -> Step (If (e1', e2, e3))
       | NotReducible -> NotReducible
      end
   | Var _ -> NotReducible
   | Let(x,e1,e2) -> 
      begin match step e1 with
      | Step e1' -> Step (Let(x,e1',e2))
      | NotReducible -> if is_val e1 then Step(subst(x,e1,e2)) else NotReducible
      end
   | Pair(e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Pair (e1', e2))
       | NotReducible ->
          begin match step e2 with
           | Step e2' -> Step (Pair (e1, e2'))
           | NotReducible -> NotReducible
          end
      end
   | First(Pair(e1, e2)) when is_val e1 && is_val e2 -> Step e1
   | First(e') ->
      begin match step e' with
       | Step e'' -> Step (First e'')
       | NotReducible -> NotReducible
      end
   | Second(Pair(e1, e2)) when is_val e1 && is_val e2 -> Step e2
   | Second(e') ->
      begin match step e' with
       | Step e'' -> Step (Second e'')
       | NotReducible -> NotReducible
      end
   | App(Fun(x, body), e2) when is_val e2 -> Step (subst(x, e2, body))
   | App(Fix(f, x, body), e2) when is_val e2 ->
      Step (subst(x, e2, subst(f, Fix(f, x, body), body)))
   | App(e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (App(e1', e2))
       | NotReducible ->
          begin match step e2 with
           | Step e2' -> Step (App(e1, e2'))
           | NotReducible -> NotReducible
          end
      end


(*
   multistep : expr -> expr
   in : AST [[e]]
   out : [[e']] such that e ->* e' -/-> in the operational semantics
*)
let rec multistep e = match step e with 
    | Step e' -> multistep e'
    | NotReducible -> e





(*****************)
(* Type checking *)
(*****************)

(* TODO: add pairs and function types COMPLETED*)
type arith_type = INT | BOOL | PairT of arith_type * arith_type | FunT of arith_type * arith_type


(* Define a type to represent finite maps/typing contexts *)
type context = (var * arith_type) list

(* empty_context : context *)
let empty_context : context = []

(* lookup : context * var -> arith_type option
- input :
-    g - a typing context
-    x - a variable
- output :
-    if (x,tau) is in g, return Some tau
-    otherwise, return None
 *)
let rec lookup (g,x) : arith_type option =
  match g with
  | [] -> None
  | (y,tp)::g' -> if x = y then Some tp else lookup(g',x)

(* extend : context * var * arith_type -> context
- input:
-    g - a typing context
-    x - a variable
-    tp - an arith_type
- output:
-    A typing context that maps x to tp and any other y to g(y)
 *)
let extend (g,x,tp) : context = (x,tp)::g

type ty_var = int

type full_type =
  | TInt
  | TBool
  | TPair of full_type * full_type
  | TFun of full_type * full_type
  | TVar of ty_var

let fresh_var =
  let counter = ref 0 in
  fun () ->
    incr counter;
    TVar !counter

let rec apply_subst (sub : (ty_var * full_type) list) (t : full_type) : full_type =
  match t with
  | TInt | TBool -> t
  | TPair (t1, t2) -> TPair (apply_subst sub t1, apply_subst sub t2)
  | TFun (t1, t2) -> TFun (apply_subst sub t1, apply_subst sub t2)
  | TVar v ->
      begin match List.assoc_opt v sub with
      | None -> TVar v
      | Some t' -> apply_subst sub t'
      end

let rec occurs (v : ty_var) (t : full_type) : bool =
  match t with
  | TInt | TBool -> false
  | TPair (t1, t2) | TFun (t1, t2) -> occurs v t1 || occurs v t2
  | TVar u -> u = v

let rec unify (sub : (ty_var * full_type) list) (t1 : full_type) (t2 : full_type)
    : (ty_var * full_type) list option =
  let t1 = apply_subst sub t1 in
  let t2 = apply_subst sub t2 in
  match (t1, t2) with
  | TInt, TInt | TBool, TBool -> Some sub
  | TPair (a1, b1), TPair (a2, b2) | TFun (a1, b1), TFun (a2, b2) ->
      begin match unify sub a1 a2 with
      | None -> None
      | Some sub' -> unify sub' b1 b2
      end
  | TVar v, t | t, TVar v ->
      if t1 = t2 then Some sub
      else if occurs v t then None
      else Some ((v, t) :: sub)
  | _ -> None

let rec to_arith_type (sub : (ty_var * full_type) list) (t : full_type) : arith_type option =
  match apply_subst sub t with
  | TInt -> Some INT
  | TBool -> Some BOOL
  | TPair (t1, t2) ->
      begin match to_arith_type sub t1, to_arith_type sub t2 with
      | Some a1, Some a2 -> Some (PairT (a1, a2))
      | _ -> None
      end
  | TFun (t1, t2) ->
      begin match to_arith_type sub t1, to_arith_type sub t2 with
      | Some a1, Some a2 -> Some (FunT (a1, a2))
      | _ -> None
      end
  | TVar _ -> None

let rec infer (env : (var * full_type) list) (sub : (ty_var * full_type) list) (e : expr)
    : (full_type * (ty_var * full_type) list) option =
  match e with
  | Int _ -> Some (TInt, sub)
  | Bool _ -> Some (TBool, sub)
  | Var x ->
      begin match List.assoc_opt x env with
      | Some t -> Some (t, sub)
      | None -> None
      end
  | Plus (e1, e2) | Mult (e1, e2) ->
      begin match infer env sub e1 with
      | None -> None
      | Some (t1, sub1) ->
          begin match unify sub1 t1 TInt with
          | None -> None
          | Some sub2 ->
              begin match infer env sub2 e2 with
              | None -> None
              | Some (t2, sub3) ->
                  begin match unify sub3 t2 TInt with
                  | None -> None
                  | Some sub4 -> Some (TInt, sub4)
                  end
              end
          end
      end
  | Equal (e1, e2) ->
      begin match infer env sub e1 with
      | None -> None
      | Some (t1, sub1) ->
          begin match unify sub1 t1 TInt with
          | None -> None
          | Some sub2 ->
              begin match infer env sub2 e2 with
              | None -> None
              | Some (t2, sub3) ->
                  begin match unify sub3 t2 TInt with
                  | None -> None
                  | Some sub4 -> Some (TBool, sub4)
                  end
              end
          end
      end
  | And (e1, e2) | Or (e1, e2) ->
      begin match infer env sub e1 with
      | None -> None
      | Some (t1, sub1) ->
          begin match unify sub1 t1 TBool with
          | None -> None
          | Some sub2 ->
              begin match infer env sub2 e2 with
              | None -> None
              | Some (t2, sub3) ->
                  begin match unify sub3 t2 TBool with
                  | None -> None
                  | Some sub4 -> Some (TBool, sub4)
                  end
              end
          end
      end
  | Not e' ->
      begin match infer env sub e' with
      | None -> None
      | Some (t', sub1) ->
          begin match unify sub1 t' TBool with
          | None -> None
          | Some sub2 -> Some (TBool, sub2)
          end
      end
  | If (e0, e1, e2) ->
      begin match infer env sub e0 with
      | None -> None
      | Some (t0, sub1) ->
          begin match unify sub1 t0 TBool with
          | None -> None
          | Some sub2 ->
              begin match infer env sub2 e1 with
              | None -> None
              | Some (t1, sub3) ->
                  begin match infer env sub3 e2 with
                  | None -> None
                  | Some (t2, sub4) ->
                      begin match unify sub4 t1 t2 with
                      | None -> None
                      | Some sub5 -> Some (apply_subst sub5 t1, sub5)
                      end
                  end
              end
          end
      end
  | Pair (e1, e2) ->
      begin match infer env sub e1 with
      | None -> None
      | Some (t1, sub1) ->
          begin match infer env sub1 e2 with
          | None -> None
          | Some (t2, sub2) -> Some (TPair (t1, t2), sub2)
          end
      end
  | First e' ->
      begin match infer env sub e' with
      | None -> None
      | Some (t', sub1) ->
          let a = fresh_var () in
          let b = fresh_var () in
          begin match unify sub1 t' (TPair (a, b)) with
          | None -> None
          | Some sub2 -> Some (a, sub2)
          end
      end
  | Second e' ->
      begin match infer env sub e' with
      | None -> None
      | Some (t', sub1) ->
          let a = fresh_var () in
          let b = fresh_var () in
          begin match unify sub1 t' (TPair (a, b)) with
          | None -> None
          | Some sub2 -> Some (b, sub2)
          end
      end
  | Let (x, e1, e2) ->
      begin match infer env sub e1 with
      | None -> None
      | Some (t1, sub1) ->
          let t1' = apply_subst sub1 t1 in
          infer ((x, t1') :: env) sub1 e2
      end
  | Fun (x, body) ->
      let tx = fresh_var () in
      begin match infer ((x, tx) :: env) sub body with
      | None -> None
      | Some (tbody, sub1) ->
          let tx' = apply_subst sub1 tx in
          let tbody' = apply_subst sub1 tbody in
          Some (TFun (tx', tbody'), sub1)
      end
  | Fix (f, x, body) ->
      let tx = fresh_var () in
      let tr = fresh_var () in
      let tf = TFun (tx, tr) in
      let env' = (f, tf) :: (x, tx) :: env in
      begin match infer env' sub body with
      | None -> None
      | Some (tbody, sub1) ->
          begin match unify sub1 tbody (apply_subst sub1 tr) with
          | None -> None
          | Some sub2 ->
              let tf' = apply_subst sub2 tf in
              Some (tf', sub2)
          end
      end
  | App (e1, e2) ->
      begin match infer env sub e1 with
      | None -> None
      | Some (t1, sub1) ->
          begin match infer env sub1 e2 with
          | None -> None
          | Some (t2, sub2) ->
              let ret = fresh_var () in
              begin match unify sub2 t1 (TFun (t2, ret)) with
              | None -> None
              | Some sub3 -> Some (ret, sub3)
              end
          end
      end

let rec arith_to_full = function
  | INT -> TInt
  | BOOL -> TBool
  | PairT (t1, t2) -> TPair (arith_to_full t1, arith_to_full t2)
  | FunT (t1, t2) -> TFun (arith_to_full t1, arith_to_full t2)

let type_of_expr ((g, e) : context * expr) : arith_type option =
  let env = List.map (fun (x, tp) -> (x, arith_to_full tp)) g in
  match infer env [] e with
  | None -> None
  | Some (t, sub) -> to_arith_type sub t
