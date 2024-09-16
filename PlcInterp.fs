module PlcInterp

open Absyn
open Environ

let rec eval (e : expr) (env : plcVal env) : plcVal =
    match e with 
    | ConI i -> IntV i
    | ConB b -> BooV b
    | EList _ -> LisV []

    | Var x  ->
      let v = lookup env x in
      match v with
      | IntV _ -> v
      | BooV _ -> v 
      | TupV _ -> v 
      | LisV _ -> v
      | Clos _ -> v
    
    | Prim1 (op, e1) -> 
      let v1 = eval e1 env in
      match (op, v1) with
      | ("-", IntV i) -> IntV (- i)
      | ("!", BooV b) -> BooV (not b)
      | ("ise", LisV l) -> BooV (l = [])
      | ("hd" , LisV l) -> if (l = []) then failwith "empty list" else List.head l
      | ("tl" , LisV l) -> if (l = []) then failwith "empty list" else LisV (List.tail l)
      | ("print", e) -> 
            let mute = printf "%s\n"(val2string e) in
            TupV []
      | _   -> failwith "Impossible"

    | Prim2 (op, e1, e2) -> 
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | ("=", _, _) -> BooV (v1 = v2)
      | ("*", IntV i1, IntV i2) -> IntV (i1 * i2)
      | ("+", IntV i1, IntV i2) -> IntV (i1 + i2)
      | ("-", IntV i1, IntV i2) -> IntV (i1 - i2)
      | ("/", IntV i1, IntV i2) -> IntV (i1 / i2)
      | ("<", IntV i1, IntV i2) -> BooV (i1 < i2)
      | ("<=", IntV i1, IntV i2) -> BooV (i1 <= i2)
      | ("!=", IntV i1, IntV i2) -> BooV (i1 <> i2)
      | ("!=", BooV b1, BooV b2) -> BooV (b1 <> b2)
      | ("&&", BooV b1, BooV b2) -> BooV (b1 && b2)
      | ("::", e1, LisV e2) -> LisV (e1 :: e2)
      | (";" , e1, e2)      -> e2
      | _   -> failwith "Impossible"
    
    | If (e1, e2, e3) -> 
      let v1 = eval e1 env in
      match v1 with
      | BooV true  -> eval e2 env
      | BooV false -> eval e3 env
      | _ -> failwith "Impossible"

    | Tuple es -> TupV (List.map (fun e -> eval e env) es)

    | Sel (e1, n) -> 
      match eval e1 env with 
      | TupV vs -> List.item (n - 1) vs
      | _ -> failwith "Impossible"

    | Anon (x, _, e) -> Clos("", x, e, env)
    
    | Let (x, e1, e2) -> 
      let v = eval e1 env in
      match (x,v,e2) with
      | (x,Clos(f,x1,e,env),e2) ->
          let env2 = (x, Clos(f, x1, e, env)) :: env in
                eval e2 env2
      | (x,v,e2) -> 
      let env2 = (x, v) :: env in
      eval e2 env2

    | Letrec (f,x,_,e,_,calle) -> 
      //let exp = Let (f, Anon (x,i,e), calle) in 
      //eval exp env

      let e1 = Clos(f, x, e, env) in 
      //(f, e1,calles)
        let env2 = (f, e1) :: env in 
        eval calle env2

    | Call (e1, e2) -> 
       let c = eval e1 env in
       match c with
       | Clos (f, x, fbody, fenv) ->
          let v = eval e2 env in
          let env1 = (x, v) :: (f, c) :: fenv in
          eval fbody env1
       | _ -> failwith "eval Call: not a function";;
    