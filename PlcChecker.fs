module PlcChecker

open Absyn
open Environ

let rec teval (e : expr) (env : plcType env) : plcType =
    match e with 
    | ConI i -> IntT
    | ConB b -> BooT
    | EList t -> t
    | Tuple [] -> TupT []
    | Var x  -> lookup env x 

    | Tuple es -> TupT (List.map (fun e -> teval e env) es)

    | Prim1 (op, e1) -> 
      let t1 = teval e1 env in
      match (op, t1) with
      | ("-", IntT) -> IntT
      | ("!", BooT) -> BooT
      | ("ise", LisT t) -> BooT
      | ("hd" , LisT t) -> t
      | ("tl" , LisT t) -> LisT t
      | ("print", t) -> TupT []
      | _   -> failwith "Checker: unknown op, or type error"

    | Prim2 (op, e1, e2) -> 
      let t1 = teval e1 env in
      let t2 = teval e2 env in
      match (op, t1, t2) with
      | ("=",  t1, t2) when t1 = t2 -> BooT
      | ("*",  IntT, IntT) -> IntT
      | ("+",  IntT, IntT) -> IntT
      | ("-",  IntT, IntT) -> IntT
      | ("/",  IntT, IntT) -> IntT
      | ("<",  IntT, IntT) -> BooT
      | ("<=", IntT, IntT) -> BooT
      | ("&&", t1, t2) when t1 = t2 -> BooT
      | ("!=", IntT, IntT) -> BooT
      | ("!=", BooT, BooT) -> BooT
      | ("::", t1, LisT t2) when t1 = t2 -> LisT t2
      | (";" , t1, t2)      -> t2
      | _   -> failwith "Checker: unknown op, or type error"
    
    | If (e1, e2, e3) -> 
      match teval e1 env with
      | BooT -> let t2 = teval e2 env in
                 let t3 = teval e3 env in
                 if t2 = t3 then 
                   t2
                 else 
                   failwith "Checker: 'if' branch types differ"

      | _    -> failwith "Checker: 'if':' condition not Boolean"

    | Sel (e1, n) -> 
      match teval e1 env with 
            | TupT ts -> 
              if 0 < n && n <= List.length ts then
                List.item (n - 1) ts
              else
                failwith "Checker: Tuple index out of range"
            | _ -> failwith ("Checker: selection operator #" + string n + "applied to non-tuple")

    | Let (x, e1, e2) -> 
      let t = teval e1 env in
      let env' = (x, t) :: env in
      teval e2 env'

    | Anon (x, s, e) -> 
      let env' = (x, s) :: env in
      let t = teval e env' in
      FunT (s,t)

    | Letrec (f,x,t,e1,t1,e2) -> 
       let env1 = (f, FunT (t,t1)) :: (x, t) :: env in 
       let env2 = (f, FunT (t,t1)) :: env in 
       let e1' = teval e1 env1 in 
       match e1' with 
       | t2 -> if t2 = t1 then teval e2 env2
               else failwith "Checker: invalid recursion function"

    | Call (e1, e2) -> 
       let c = teval e1 env in
       match c with
       | FunT (xt, rt) ->
         if teval e2 env = xt then 
           rt
         else
           failwith "Checker: type mismatch in function call"
       | _ -> failwith "Checker: function is undefined"