type value = int
exception NotFound (* we throw this exception when the key is not found *)
(* exception TODO *)

(* let approach_name = "Functional environments" *)
type env' = string -> value 
(* type symbol = string  *)
(* let symbol = fun x -> x  *)
let empty_env' = fun _ -> raise NotFound
let insert' e k v = 
  fun x -> if x = k then v else e x 
let lookup' (e: env') (k: string) = e k

let test_double_insert' e k v1 v2 = 
  let e' = insert' (insert' e k v1) k v2 in
  let res = lookup' e' k in
  assert (res = v2)
let test_lookup_empty' k = 
  try
    ignore (lookup' empty_env' k);
    failwith "wrong"
  with
  NotFound -> ()

let _ = test_double_insert' empty_env' "a" 0 1
let _ = test_lookup_empty' "a"

module type EnvSig = sig 
  type env                  (* abstract environment type *)
  type symbol               (* abstract symbol type      *)
  val empty_env : env       (* create an empty environment *)
  val insert : env -> symbol -> value -> env  (* insert; returns updated environment *)
  val lookup : env -> symbol -> value         (* lookup; returns the value or raises NotFound *)
  val symbol : string -> symbol   (* create a symbol value from string *)
  val approach_name: string (* the name of our approach, for benchmarking later *)
end

module FunEnv : EnvSig = struct
  let approach_name = "Functional environments"
  type env = string -> value 
  type symbol = string 
  let symbol = fun x -> x 
  let empty_env = fun _ -> raise NotFound
  let insert e k v = 
    fun x -> if x = k then v else e x 
  let lookup (e: env) (k: string) = e k
end

(* Tests that work with any implementation of EnvSig *)
let test_double_insert env_impl = 
  let module EnvImpl = (val env_impl: EnvSig) in
  let open EnvImpl in 

  let (e, k, v1, v2) = (empty_env, symbol "a", 0, 1) in
  let e' = insert (insert e k v1) k v2 in
  let res = lookup e' k in
  assert (res = v2)

let test_lookup_empty env_impl = 
  let module EnvImpl = (val env_impl: EnvSig) in
  let open EnvImpl in 
  
  let k = symbol "a" in
  try
    ignore (lookup empty_env k);
    failwith "wrong"
  with
  NotFound -> ()
  
let test_impl env_impl = 
  test_double_insert env_impl;
  test_lookup_empty env_impl;
  let module EnvImpl = (val env_impl: EnvSig) in
  Printf.printf "Implementation %s behaves correctly\n" EnvImpl.approach_name

let _ = test_impl (module FunEnv: EnvSig)