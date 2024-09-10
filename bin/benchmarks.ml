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
  (* val delete : symbol -> env -> env  *)
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

(* replace the code that raises TODO with your implementation *)
(* exception TODO  *)


module ListEnv : EnvSig = struct 
  let approach_name = "List-based environments"
  type env = (string * value) list 
  type symbol = string 
  let symbol = fun x -> x 
  let empty_env = []
  let (* rec *) insert e k v = 
(*    match e with
     (* | [] as e -> [(k, v)] @ e *)
    | [] -> [(k, v)]
    (* | (k', v') :: e' -> if k' = k then (k, v) :: e' else (k', v') :: insert e' k v *)
    | (k', v') :: e' -> (k, v) :: (k', v') :: e'
 *)
   (k, v) :: e
 let rec lookup e k = match e with
  | [] -> raise NotFound
  | (k', v) :: e' -> if k' = k then v else lookup e' k
end     


(* Approach 3 - Map-based environments  *)
module MapEnv : EnvSig = struct 
  let approach_name = "Map-based environments"
  module StringMap = Map.Make (String)   (* using Map functor; do read up on this !*)
  type symbol = string 
  let symbol = fun x-> x

  type env = value StringMap.t
  let empty_env = StringMap.empty
  let lookup e k = try StringMap.find k e with Not_found -> raise NotFound
  let insert e k v = StringMap.add k v e
end


(* Approach 4 - Map-based environments with int hashtable; 
  based on MCIML, Chapter 5.1 *)

  module MapHashEnv : EnvSig = struct 
  let approach_name = "Map-based envs with int H/T"
  let nextsym = ref 0
  type symbol = string * int
  module H = Hashtbl
  let hashtbl: (string, int) H.t = H.create 2048 (* some init size *)
  module SymbolMap = 
      Map.Make (
          struct 
            type t = symbol 
            let compare (_,n1) (_,n2) = compare n1 n2 
          end)

  let symbol name =
    match Hashtbl.find_opt hashtbl name with
    | Some i -> (name, i)
    | None ->
      let i = !nextsym in
      nextsym := i + 1;
      H.add hashtbl name i ;
      (name, i)
          
  type env = value SymbolMap.t 
  let empty_env = SymbolMap.empty
  let insert e k v = SymbolMap.add k v e
  let lookup e k = try SymbolMap.find k e with Not_found -> raise NotFound
end

(* Tests that work with any implementation of EnvSig *)
let test_double_insert env_impl = 
  let module EnvImpl = (val env_impl: EnvSig) in
  let open EnvImpl in 

  let (e, k, v1, v2) = (empty_env, symbol "a", 0, 1) in
  let e' = insert (insert e k v1) k v2 in
  let res = lookup e' k in
  assert (res = v2)

let test_lookup_old env_impl = 
  let module EnvImpl = (val env_impl: EnvSig) in
  let open EnvImpl in 

  let (e, k1, k2, v1, v2) = (empty_env, symbol "a", symbol "b", 0, 1) in
  let e' = insert (insert e k1 v1) k2 v2 in
  let res = lookup e' k1 in
  assert (res = v1)

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
  test_lookup_old env_impl;
  test_lookup_empty env_impl;
  let module EnvImpl = (val env_impl: EnvSig) in
  Printf.printf "Implementation %s behaves correctly\n" EnvImpl.approach_name

let _ = test_impl (module FunEnv: EnvSig)
let _ = test_impl (module ListEnv: EnvSig)
let _ = test_impl (module MapEnv: EnvSig)
let _ = test_impl (module MapHashEnv: EnvSig)

(* Benchmarking using core_bench *)
(* This probably requires extra libraries to install using opam *)
let _ = Random.self_init();;

let random_string n = 
  String.init n (fun _ -> Char.chr(97 + (Random.int 26)))

let max_n = 2000 
let name_strings = List.init max_n (fun _ -> (random_string 10))
let values = List.init max_n (fun x -> x )

module type BenchSig = sig 
  val approach_name : string 
  val bench : unit -> int
end


(* Question to students: is this a good way of doing benchmarking? *)
(* Are there any issues to discuss about this? *)
module EnvBench (E:EnvSig):BenchSig = struct
  include E  
  let names = List.map symbol name_strings
  let names_and_values = List.combine names values
  let bench () = 
    let e1 = List.fold_left (fun e (k,v) -> insert e k v ) empty_env names_and_values in 
    let s = List.fold_left (fun s n -> s + lookup e1 n ) 0 names  in
    s
end 

(* main function *)
let () =
  let open Core in 
  let open Core_bench in 
  let f m = let module M = (val m : BenchSig) in             
            Bench.Test.create ~name: M.approach_name M.bench 
  in (List.map [(module EnvBench (ListEnv)    : BenchSig);
                (module EnvBench (FunEnv)     : BenchSig);
                (module EnvBench (MapEnv)     : BenchSig);
                (module EnvBench (MapHashEnv) : BenchSig); 
                ]  ~f: f) 
  |> Bench.make_command   (* we're plugging into the bench main functionality *)
  |> Command_unix.run