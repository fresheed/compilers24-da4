(* Defining the type for binary operations *)
type binop = Add | Sub | Mul | Div

(* Defining the type for arithmetic expressions *)
type expr 
  = Int of int                       (* Integer constant *)
  | BinOp of binop * expr * expr     (* Binary operation *)


let op2str (op: binop) =
  match op with
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let rec str_repeat (s: string) (n: int): string = 
  if (n = 0) then ""    
  else
    let s' = str_repeat s (n - 1) in
    s ^ s'

let tree2str (e: expr): string =

  let rec tree2str' (e: expr) (i: int): string = 
    let indent = str_repeat " " i in
    match e with
    | Int n -> indent ^ string_of_int n
    | BinOp (op, e1, e2) -> 
      let o = op2str op in
      let t1 = tree2str' e1 (i + 1) in
      let t2 = tree2str' e2 (i + 1) in
      Printf.sprintf "%s%s\n%s\n%s" indent o t1 t2
  (* end of tree2str' *) 
  in
  tree2str' e 0


let my_expr = BinOp (Add, Int 5, BinOp (Sub, Int 3, Int 1))

type nodeId = string
type node = nodeId * string
type edge = nodeId * nodeId
type graph = { nodes: node list; edges: edge list }

let merge_graphs g1 g2: graph = 
  match g1, g2 with
  | { nodes = n1; edges = e1}, { nodes = n2; edges = e2} ->
    { nodes = n1 @ n2; edges = e1 @ e2}

let expr2graph (e: expr): graph = 

  let rec expr2graph' (e: expr) (id: nodeId): graph = 
    match e with
    | Int n -> { nodes = [(id, string_of_int n)]; edges = [] }
    | BinOp (op, e1, e2) -> 
      let id1 = id ^ "L" in
      let id2 = id ^ "R" in
      let g1 = expr2graph' e1 id1 in
      let g2 = expr2graph' e2 id2 in
      let o = { nodes = [(id, op2str op)]; edges = [(id, id1); (id, id2)] } in
      merge_graphs o (merge_graphs g1 g2)
  in
  expr2graph' e "L"

(* not covered in class. Pay attention to how List.map is used *)
let graph2str (g: graph): string =
  let open Printf in (* use (f/s)printf functions without "Printf." inside graph2str *)
  let node2str (n: node) = sprintf "  %s [label=\"%s\"];" (fst n) (snd n) in
  let nodes = String.concat "\n" (List.map node2str g.nodes) in
  let edge2str ((n1, n2): edge) = sprintf "  %s -> %s;" n1 n2 in
  let edges = String.concat "\n" (List.map edge2str g.edges) in
  sprintf "digraph AST {\n%s\n%s\n}" nodes edges
  

let _ =
  let g = expr2graph my_expr in
  let s = graph2str g in
  let oc = open_out "graph.dot" in
  Printf.fprintf oc "%s\n" (s);
  close_out oc;
  let r = Sys.command "dot -Tpng graph.dot -o graph.png" in
  Printf.printf "Exit code of dot: %d\n" r
