(** Testing framework for a toy language which you can adapt to the Dolphin compiler.
    It doesn't use any standard framework (e.g. OUnit/Alcotest) - you can use one.
    Moreover, if you don't want to automatize tests, you don't need to.
    But it's crucial that your test suite clearly states
     what is the expected behavior of compiler on each of test cases (see the comp_res type below).
    If you decide not to automatize, you can specify it in comments or in the report.
    However, I strongly suggest automatizing as much as you can, as it simplifies your development in the long run.
    (and allows me to grade faster)
*)

(** An "untyped AST" for the toy language *)
type elem = Int of int | Read | Print
type ast = elem list

(** semantics of the language: 
    - Int i prints i
    - Read reads a number in runtime
    - Print prints the latest runtime input

  So the output of
  Int 1
  read
  read
  int 2
  print
     
  with runtime input "4 5" is
  1, 2, 5
*)


(* In our assignments, "a" would be a TAst.prog, "compiled_path" is the path to produced .ll file *)
(* In this example, we'll "compile" the input program to a shell script *)
let compile (a: ast) (compiled_path: string) = 
    let open Printf in

    (* the actual "compilation" happens here *)
    let compile_num (e: elem) = 
        match e with
        | Int i ->
            (* an example of compilation mistakes *)
            let i' =
                if i = 10 then 0 else
                if i = 11 then failwith "crash during compilation"
                else i
            in sprintf "echo %d" i'
        | Read -> "read loc"
        | Print -> "echo $loc"
        in
    let compiled_prog = List.map compile_num a |> String.concat "\n" in

    let oc = open_out compiled_path in
    Printf.fprintf oc "%s\n" compiled_prog;
    close_out oc

(* "Linking" procedure.
    For the assignments, you'd link the .ll file with C runtime here *)    
let link (compiled_path: string) (out_path: string) =
    (* use clang here *)
    let r = Sys.command (Printf.sprintf "cp %s %s" compiled_path out_path) in
    (* TODO: to be completely precise, the failure here should lead to CompilationCrash outcome *)
    if r = 0 then () else failwith "Not linked"

(* Run an executable with two types of arguments: 
   1) command line arguments specified on call to executable.
      You don't need these for assignments
   2) runtime arguments provided during execution.
      Here we assume that such arguments can be specified in one line.
      These can be used to supply values for read_integer
   Return the lines of output from running the executable. 
   You should be able to reuse this function almost as is for the assignments tests.
*)
let run_with_input (exec_path: string) (inp_args: string list) (rt_args: string list): string list =
    let rt_args_str = String.concat " \\n " rt_args in
    let inp_args_str = String.concat " " inp_args in   
    let cmd = Printf.sprintf "echo -e \"%s\" | %s %s" rt_args_str exec_path inp_args_str in

    (* TODO: you'd also need to check that the process exited with code 0, 
       otherwise it should be another kind of compilation result *)

    let chan = Unix.open_process_in cmd in
    (* get the whole executable output as a string *)    
    let out = In_channel.input_all chan in
    (* Split by lines, drop empty *)
    let res = String.split_on_char '\n' out |> List.filter (fun s -> s <> "") 
    in res

(* Possible outcomes of running a single test case. *)
(* In assignments, SemanticErrors can be the list of semantic errors that you look for *)
type comp_res = SemanticErrors of string list | ProducedOutput of string list | CompilationCrash of string
                (* | ... add results corresponding to lexing and parsing errors *)
                (* | ... add result corresponding to a runtime error *)

let comp_res2str cr: string = 
    let open Printf in 
    match cr  with 
    | ProducedOutput o -> 
        let so = String.concat "; " o in
        sprintf "Output %s" so
    | SemanticErrors se -> 
        let s = String.concat "; " se in
        sprintf "Errros %s" s
    | CompilationCrash s -> s

(* produce readable error messages *)
let compare_comp_res cr_actual cr_expected: string =
    let open Printf in 
    match cr_actual, cr_expected with
    | ProducedOutput actual, ProducedOutput expected -> 
        let sa = String.concat "; " actual in
        let se = String.concat "; " expected in
        sprintf "Expected output %s, actual output is %s" se sa
    | SemanticErrors actual, SemanticErrors expected ->
        let sa = String.concat "; " actual in
        let se = String.concat "; " expected in
        sprintf "Expected errros are %s, actual errors are %s" se sa
    | actual, expected -> 
        sprintf "Different output kind: expected %s, actual %s" (comp_res2str expected) (comp_res2str actual)

(* convert untyped AST to typed AST + list of semantic errors (represented simply by strings here).
   For our example, simply check whether the input program is empty; 
     also assume that typed and untyped AST are represented by the same OCaml type
*)
let semant (prog: ast): ast * string list = 
    let is_correct = not (List.is_empty prog) in
    let sem_errs = if is_correct then [] else ["input program is empty"] in
    (prog, sem_errs)


(* The resulting procedure for compiling and running an input program.
   The procedure below is complete for phases < 3, 
   for the next phases you need to start with lexing and parsing the source code
*)
let run_compiler prog inputs: comp_res = 
    (* for phases >= 3, add calls to lexer and parser, handle possible errors *)
    let (tprog, sem_errs) = semant prog in
    if List.is_empty sem_errs then
        try
            compile tprog "./prog.sh";
            link "./prog.sh" "./prog_final.sh";
            let cmd_args = [List.length inputs |> string_of_int] in
            let out = run_with_input "bash prog.sh" cmd_args inputs in
            ProducedOutput out
        with 
        (* TODO: handle different kinds of exceptions separately and return an appropriate result *)
        | _ -> CompilationCrash "crash"
    else SemanticErrors sem_errs
    

let execute_case case_num (prog, inputs, tgt_out): unit = 
    let out = run_compiler prog inputs in
    (* You might need to introduce a custom comparison function for compilation results 
       E.g. convert lists of semantic errors to sets *)
    if (out = tgt_out) 
    then () 
    else Printf.printf "Case %d failed: %s \n" case_num (compare_comp_res out tgt_out)

(* every test case is an initial program, runtime arguments and expected result *)
(* RT arguments are supplied to read_integer *)
(* for phases < 3, initial program is untyped AST *)
(* for phases >= 3, initial program is either string with Dolphin program or path to file with it *)
let test_cases: (ast * string list * comp_res) list = [
    ([Int 11; Int 2; Int 3], [], ProducedOutput ["11"; "2"; "3"]); (* demonstrates that an exception raised in one test case doesn't stop the whole testing process *)
    ([Int 1; Int 2; Int 3], [], ProducedOutput ["1"; "2"; "3"]); (* test valid and correct output*)
    ([Int 1; Int 2; Int 10], [], ProducedOutput ["1"; "2"; "10"]);   (* valid, but incorrect compiler output - should fail *)
    ([Int 1; Read; Read; Int 2; Print], ["4"; "5"], ProducedOutput ["1"; "2"; "5"]);   (* test for a program reading runtime input *)
    ([], [], SemanticErrors ["input program is empty"]);   (* test for failing semantic analysis *)
 ]

(* keep the output of test runner minimal, e.g. avoid printing AST or produced LLVM code *)
let _ = 
    print_endline "Running tests:";
    List.iteri execute_case test_cases