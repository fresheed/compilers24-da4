(* An initial representation of the program. For assignment 2, it'd be eprog *)
type prog = unit


(* Compile the given program into file with given path.
   For assignment 2, it'd be the .s file *)
let compile (_: prog) (compiled_path: string) = 
    let r = Sys.command (Printf.sprintf "cp input_sum.c %s" compiled_path) in
    if r = 0 then () else failwith "Not compiled"


(* Link the compiled program with needed libraries *)    
let link (compiled_path: string) (out_path: string) =
    let r = Sys.command (Printf.sprintf "clang %s -o %s" compiled_path out_path) in
    if r = 0 then () else failwith "Not linked"


(* Run an executable with two types of arguments: 
   1) command line arguments specified on call to executable.
      You don't need these for assignment 2, but in this example we use it to specify the number of runtime arguments
   2) runtime arguments provided during execution.
      Here we assume that such arguments can be specified in one line.
      These can be used to supply values for Input statements in assignment 2.
   As an output, we return the "result" from executable. 
   Here we assume that the result is the last line of executable's output
*)
let run_with_input (out_path: string) (inp_args: string list) (rt_args: string list) =
    let rt_args_str = String.concat " " rt_args in
    let inp_args_str = String.concat " " inp_args in
    let cmd = Printf.sprintf "echo %s | %s %s" rt_args_str out_path inp_args_str in

    let chan = Unix.open_process_in cmd in
    (* get the whole executable output as a string *)    
    let out = In_channel.input_all chan in
    (* Split strings, drop empty, take the last one *)
    let res = String.split_on_char '\n' out |> List.filter (fun s -> s <> "") 
              |> List.rev |> List.hd in
    res

(* every test case is an initial program, runtime arguments and expected result *)
let test_cases = [
    ((), ["1"; "2"; "3"], "6");
    ((), ["1"], "1");
    ((), [], "0");
    ((), ["1"; "2"; "3"], "666")
]


let run_case case_num (prog, inputs, tgt_out) = 
    compile prog "./prog.c";
    link "./prog.c" "./prog.out";
    let cmd_args = [List.length inputs |> string_of_int] in
    let out = run_with_input "./prog.out" cmd_args inputs in
    if (out = tgt_out) then () else Printf.printf "Case %d failed\n" case_num

let _ = List.iteri run_case test_cases