type calltype = Call | Jump | Callcond | Jumpcond | Else;;
type callsite = {address : string; calltype: calltype; by: string};;

let callsites = Hashtbl.create 10000;;

let convert j = 
    match j with
    | "call!" -> Call
    | "call?" -> Callcond
    | "jump!" -> Jump
    | "jump?" -> Jumpcond
    | _ -> Else;;

let read_file f =
        let chan = open_in f in
        try 
                let current_entry = ref "" in
                while true; do
                        let line = input_line chan |> Str.split (Str.regexp " \|\t") in
                        (* List.iteri (fun i x -> Printf.printf "[%d:%s]" i x) line;
                        print_endline "";
                        *)
                        match line with
                        | "entry" :: s :: _ -> begin
                                (* Printf.printf "entry %s\n" s;  *)
                                current_entry := s;
                        end
                        | addr :: arrow :: jump :: dest ::  _-> 
                              let l = dest |> Str.split (Str.regexp "/") |> List.length in
                              if (l = 3 &&
                                (Hashtbl.find_all callsites dest |> 
                                 List.map (fun x -> x.by) |> 
                                 List.mem !current_entry |> not)) then
                                Hashtbl.add callsites dest 
                                {address=addr; calltype = convert jump; by= !current_entry}
                              else ();
                        | _ -> ()
                        
                done
        with End_of_file ->
                close_in chan;;

read_file "calltree.txt";;
Hashtbl.iter (fun x y -> Printf.printf "entry=%s is called by %s at %s\n" x y.by y.address) callsites;;

let rec reverse l_candidat l_result l_alreadyseen = 
        List.iter (fun x -> Printf.printf "reverse : %s @ %s\n" x.by x.address) l_candidat;
        match l_candidat with
        | [] -> l_result
        | x::l_c -> 
                    if List.mem x.by l_alreadyseen then
                        reverse l_c l_result l_alreadyseen
                    else 
                        let (l_text,l_remain) = Hashtbl.find_all callsites x.by |> 
                               List.partition (fun t -> String.sub t.by 0 6 = ".text/") 
                        in
                        reverse (l_c@l_remain ) (l_text@l_result) (x.by::l_alreadyseen);;


let call = reverse [{by = ".textlib/__int32_lt_float32_float32/0x00406190";address="";calltype=Else}] [] [];;
List.iter (fun x -> Printf.printf "%s @ %s\n" x.by x.address) call;;
