open Printf
open Sast



let make_header (file_name : string) (is_recipe : bool) =
        if is_recipe then
        sprintf "public final class %s {\n  public static void perform(){\n" file_name
        else
        sprintf "public class %s {\n  public static void main(String args[])\n" file_name

let write_out filename buffer =
        let file = open_out ("java/" ^ filename ^ ".java") in
            fprintf file "%s" buffer

let print_recipe (recipe : a_stage) =
        let header = make_header recipe.sname true in
        let buffer = header in
        (*let buffer1 = buffer ^*)  
            write_out recipe.sname buffer




let start_gen (sast : a_program) (filename : string) =

        let phony2 = List.map print_recipe sast.stages in

        (*print java header*) 
        let header = make_header filename false in        
        write_out filename header
        
        




(* 
 *make a buffer for each Java file
 *add all the contents for each file to buffer
 *fprintf buffer to correct file for each buffer we have
 * *)








