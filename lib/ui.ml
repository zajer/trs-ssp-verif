module IntMapping =
struct
    type t = int * int
    let compare (ui0,_) (ui1,_) =
    match Stdlib.compare ui0 ui1 with
        | 0 -> 0
        | c -> c
end
module IntMappingsSet = Set.Make(IntMapping)
type t = IntMapping.t
type map = IntMappingsSet.t
let make_map_of_list source = 
    IntMappingsSet.of_list source
let transform_codom ~transformed_mapping ~codom_mapping = 
    IntMappingsSet.map 
        (
            fun (ui,mapped_val) -> 
                let corresponding_transformation = IntMappingsSet.find_opt (mapped_val,-1) codom_mapping in
                match corresponding_transformation with
                | Some (_, new_mapped_val) -> ui,new_mapped_val
                | None -> ui,mapped_val
        )
        transformed_mapping
let is_subset ~target ~subset =
    IntMappingsSet.subset subset target
let union ~base ~extension =
    IntMappingsSet.fold 
    (
        fun (chk_ui,new_mapping) extended_set -> 
            if not (IntMappingsSet.exists (fun (ui,_) -> ui=chk_ui ) base) then
                IntMappingsSet.add (chk_ui,new_mapping) extended_set
            else
                extended_set
    )
    extension
    base
let are_equal s1 s2 = 
    let s1els = IntMappingsSet.elements s1
    and s2els = IntMappingsSet.elements s2 in
    List.for_all2 (fun es1 es2 -> es1 = es2) s1els s2els
let mapping_to_string (i1,i2) = "("^(string_of_int i1 )^","^(string_of_int i2)^")"
let map_to_string map = let res = IntMappingsSet.elements map |> List.map (fun mapping -> mapping_to_string mapping) |> String.concat ";" in "{"^res^"}"