module UIMapping =
struct
    type t = int * int
    let compare (ui0,_) (ui1,_) =
    match Stdlib.compare ui0 ui1 with
        | 0 -> 0
        | c -> c
end
module UIMappingsSet = Set.Make(UIMapping)
type t = UIMapping.t
type map = UIMappingsSet.t
let empty_map = UIMappingsSet.empty
let make_map_of_list source = 
    UIMappingsSet.of_list source
let transform_codom keep_unmapped ~transformed_mapping ~codom_mapping = 
    UIMappingsSet.filter_map 
        (
            fun (ui,mapped_val) -> 
                let corresponding_transformation = UIMappingsSet.find_opt (mapped_val,-1) codom_mapping in
                match corresponding_transformation with
                | Some (_, new_mapped_val) -> Some (ui,new_mapped_val)
                | None -> if keep_unmapped then Some (ui,mapped_val) else None
        )
        transformed_mapping
let union ~base ~extension =
    UIMappingsSet.fold 
    (
        fun (chk_ui,new_mapping) extended_set -> 
            if not (UIMappingsSet.exists (fun (ui,_) -> ui=chk_ui ) base) then
                UIMappingsSet.add (chk_ui,new_mapping) extended_set
            else
                extended_set
    )
    extension
    base
let are_equal s1 s2 =
    if UIMappingsSet.cardinal s1 = UIMappingsSet.cardinal s2 then
        let s1els = UIMappingsSet.elements s1
        and s2els = UIMappingsSet.elements s2 in
        List.for_all2 (fun es1 es2 -> es1 = es2) s1els s2els
    else
        false
let is_subset ~target ~subset =
    let intersection = UIMappingsSet.inter target subset in
        if UIMappingsSet.cardinal intersection = UIMappingsSet.cardinal subset then
            are_equal intersection subset
        else
            false
let domain map = 
    UIMappingsSet.elements map |> List.map (fun (d,_)-> d)
let inverse map = 
    let source = UIMappingsSet.elements map in
    let inverse_source = List.map (fun (i1,i2)-> i2,i1) source in
    let result = make_map_of_list inverse_source in
    result
let mapping_to_string (i1,i2) = "("^(string_of_int i1 )^","^(string_of_int i2)^")"
let map_to_string map = let res = UIMappingsSet.elements map |> List.map (fun mapping -> mapping_to_string mapping) |> String.concat ";" in "{"^res^"}"