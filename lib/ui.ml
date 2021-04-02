module IntMapping =
struct
    type t = int * int
    let compare (ui0,v0) (ui1,v1) =
    match Stdlib.compare ui0 ui1 with
        | 0 -> Stdlib.compare v0 v1
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
                let corresponding_transformation = IntMappingsSet.find_first (fun (chk_ui,_) -> chk_ui = mapped_val) codom_mapping in
                match corresponding_transformation with
                | _, new_mapped_val -> ui,new_mapped_val
        )
        transformed_mapping
let is_subset ~target ~subset =
    IntMappingsSet.subset subset target