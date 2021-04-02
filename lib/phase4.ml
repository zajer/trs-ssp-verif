open Tracking_bigraph
type mapped_reacts = (int,Tracking_bigraph.TTS.trans_exported) Hashtbl.t
let is_update_with_trans_possible ~ui2state_map trans ~ui2par_map =
    let codom_mapping = Ui.make_map_of_list (Bigraph.Iso.to_list trans.TTS.participants) in
    let ui2state_map_from_par = Ui.transform_codom ~transformed_mapping:ui2par_map ~codom_mapping in
    Ui.is_subset ~subset:ui2state_map_from_par ~target:ui2state_map
type mapped_trans = ((int*string),TTS.trans_exported) Hashtbl.t
let rec _find_trans_allowing_for_update ~ui2state_map ~ui2par_map trans_with_same_key =
    match trans_with_same_key with 
    | [] -> false, None
    | h::t -> 
        if is_update_with_trans_possible ~ui2state_map ~ui2par_map h then
            true,Some h
        else
            _find_trans_allowing_for_update ~ui2state_map ~ui2par_map t
let is_update_possible ~ui2state_map ~ui2par_map trans_to_be_applied all_trans = 
    let key = trans_to_be_applied.TTS.in_state_idx,trans_to_be_applied.react_label in
    let known_similar_transitions = Hashtbl.find_all all_trans key in
    _find_trans_allowing_for_update ~ui2state_map ~ui2par_map known_similar_transitions


