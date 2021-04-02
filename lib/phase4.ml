open Tracking_bigraph
type mapped_reacts = (int,Tracking_bigraph.TTS.trans_exported) Hashtbl.t
let is_update_with_trans_possible ~ui2state_map trans ~ui2par_map =
    let codom_mapping = Ui.make_map_of_list (Bigraph.Iso.to_list trans.TTS.participants) in
    let ui2state_map_from_par = Ui.transform_codom ~transformed_mapping:ui2par_map ~codom_mapping in
    Ui.is_subset ~subset:ui2state_map_from_par ~target:ui2state_map