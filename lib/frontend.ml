let map_states_to_idx states_list = 
    let result = Hashtbl.create (List.length states_list) in
    List.iter (fun state -> Hashtbl.add result state.Tracking_bigraph.TTS.index state.bigraph  ) states_list;
        result
let map_trans_to_idx trans_list = 
    let result = Hashtbl.create (List.length trans_list) in
    List.iteri (fun i trans -> Hashtbl.add result i trans  ) trans_list;
        result
let map_trans_to_key trans_list =
    let result = Hashtbl.create (List.length trans_list) in
    List.iter 
        (
            fun trans -> 
                let in_state_idx = trans.Tracking_bigraph.TTS.in_state_idx
                and react_label = trans.Tracking_bigraph.TTS.react_label in
                let key = in_state_idx,react_label in
                Hashtbl.add result key trans
        )
        trans_list;
    result

