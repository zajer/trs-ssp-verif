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
type config_raw = {scenario_description:string; controls_2_color:(string*string) list} [@@deriving of_yojson]
type config = {directory:string; description:string; short_name:string; control2color:(string,string) Hashtbl.t}
let convert config_raw short_name =
    let control2color = Hashtbl.create (List.length config_raw.controls_2_color)
    and directory = (Str.global_replace (Str.regexp "\\s") "-" short_name)^"_content"  in
    List.iter (fun (control,color) -> Hashtbl.add control2color control color ) config_raw.controls_2_color;
        {directory; description=config_raw.scenario_description ; short_name ; control2color}
let make_visjs_timeline_config config = 
    {Visjs.directory=config.directory;name=config.short_name;known_objects=Common.IntSet.empty;current_timeline=[];}
let make_visjs_serialization_config config = 
    {Visjs.directory=config.directory;file_prefix=config.short_name; control2color=config.control2color}
let make_output_info config =
    let tmp1 = make_visjs_timeline_config config
    and tmp2 = make_visjs_serialization_config config in
    {Phase1.name=config.description;is_scenario_valid=true;message=None;directory=config.directory;timeline_filename=Visjs.timeline_filename tmp1; groups_filename=Visjs.groups_filename tmp1; states_regex=Visjs.state_serialized_filename_regex tmp2}