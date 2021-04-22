type state_serialization_config = {directory:string;file_prefix:string; control2shape:(string,string)Hashtbl.t; control2color:(string,string) Hashtbl.t}
type node = {id:int;label:string;shape:string;color:string}
[@@deriving yojson, show]
type edge = { from: int; to_:int[@key "to"]; arrows: string }
[@@deriving yojson, show]
type network_data = { nodes : node list; edges:edge list}
[@@deriving yojson, show]
let _default_string_fun = fun str_opt -> match str_opt with | None -> "" | Some s -> s
let _big_node_2_node config index (ctrl:Bigraph.Ctrl.t) =
    let ctrl_str = Bigraph.Ctrl.name ctrl in
    let color = Hashtbl.find_opt config.control2color ctrl_str |> _default_string_fun
    and shape = Hashtbl.find_opt config.control2shape ctrl_str |> _default_string_fun in
    {id=index;label=(string_of_int index)^":"^ctrl_str;color;shape}
let _state_2_nodes config bigraph = 
    Bigraph.Nodes.fold (fun idx ctrl res -> let part_res = _big_node_2_node config idx ctrl in part_res::res ) (bigraph.Bigraph.Big.n) []
let _big_link_2_edge ~from_idx ~to_idx arrows =
    { from=from_idx; to_=to_idx; arrows }
let _place_graph_2_edges bigraph = 
    Bigraph.Sparse.fold (fun from_idx to_idx res -> _big_link_2_edge ~from_idx ~to_idx "to" :: res) bigraph.Bigraph.Big.p.nn []
let _link_graph_edge_2_edge edge =
    Bigraph.Link.Ports.fold 
        (
            fun from_idx to_idx res -> _big_link_2_edge ~from_idx ~to_idx "" :: res 
        ) 
        edge.Bigraph.Link.p 
        []
let hyperedge_node_tag = "hyperedge"
let _hyper_edge_2_node config node_id =
    let color = Hashtbl.find_opt config.control2color hyperedge_node_tag |> _default_string_fun
    and shape = Hashtbl.find_opt config.control2shape hyperedge_node_tag |> _default_string_fun in
    {id=node_id;label="";color;shape}
let _link_graph_edge_2_edges_and_node config edge ~max_node_id =
    let num_of_connections = Bigraph.Link.Ports.cardinal edge.Bigraph.Link.p in
    let res_edgs =
        if num_of_connections > 2 then (* many-many *)
            Bigraph.Link.Ports.fold 
            (fun from_idx _ res_edgs -> (_big_link_2_edge ~from_idx ~to_idx:(max_node_id+1) "")::res_edgs) 
            edge.Bigraph.Link.p
            []
            else (* 1-1 connection *)
                let nodes_to_connect = 
                    Bigraph.Link.Ports.fold 
                    (fun from_idx _ res -> from_idx::res )
                    edge.Bigraph.Link.p
                    [] in
                [_big_link_2_edge ~from_idx:(List.nth nodes_to_connect 0) ~to_idx:(List.nth nodes_to_connect 1) ""]
    in
    if num_of_connections > 2 then
        res_edgs,Some (_hyper_edge_2_node config (max_node_id+1))
    else
        res_edgs,None
let _link_graph_2_edges bigraph =
    Bigraph.Link.Lg.fold (fun edge res -> _link_graph_edge_2_edge edge :: res ) bigraph.Bigraph.Big.l [] |> List.flatten
let _link_graph_2_nodes_and_edges config bigraph max_node_id =
    let current_max_node_id = ref max_node_id in
    let res_edges,res_nodes = 
        Bigraph.Link.Lg.fold 
            (
                fun edge (res_edgs,res_nds) -> 
                    let edgs,node_opt = _link_graph_edge_2_edges_and_node config edge ~max_node_id:!current_max_node_id in
                    match node_opt with
                    | None -> edgs::res_edgs,res_nds
                    | Some node -> 
                        current_max_node_id := !current_max_node_id+1;
                        edgs::res_edgs,node::res_nds            
            ) 
            bigraph.Bigraph.Big.l 
            ([],[])
    in
    (List.flatten res_edges),res_nodes 
let _state_2_edges bigraph =
    let edges_from_place_graph = _place_graph_2_edges bigraph
    and edges_from_link_graph = _link_graph_2_edges bigraph in
    List.append edges_from_place_graph edges_from_link_graph
let _state_2_edges_and_nodes config bigraph max_node_id =
    let edges_from_place_graph = _place_graph_2_edges bigraph
    and edges_from_link_graph,nodes_from_link_graph = _link_graph_2_nodes_and_edges config bigraph max_node_id in
    List.append edges_from_place_graph edges_from_link_graph,nodes_from_link_graph
let bigraph_2_network config bigraph = 
    let nodes = _state_2_nodes config bigraph in
    let edges,nodes2 = _state_2_edges_and_nodes config bigraph ((List.length nodes)-1) in
    {nodes=nodes@nodes2;edges}
let _network_to_json = [%yojson_of: network_data]
type state_serialized_raw = {sat_config:Ssp.Template_state.t;network:network_data;ui_map:Ui.map}
type state_serialized = {sat_config:(int*int)array;network_data:string[@key "network_data_file"];ui_map:string}
[@@deriving yojson, show]
let network_filename config time_moment =
     if config.directory <> "" then
        config.directory^"\\"^config.file_prefix^"-network-T:"^(string_of_int time_moment)^".json"
    else
        config.file_prefix^"-network-T:"^(string_of_int time_moment)^".json"
let state_serialized_filename config time_moment =
    if config.directory <> "" then
        config.directory^"\\"^config.file_prefix^"-state-T:"^(string_of_int time_moment)^".json"
    else
        config.file_prefix^"-state-T:"^(string_of_int time_moment)^".json"
let state_serialized_filename_regex config =
    config.file_prefix^"-state-T:[0-9]+.json"
let _raw_2_exported (ssr:state_serialized_raw) (config:state_serialization_config) time_moment =
    let network_filename = network_filename config time_moment in
        {sat_config = ssr.sat_config;network_data=network_filename;ui_map=Ui.map_to_string ssr.ui_map}
let _make_ssr sat net uim =
    {network=net;sat_config=sat;ui_map=uim}
let transformer_save_state config (part_res_cs,_) current_result = 
    let filename = state_serialized_filename config part_res_cs.Phase3.time 
    and network_filename = network_filename config part_res_cs.Phase3.time 
    and network = bigraph_2_network config part_res_cs.state.bigraph in
    let content_raw = _make_ssr part_res_cs.sat_config network part_res_cs.ui_map in
    let network_json = [%yojson_of: network_data] network 
    and content_json = (_raw_2_exported content_raw config part_res_cs.time) |> [%yojson_of: state_serialized]  in
    Yojson.Safe.to_file filename content_json;
    Yojson.Safe.to_file network_filename network_json ;
    current_result
type _style = {color:string; background_color:string;}
let _style_2_string style = "{color:"^style.color^"; background-color:"^style.background_color^";}"
type _timeline_item_raw = { start_time: int; end_time: int; object_name: string; style:_style; object_id:int}
type timeline_item = { start_time: int[@key "start"]; end_time: int[@key "end"]; content: string; style:string; group:int}
[@@deriving yojson, show]
type timeline = timeline_item list
[@@deriving yojson, show]
type timeline_config = {directory:string;name:string;mutable known_objects:Common.IntSet.t;mutable current_timeline:timeline;}
type group = {id:int;content:string}
[@@deriving yojson, show]
let _conv_raw_timeline (ti:_timeline_item_raw) = {start_time=ti.start_time; end_time=ti.end_time; content=ti.object_name; style=_style_2_string ti.style; group=ti.object_id}
let timeline_filename (config:timeline_config) = 
    if config.directory <> "" then
        config.directory^"\\"^config.name^"-timeline.json"
    else
        config.name^"-timeline.json"
let _save_timeline config tl = 
    let tl_json = [%yojson_of: timeline] tl in
    Yojson.Safe.to_file (timeline_filename config) tl_json
let set_of_participants_2_groups sop = 
    Common.IntSet.fold (fun participant_id res -> {id=participant_id;content=(string_of_int participant_id)}::res) sop []
let groups_filename (config:timeline_config) = 
    if config.directory <> "" then
        config.directory^"\\"^config.name^"-groups.json"
    else
        config.name^"-groups.json"
let _save_groups config gs =
    let gs_json = [%yojson_of: group list] gs in
    Yojson.Safe.to_file (groups_filename config) gs_json
let _time_info_2_participating_objects_set ti =
    List.fold_left (fun res object_id -> Common.IntSet.add object_id res)
    Common.IntSet.empty
    ti.Phase3.participants
let _gen_style () : _style =
    let hue = Random.int 360 in
    {color="white";background_color="hsl("^(string_of_int hue)^", 75%, 50%)"}
let _time_info_2_raw_timeline_items (ti:Phase3.time_info) =
    let style = _gen_style () in
    List.map 
    (
        fun object_id -> 
            {start_time=ti.start_time; end_time=ti.end_time; object_name=string_of_int object_id; style; object_id;}    
    )
    ti.Phase3.participants
let time_infos_2_timeline tis = 
    List.map (fun ti -> _time_info_2_raw_timeline_items ti ) tis |> List.flatten |> List.map (fun tir -> _conv_raw_timeline tir)
let transformer_save_timeline config (_,time_infos) current_result =
    let new_timeline_items = time_infos_2_timeline time_infos
    and currently_participating_objects = 
        List.fold_left 
        (fun res ti -> Common.IntSet.union res (_time_info_2_participating_objects_set ti) ) 
        Common.IntSet.empty 
        time_infos in
    let full_timeline = List.append new_timeline_items config.current_timeline 
    and are_groups_extended = not (Common.IntSet.subset currently_participating_objects config.known_objects) in
    (if are_groups_extended then 
        let groups = set_of_participants_2_groups (Common.IntSet.union config.known_objects currently_participating_objects) in
        config.known_objects <- (Common.IntSet.union currently_participating_objects config.known_objects);
        _save_groups config groups
    );
    config.current_timeline <- full_timeline;
    _save_timeline config full_timeline;
    current_result