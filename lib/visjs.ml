type state_serialization_config = {file_prefix:string; control2shape:(string,string)Hashtbl.t; control2color:(string,string) Hashtbl.t}
type node = {id:int;label:string;shape:string;color:string}
[@@deriving yojson_of]
type edge = { from: int; to_:int[@key "to"]; arrows: string }
[@@deriving yojson_of]
type network_data = { nodes : node list; edges:edge list}
[@@deriving yojson_of]
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
    Bigraph.Link.Ports.fold (fun from_idx to_idx res -> _big_link_2_edge ~from_idx ~to_idx "" :: res ) edge.Bigraph.Link.p []
let _link_graph_2_edges bigraph =
    Bigraph.Link.Lg.fold (fun edge res -> _link_graph_edge_2_edge edge :: res ) bigraph.Bigraph.Big.l [] |> List.flatten
let _state_2_edges bigraph =
    let edges_from_place_graph = _place_graph_2_edges bigraph
    and edges_from_link_graph = _link_graph_2_edges bigraph in
    List.append edges_from_place_graph edges_from_link_graph
let bigraph_2_network config bigraph = 
    let nodes = _state_2_nodes config bigraph
    and edges = _state_2_edges bigraph in
    {nodes;edges}
let network_to_json = [%yojson_of: network_data]
let _save_to_file ~filename ~content =
    let oc = open_out filename in (* create or truncate file, return channel *)
        Printf.fprintf oc "%s\n" content; (* write something *)   
    close_out oc 
let transformer_save_state_as_network config (part_res_cs,_) current_result = 
    let filename = config.file_prefix ^ "_state_T-"^(string_of_int part_res_cs.Phase3.time) 
    and content_raw = bigraph_2_network config part_res_cs.state.bigraph in
    let content = network_to_json content_raw |> string_of_yojson in
    _save_to_file ~filename ~content;
    current_result