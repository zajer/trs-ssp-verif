open OUnit2
open Ssp_verification

let test_constructed_state_2_network_1 _ =
    let bigraph = "{(0, X:0),(1, Y:0),(2, Z:0)}\n0 3 0\n000\n000\n000" |> Bigraph.Big.of_string
    and config = {Visjs.directory="";file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
    let result = Visjs.bigraph_2_network config bigraph
    and expected_result = 
        {
            Visjs.nodes=
                [
                    {Visjs.id=2;label="2:Z";color="";shape=""};
                    {Visjs.id=1;label="1:Y";color="";shape=""};
                    {Visjs.id=0;label="0:X";color="";shape=""}
                ];
            edges=[]
        } in
    assert_equal
        ~msg:"Constructed network is not equal to expected"
        ~printer:[%derive.show: Visjs.network_data]
        expected_result
        result
let test_constructed_state_2_network_2 _ =
    let bigraph = "{(0, X:0),(1, Y:0),(2, Z:0)}\n0 3 0\n010\n001\n000" |> Bigraph.Big.of_string
    and config = {Visjs.directory="";file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
    let result = Visjs.bigraph_2_network config bigraph
    and expected_result = 
        {
            Visjs.nodes=
                [
                    {Visjs.id=2;label="2:Z";color="";shape=""};
                    {Visjs.id=1;label="1:Y";color="";shape=""};
                    {Visjs.id=0;label="0:X";color="";shape=""}
                ];
            edges=
                [
                    {Visjs.from=1; to_=2;arrows="to"};
                    {Visjs.from=0; to_=1;arrows="to"}
                ]
        } in
    assert_equal
        ~msg:"Constructed network is not equal to expected"
        ~printer:[%derive.show: Visjs.network_data]
        expected_result
        result
let test_constructed_state_2_network_3 _ =
    let bigraph = "{(0, X:1),(1, Y:1),(2, Z:1),(3, W:1)}\n0 4 0\n0100\n0010\n0000\n0000\n({}, {}, {(0, 1), (1, 1), (2, 1)})\n({}, {}, {(2, 1), (3, 1)})" |> Bigraph.Big.of_string
    and config = {Visjs.directory="";file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
    let result = Visjs.bigraph_2_network config bigraph
    and expected_result = 
        {
            Visjs.nodes=
                [
                    {Visjs.id=3;label="3:W";color="";shape=""};
                    {Visjs.id=2;label="2:Z";color="";shape=""};
                    {Visjs.id=1;label="1:Y";color="";shape=""};
                    {Visjs.id=0;label="0:X";color="";shape=""};
                    {Visjs.id=4;label="";color="";shape=""};
                ];
            edges=
                [
                    {Visjs.from=1; to_=2;arrows="to"};
                    {Visjs.from=0; to_=1;arrows="to"};
                    {Visjs.from=3; to_=2;arrows=""};
                    {Visjs.from=2; to_=4;arrows=""};
                    {Visjs.from=1; to_=4;arrows=""};
                    {Visjs.from=0; to_=4;arrows=""}
                ]
        } in
    assert_equal
        ~msg:"Constructed network is not equal to expected"
        ~printer:[%derive.show: Visjs.network_data]
        expected_result
        result
let test_save_state_as_network ctx =
    let tmp_dir_name = bracket_tmpdir ctx in
    let bigraph = "{(0, X:1),(1, Y:1),(2, Z:1),(3, W:1)}\n0 4 0\n0100\n0010\n0000\n0000\n({}, {}, {(0, 1), (1, 1), (2, 1)})\n({}, {}, {(2, 1), (3, 1)})" |> Bigraph.Big.of_string 
    and sat_config = [|(1,2);(2,3)|]
    and ui_map = Ui.make_map_of_list [(0,7);(1,5);(2,12)]
    and config = {Visjs.directory=tmp_dir_name; file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} 
    and time_moment = 777 in 
    let network = Visjs.bigraph_2_network config bigraph 
    and input_result = {Phase1.is_successful=true;value=[],[];error_message=None} 
    and input_state = {Phase3.state={Tracking_bigraph.TTS.bigraph;index=777};ui_map;sat_config;time=time_moment} in
    let result = Visjs.transformer_save_state config (input_state,[]) input_result 
    and expected_result = input_result in
    let saved_state = Yojson.Safe.from_file (Visjs.state_serialized_filename config time_moment) |> [%of_yojson: Visjs.state_serialized] 
    and saved_network = Yojson.Safe.from_file (Visjs.network_filename config time_moment) |> [%of_yojson: Visjs.network_data]  in
    assert_equal
        ~msg:"Returned result should not be altered"
        expected_result
        result;
    assert_equal
        ~msg:"Saved state not equal to expected"
        ~printer:[%show: Visjs.state_serialized]
        {Visjs.sat_config;network_data=(Visjs.network_filename config time_moment);ui_map=Ui.map_to_string ui_map}
        saved_state;
    assert_equal
        ~msg:"Saved network not equal to expected"
        ~printer:[%show: Visjs.network_data]
        network
        saved_network
let suite =
    "Visjs" >::: [
        "State conversion test 1">:: test_constructed_state_2_network_1;
        "State conversion test 2">:: test_constructed_state_2_network_2;
        "State conversion test 3">:: test_constructed_state_2_network_3;
        "Saving state">:: test_save_state_as_network;
    ]

let () =
  run_test_tt_main suite