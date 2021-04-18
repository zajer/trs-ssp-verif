open OUnit2
open Ssp_verification

let test_constructed_state_2_network_1 _ =
    let bigraph = "{(0, X:0),(1, Y:0),(2, Z:0)}\n0 3 0\n000\n000\n000" |> Bigraph.Big.of_string
    and config = {Visjs.file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
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
    and config = {Visjs.file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
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
    and config = {Visjs.file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
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
let suite =
    "Visjs" >::: [
        "State conversion test 1">:: test_constructed_state_2_network_1;
        "State conversion test 2">:: test_constructed_state_2_network_2;
        "State conversion test 3">:: test_constructed_state_2_network_3;
    ]

let () =
  run_test_tt_main suite