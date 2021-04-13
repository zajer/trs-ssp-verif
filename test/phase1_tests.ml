open OUnit2
open Ssp_verification
let _map_states_to_idx list = 
    let result = Hashtbl.create (List.length list) in
    List.iter (fun state -> Hashtbl.add result state.Tracking_bigraph.TTS.index state.bigraph  ) list;
    result
let _map_trans_to_idx list = 
    let result = Hashtbl.create (List.length list) in
    List.iteri (fun i trans -> Hashtbl.add result i trans  ) list;
    result
let _map_trans_to_key trans_list =
    let result = Hashtbl.create (List.length trans_list) in
    List.iter 
        (fun trans -> 
            let in_state_idx = trans.Tracking_bigraph.TTS.in_state_idx
            and react_label = trans.Tracking_bigraph.TTS.react_label in
            let key = in_state_idx,react_label in
            Hashtbl.add result key trans
        )
        trans_list;
    result
let _compare_constructed_states sl1 sl2 = 
    if (List.compare_lengths sl1 sl2 = 0 ) then
        List.for_all2 
            (
                fun s1 s2 ->  
                s1.Phase3.state.index = s2.Phase3.state.index
                &&
                Ui.are_equal s1.ui_map s2.ui_map
                &&
                s1.time = s2.time
            )
            sl1
            sl2
    else
        false
let _constructed_state2string cs = 
    "(state:"^(cs.Phase3.state.index |> string_of_int)^" ui map:"^(Ui.map_to_string cs.ui_map)^" time:"^(string_of_int cs.time)^")"
let test_perform_phase_1 _ = 
    let states_filename = "exmp_1_states.csv"
    and trans_filename = "exmp_1_trans_normed.csv"
    and walk_filename = "exmp_1_walk.csv"
    and number_of_agents = 2 in
    let states = Tracking_bigraph.TTS.import_states states_filename
    and trans = Tracking_bigraph.TTS.import_transitions ~headers_in_first_row:false trans_filename
    and walk = Ssp.Frontend.import_trans_funs walk_filename in
    let states_by_idx = _map_states_to_idx states
    and trans_by_idx = _map_trans_to_idx trans
    and trans_by_key = _map_trans_to_key trans in
    let result = Phase1.perform_phase walk ~num_of_agents:number_of_agents states_by_idx trans_by_idx trans_by_key (fun _ x -> x)
    and expected_status = true 
    and expected_states =
        [
            {
                Phase3.state=(List.nth states 4);
                ui_map=[(1,0);(2,1);(3,2);(4,3);(5,4)] |> Ui.make_map_of_list;
                sat_config=[|(1,0);(2,0)|];time=0
            };
            {
                Phase3.state=(List.nth states 4);
                ui_map=[(1,0);(2,1);(3,2);(4,3);(5,4)] |> Ui.make_map_of_list;
                sat_config=[|(1,0);(2,0)|];time=1
            };
            {
                Phase3.state=(List.nth states 4);
                ui_map=[(1,0);(2,1);(3,2);(4,3);(5,4)] |> Ui.make_map_of_list;
                sat_config=[|(1,0);(2,0)|];time=2
            };
            {
                Phase3.state=(List.nth states 3);
                ui_map=[(1,0);(4,1);(3,3);(2,2);(5,4)] |> Ui.make_map_of_list;
                sat_config=[|(2,3);(1,0)|];time=3
            };
            {
                Phase3.state=(List.nth states 3);
                ui_map=[(1,0);(4,1);(3,3);(2,2);(5,4)] |> Ui.make_map_of_list;
                sat_config=[|(2,3);(1,3)|];time=4
            };
            {
                Phase3.state=(List.nth states 5);
                ui_map=[(4,0);(5,1);(2,2);(1,3);(3,4)] |> Ui.make_map_of_list;
                sat_config=[|(1,5);(2,5)|];time=5
            };
        ]
     in
    assert_equal
        ~msg:"Phase 1 should end successfuly"
        expected_status
        result.is_successful;
    let result_constructed_states,_ = result.value in
    assert_equal
        ~msg:"Constructed states are not equal to expected"
        ~cmp:_compare_constructed_states
        ~printer:(fun sl -> let result = List.map _constructed_state2string sl |> String.concat "\n\t" in "\n[\n\t"^result^"\n]" )
        expected_states
        result_constructed_states
    
let suite =
    "Phase 1" >::: [
        "Perform phase test">:: test_perform_phase_1;
    ]

let () =
  run_test_tt_main suite