open OUnit2
open Ssp_verification
let _int_list2tstring il =
    "["^(List.map (fun i -> string_of_int i) il |> String.concat ";")^"]"
let _tinfo2string tinfo = 
    let participants_str = _int_list2tstring tinfo.Phase3.participants 
    and transition_idx_str = string_of_int tinfo.transition_idx
    and new_objs_str = _int_list2tstring tinfo.new_objs
    and term_objs_str = _int_list2tstring tinfo.term_objs
    and start_time_str = string_of_int tinfo.start_time
    and end_time_str = string_of_int tinfo.end_time in
    let result = ["participants:"^participants_str;"react_label:"^tinfo.react_label;"transition_idx:"^transition_idx_str;"new_objs:"^new_objs_str;"term_objs:"^term_objs_str;"start_time:"^start_time_str;"end_time:"^end_time_str] |> String.concat ";" in
    "{"^result^"}"
let test_extract_time_info_1 _ = 
    let ui2state_before_transition = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
    and ui2state_after_transition = [(0,2);(1,0);(2,1)] |> Ui.make_map_of_list
    and ui2redex = [(0,0);(2,2)] |> Ui.make_map_of_list
    and start_time = 777
    and duration_of_transition = 21
    and transition_idx = 7
    and react_label = "my react" in
    let result = Phase3.extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~start_time ~duration_of_transition ~transition_idx react_label 
    and expected_result = {Phase3.participants=[0;2] ; react_label; transition_idx; new_objs=[]; term_objs=[]; start_time=777; end_time=798} in
    assert_equal
        ~msg:"Result time info is not equal to expected"
        ~printer:_tinfo2string
        expected_result
        result
let test_extract_time_info_2 _ = 
    let ui2state_before_transition = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
    and ui2state_after_transition = [(0,2);(1,0);(2,3);(3,1)] |> Ui.make_map_of_list
    and ui2redex = [(0,0);(2,2)] |> Ui.make_map_of_list
    and start_time = 777
    and duration_of_transition = 21
    and transition_idx = 7
    and react_label = "my react" in
    let result = Phase3.extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~start_time ~duration_of_transition ~transition_idx react_label 
    and expected_result = {Phase3.participants=[0;2] ; react_label; transition_idx; new_objs=[3]; term_objs=[]; start_time=777; end_time=798} in
    assert_equal
        ~msg:"Result time info is not equal to expected"
        ~printer:_tinfo2string
        expected_result
        result
let test_extract_time_info_3 _ = 
    let ui2state_before_transition = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
    and ui2state_after_transition = [(0,1);(2,0)] |> Ui.make_map_of_list
    and ui2redex = [(0,0);(2,2)] |> Ui.make_map_of_list
    and start_time = 777
    and duration_of_transition = 21
    and transition_idx = 7
    and react_label = "my react" in
    let result = Phase3.extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~start_time ~duration_of_transition ~transition_idx react_label 
    and expected_result = {Phase3.participants=[0;2] ; react_label; transition_idx; new_objs=[]; term_objs=[1]; start_time=777; end_time=798} in
    assert_equal
        ~msg:"Result time info is not equal to expected"
        ~printer:_tinfo2string
        expected_result
        result
let test_extract_time_info_4 _ = 
    let ui2state_before_transition = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
    and ui2state_after_transition = [(0,1);(3,0);(2,2)] |> Ui.make_map_of_list
    and ui2redex = [(0,0);(2,2)] |> Ui.make_map_of_list
    and start_time = 777
    and duration_of_transition = 21
    and transition_idx = 7
    and react_label = "my react" in
    let result = Phase3.extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~start_time ~duration_of_transition ~transition_idx react_label 
    and expected_result = {Phase3.participants=[0;2] ; react_label; transition_idx; new_objs=[3]; term_objs=[1]; start_time=777; end_time=798} in
    assert_equal
        ~msg:"Result time info is not equal to expected"
        ~printer:_tinfo2string
        expected_result
        result
let _compare_ewalks ew1 ew2 =
    if (List.length ew1) = (List.length ew2) then
        List.for_all2 (fun ew1e ew2e -> 
            Ui.are_equal ew1e.Phase3.ui2redex ew2e.Phase3.ui2redex
            
                
        ) ew1 ew2
    else
        false
let _tinfo_list2string = (fun til -> let res = List.map (fun ti -> _tinfo2string ti) til |> String.concat ";\n\t" in "\n[\n\t"^res^"\n]")
let test_perform_phase_1 _ =
    let previous_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_one_agent_previous_state;index=0}
    and previous_state_mapping = [(1,0);(2,1);(3,2)] |> Ui.make_map_of_list 
    and previous_sat_config = [|(1,0)|]
    and ewalk = Phase3_tests_data.phase_test_one_agent_ewalk 
    and constructed_time_moment = 1
    and num_of_agents = 1
    and all_states = Phase3_tests_data.phase_test_one_agent_all_states
    and all_trans_by_idx = Phase3_tests_data.phase_test_one_agent_all_trans_by_idx
    and all_trans_by_keys = Phase3_tests_data.phase_test_one_agent_all_trans_by_keys in
    let state_at_previous_moment = {Phase3.state=previous_state;ui_map=previous_state_mapping;sat_config=previous_sat_config;time=constructed_time_moment-1} in
    let result_state,unused_ewalk,result_time_infos = Phase3.perform_phase
                    state_at_previous_moment
                    ewalk
                    ~constructed_time_moment
                    ~num_of_agents
                    all_states
                    all_trans_by_idx
                    all_trans_by_keys 
    and expected_unused_walk = []
    and expected_result_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_one_agent_output_state;Tracking_bigraph.TTS.index=1}
    and expected_state_mapping = [(1,0);(2,1);(3,2)] |> Ui.make_map_of_list
    and expected_result_sat = [|(1,1)|]
    and expected_time_infos = [{Phase3.participants=[1]; react_label="PT-1A-R1"; transition_idx=1; new_objs=[]; term_objs=[]; start_time=0; end_time=1}] in
    assert_equal
        ~msg:"There should be no unused walk elements as the result"
        ~cmp:_compare_ewalks
        expected_unused_walk
        unused_ewalk;
    assert_equal
        ~msg:"Result state is not equal to expected"
        ~cmp:(fun s1 s2-> (Bigraph.Big.equal s1.Tracking_bigraph.TTS.bigraph s2.Tracking_bigraph.TTS.bigraph) && s1.index = s2.index )
        expected_result_state
        result_state.state;
    assert_equal
        ~msg:"Result state mapping is not equal to expected"
        ~printer:Ssp_verification.Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_state_mapping
        result_state.ui_map;
    assert_equal
        ~msg:"Result SAT configuration is not equal to expected"
        ~printer:Ssp.Template_state.to_stirng
        expected_result_sat
        result_state.sat_config;
    assert_equal
        ~msg:"Result state's time moment is not equal to expected"
        constructed_time_moment
        result_state.time;
    assert_equal
        ~msg:"Result time infos are not equal to expected"
        ~printer:_tinfo_list2string
        expected_time_infos
        result_time_infos
let test_perform_phase_2 _ =
    let previous_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_one_agent_previous_state;index=0}
    and previous_state_mapping = [(1,0);(2,1);(3,2)] |> Ui.make_map_of_list 
    and previous_sat_config = [|(1,1)|]
    and ewalk = Phase3_tests_data.phase_test_one_agent_ewalk 
    and constructed_time_moment = 1
    and num_of_agents = 1
    and all_states = Phase3_tests_data.phase_test_one_agent_all_states
    and all_trans_by_idx = Phase3_tests_data.phase_test_one_agent_all_trans_by_idx
    and all_trans_by_keys = Phase3_tests_data.phase_test_one_agent_all_trans_by_keys in
    let state_at_previous_moment = {Phase3.state=previous_state;ui_map=previous_state_mapping;sat_config=previous_sat_config;time=constructed_time_moment-1} in
    let result_state,unused_ewalk,result_time_infos = Phase3.perform_phase
                    state_at_previous_moment
                    ewalk
                    ~constructed_time_moment
                    ~num_of_agents
                    all_states
                    all_trans_by_idx
                    all_trans_by_keys 
    and expected_unused_walk = ewalk
    and expected_result_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_one_agent_previous_state;Tracking_bigraph.TTS.index=0}
    and expected_state_mapping = [(1,0);(2,1);(3,2)] |> Ui.make_map_of_list
    and expected_result_sat = [|(1,1)|] 
    and expected_time_infos = [] in
    assert_equal
        ~msg:"There should be one unused walk element as the result"
        ~cmp:_compare_ewalks
        expected_unused_walk
        unused_ewalk;
    assert_equal
        ~msg:"Result state is not equal to expected"
        ~cmp:(fun s1 s2-> (Bigraph.Big.equal s1.Tracking_bigraph.TTS.bigraph s2.Tracking_bigraph.TTS.bigraph) && s1.index = s2.index )
        expected_result_state
        result_state.state;
    assert_equal
        ~msg:"Result state mapping is not equal to expected"
        ~printer:Ssp_verification.Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_state_mapping
        result_state.ui_map;
    assert_equal
        ~msg:"Result SAT configuration is not equal to expected"
        ~printer:Ssp.Template_state.to_stirng
        expected_result_sat
        result_state.sat_config;
    assert_equal
        ~msg:"Result state's time moment is not equal to expected"
        constructed_time_moment
        result_state.time;
    assert_equal
        ~msg:"Result time infos are not equal to expected"
        ~printer:_tinfo_list2string
        expected_time_infos
        result_time_infos
let test_perform_phase_3 _ =
    let previous_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_two_agents_previous_state;index=0}
    and previous_state_mapping = [(1,0);(2,1);(3,2);(4,3)] |> Ui.make_map_of_list 
    and previous_sat_config = [|(1,0);(2,0)|]
    and ewalk = Phase3_tests_data.phase_test_two_agents_ewalk_1 
    and constructed_time_moment = 1
    and num_of_agents = 2
    and all_states = Phase3_tests_data.phase_test_two_agents_all_states
    and all_trans_by_idx = Phase3_tests_data.phase_test_two_agents_all_trans_by_idx
    and all_trans_by_keys = Phase3_tests_data.phase_test_two_agents_all_trans_by_keys () in
    let state_at_previous_moment = {Phase3.state=previous_state;ui_map=previous_state_mapping;sat_config=previous_sat_config;time=constructed_time_moment-1} in
    let result_state,unused_ewalk,result_time_infos = Phase3.perform_phase
                    state_at_previous_moment
                    ewalk
                    ~constructed_time_moment
                    ~num_of_agents
                    all_states
                    all_trans_by_idx
                    all_trans_by_keys 
    and expected_unused_walk = []
    and expected_result_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_two_agents_output_state;Tracking_bigraph.TTS.index=2}
    and expected_state_mapping = [(2,0);(1,1);(3,2);(4,3)] |> Ui.make_map_of_list
    and expected_result_sat = [|(1,1);(2,1)|] 
    and expected_time_infos = 
        [
            {Phase3.participants=[1;3;4]; react_label="PT-2A-R1"; transition_idx=3; new_objs=[]; term_objs=[]; start_time=0; end_time=1};
            {Phase3.participants=[2;3;4]; react_label="PT-2A-R1"; transition_idx=2; new_objs=[]; term_objs=[]; start_time=0; end_time=1}
        ] in
    assert_equal
        ~msg:"There should be no unused walk element as the result"
        ~cmp:_compare_ewalks
        ~printer:(fun ew -> "walk of length:"^(string_of_int (List.length ew)))
        expected_unused_walk
        unused_ewalk;
    assert_equal
        ~msg:"Result state is not equal to expected"
        ~cmp:(fun s1 s2-> (Bigraph.Big.equal s1.Tracking_bigraph.TTS.bigraph s2.Tracking_bigraph.TTS.bigraph) && s1.index = s2.index )
        expected_result_state
        result_state.state;
    assert_equal
        ~msg:"Result UI on state's nodes mapping is not equal to expected"
        ~printer:Ssp_verification.Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_state_mapping
        result_state.ui_map;
    assert_equal
        ~msg:"Result SAT configuration is not equal to expected"
        ~printer:Ssp.Template_state.to_stirng
        expected_result_sat
        result_state.sat_config;
    assert_equal
        ~msg:"Result state's time moment is not equal to expected"
        constructed_time_moment
        result_state.time;
    assert_equal
        ~msg:"Result time infos are not equal to expected"
        ~printer:_tinfo_list2string
        expected_time_infos
        result_time_infos
let test_perform_phase_4 _ =
    let previous_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_two_agents_previous_state;index=0}
    and previous_state_mapping = [(1,0);(2,1);(3,2);(4,3)] |> Ui.make_map_of_list 
    and previous_sat_config = [|(1,0);(2,0)|]
    and ewalk = Phase3_tests_data.phase_test_two_agents_ewalk_2 
    and constructed_time_moment = 1
    and num_of_agents = 2
    and all_states = Phase3_tests_data.phase_test_two_agents_all_states
    and all_trans_by_idx = Phase3_tests_data.phase_test_two_agents_all_trans_by_idx
    and all_trans_by_keys = Phase3_tests_data.phase_test_two_agents_all_trans_by_keys () in
    let state_at_previous_moment = {Phase3.state=previous_state;ui_map=previous_state_mapping;sat_config=previous_sat_config;time=constructed_time_moment-1} in
    let result_state,unused_ewalk,result_time_infos = Phase3.perform_phase
                    state_at_previous_moment
                    ewalk
                    ~constructed_time_moment
                    ~num_of_agents
                    all_states
                    all_trans_by_idx
                    all_trans_by_keys 
    and expected_unused_walk = []
    and expected_result_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_two_agents_output_state;Tracking_bigraph.TTS.index=2}
    and expected_state_mapping = [(1,0);(2,1);(3,2);(4,3)] |> Ui.make_map_of_list
    and expected_result_sat = [|(1,1);(2,1)|]
    and expected_time_infos = 
        [
            {Phase3.participants=[2;3;4]; react_label="PT-2A-R1"; transition_idx=3; new_objs=[]; term_objs=[]; start_time=0; end_time=1};
            {Phase3.participants=[1;3;4]; react_label="PT-2A-R1"; transition_idx=1; new_objs=[]; term_objs=[]; start_time=0; end_time=1}
        ] in
    assert_equal
        ~msg:"There should be no unused walk element as the result"
        ~cmp:_compare_ewalks
        ~printer:(fun ew -> "walk of length:"^(string_of_int (List.length ew)))
        expected_unused_walk
        unused_ewalk;
    assert_equal
        ~msg:"Result state is not equal to expected"
        ~cmp:(fun s1 s2-> (Bigraph.Big.equal s1.Tracking_bigraph.TTS.bigraph s2.Tracking_bigraph.TTS.bigraph) && s1.index = s2.index )
        expected_result_state
        result_state.state;
    assert_equal
        ~msg:"Result UI on state's nodes mapping is not equal to expected"
        ~printer:Ssp_verification.Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_state_mapping
        result_state.ui_map;
    assert_equal
        ~msg:"Result SAT configuration is not equal to expected"
        ~printer:Ssp.Template_state.to_stirng
        expected_result_sat
        result_state.sat_config;
    assert_equal
        ~msg:"Result state's time moment is not equal to expected"
        constructed_time_moment
        result_state.time;
    assert_equal
        ~msg:"Result time infos are not equal to expected"
        ~printer:_tinfo_list2string
        expected_time_infos
        result_time_infos
let test_perform_phase_5 _ =
    let previous_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_two_agents_previous_state;index=0}
    and previous_state_mapping = [(1,0);(2,1);(3,2);(4,3)] |> Ui.make_map_of_list 
    and previous_sat_config = [|(1,1);(2,0)|]
    and ewalk = Phase3_tests_data.phase_test_two_agents_ewalk_2 
    and constructed_time_moment = 1
    and num_of_agents = 2
    and all_states = Phase3_tests_data.phase_test_two_agents_all_states
    and all_trans_by_idx = Phase3_tests_data.phase_test_two_agents_all_trans_by_idx
    and all_trans_by_keys = Phase3_tests_data.phase_test_two_agents_all_trans_by_keys () in
    let state_at_previous_moment = {Phase3.state=previous_state;ui_map=previous_state_mapping;sat_config=previous_sat_config;time=constructed_time_moment-1} in
    let result_state,unused_ewalk,result_time_infos = Phase3.perform_phase
                    state_at_previous_moment
                    ewalk
                    ~constructed_time_moment
                    ~num_of_agents
                    all_states
                    all_trans_by_idx
                    all_trans_by_keys 
    and expected_unused_walk = 
        [{
            Phase3.trans_fun=Phase3_tests_data.phase_test_two_agents_trans_fun_1;
            ui2redex=[(1,0);(3,1);(4,2)]|> Ui.make_map_of_list;
            ui2state=[(1,0);(2,1);(3,2);(4,3)]|>Ui.make_map_of_list;
            first_new_ui=5;
            time_change=((Phase3_tests_data.IntSet.singleton 1),1)
        }]
    and expected_result_state = {Tracking_bigraph.TTS.bigraph=Phase3_tests_data.phase_test_two_agents_middle_state;Tracking_bigraph.TTS.index=1}
    and expected_state_mapping = [(1,1);(2,0);(3,2);(4,3)] |> Ui.make_map_of_list
    and expected_result_sat = [|(1,1);(2,1)|] 
    and expected_time_infos = [{Phase3.participants=[2;3;4]; react_label="PT-2A-R1"; transition_idx=3; new_objs=[]; term_objs=[]; start_time=0; end_time=1}] in
    assert_equal
        ~msg:"There should be one unused walk element as the result"
        ~cmp:_compare_ewalks
        ~printer:(fun ew -> "walk of length:"^(string_of_int (List.length ew)))
        expected_unused_walk
        unused_ewalk;
    assert_equal
        ~msg:"Result state is not equal to expected"
        ~cmp:(fun s1 s2-> (Bigraph.Big.equal s1.Tracking_bigraph.TTS.bigraph s2.Tracking_bigraph.TTS.bigraph) && s1.index = s2.index )
        ~printer:(fun s -> "State with idx:"^(string_of_int s.index))
        expected_result_state
        result_state.state;
    assert_equal
        ~msg:"Result UI on state's nodes mapping is not equal to expected"
        ~printer:Ssp_verification.Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_state_mapping
        result_state.ui_map;
    assert_equal
        ~msg:"Result SAT configuration is not equal to expected"
        ~printer:Ssp.Template_state.to_stirng
        expected_result_sat
        result_state.sat_config;
    assert_equal
        ~msg:"Result state's time moment is not equal to expected"
        constructed_time_moment
        result_state.time;
    assert_equal
        ~msg:"Result time infos are not equal to expected"
        ~printer:_tinfo_list2string
        expected_time_infos
        result_time_infos
let suite =
    "Phase 3" >::: [
        "Extracting time info test 1 - no new or deleted objects">:: test_extract_time_info_1;
        "Extracting time info test 2 - new obj, no deleted objects">:: test_extract_time_info_2;
        "Extracting time info test 3 - no new, deleted objects">:: test_extract_time_info_3;
        "Extracting time info test 4 - new and deleted objects">:: test_extract_time_info_4;
        "Performing phase 3 test 1 - one agent - walk with one element - applicable">:: test_perform_phase_1;
        "Performing phase 3 test 2 - one agent - walk with one element - not applicable because SAT config already sets the agent in a future moment">:: test_perform_phase_2;
        "Performing phase 3 test 3 - two agents - walk with two elements - both are applicable">:: test_perform_phase_3;
        "Performing phase 3 test 4 - two agents - walk with two elements - both are applicable - different walk">:: test_perform_phase_4;
        "Performing phase 3 test 5 - two agents - walk with two elements - only one is applicable because SAT config already sets one of the agents in a future moment">:: test_perform_phase_5;
    ]

let () =
  run_test_tt_main suite