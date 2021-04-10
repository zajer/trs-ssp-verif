open OUnit2
open Ssp_verification
let test_is_update_with_trans_possible_1 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(777,0);(7,1)]
  and transition = 
    {
        Tracking_bigraph.TTS.in_state_idx=(-1);
        out_state_idx=(-1);
        react_label="my react";
        participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
        residue=Bigraph.Fun.empty;
        actual_out_state=Bigraph.Big.one
    }
  in
  let result = Phase4.is_update_with_trans_possible ~ui2state_map transition ~ui2par_map
  and expected_result = true in
  assert_equal
    ~msg:"Result not equal to expected"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let test_is_update_with_trans_possible_2 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(777,1);(7,0)]
  and transition = 
    {
        Tracking_bigraph.TTS.in_state_idx=(-1);
        out_state_idx=(-1);
        react_label="my react";
        participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
        residue=Bigraph.Fun.empty;
        actual_out_state=Bigraph.Big.one
    }
  in
  let result = Phase4.is_update_with_trans_possible ~ui2state_map transition ~ui2par_map
  and expected_result = false in
  assert_equal
    ~msg:"Result not equal to expected"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let test_is_update_with_trans_possible_3 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(5,0);(7,1)]
  and transition = 
    {
        Tracking_bigraph.TTS.in_state_idx=(-1);
        out_state_idx=(-1);
        react_label="my react";
        participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
        residue=Bigraph.Fun.empty;
        actual_out_state=Bigraph.Big.one
    }
  in
  let result = Phase4.is_update_with_trans_possible ~ui2state_map transition ~ui2par_map
  and expected_result = false in
  assert_equal
    ~msg:"Result not equal to expected"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let test_is_update_possible_1 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(777,0);(7,1)] 
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,1);(1,2)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_1 = 
    {
      Tracking_bigraph.TTS.in_state_idx=(-1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,1);(1,2)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_2 =
    {
      Tracking_bigraph.TTS.in_state_idx=(2);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_3 =
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react2";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_4 = 
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and state = {Tracking_bigraph.TTS.bigraph=Bigraph.Big.one;index=1} in
  let known_transitions = [known_transition_1;known_transition_2;known_transition_3;known_transition_4] in
  let known_trans_dict = 
    Seq.unfold 
      (
        fun list -> 
          match list with 
          | [] -> None
          | h::t -> 
            let dict_elem_key = h.Tracking_bigraph.TTS.in_state_idx,h.react_label
            and dict_elem_val = h in
            Some ((dict_elem_key,dict_elem_val),t)
      ) 
      known_transitions |> Hashtbl.of_seq in
  let result = Phase4.is_update_possible state ~ui2state_map ~ui2par_map applied_transition known_trans_dict
  and expected_result = true, Some known_transition_4 in
  assert_equal
    ~msg:"Result of is_update_possible is not equal to expected"
    expected_result
    result
let test_is_update_possible_2 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(777,0);(7,1)] 
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,1);(1,2)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_1 = 
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,1);(1,2)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_2 =
    {
      Tracking_bigraph.TTS.in_state_idx=(2);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_3 =
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react2";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and state = {Tracking_bigraph.TTS.bigraph=Bigraph.Big.one;index=1} in
  let known_transitions = [known_transition_1;known_transition_2;known_transition_3] in
  let known_trans_dict = 
    Seq.unfold 
      (
        fun list -> 
          match list with 
          | [] -> None
          | h::t -> 
            let dict_elem_key = h.Tracking_bigraph.TTS.in_state_idx,h.react_label
            and dict_elem_val = h in
            Some ((dict_elem_key,dict_elem_val),t)
      ) 
      known_transitions |> Hashtbl.of_seq in
  let result = Phase4.is_update_possible state ~ui2state_map ~ui2par_map applied_transition known_trans_dict
  and expected_result = false, None in
  assert_equal
    ~msg:"Result of is_update_possible is not equal to expected"
    expected_result
    result
let test_update_1 _ = 
  let output_state = Phase4_tests_data.update_test_1_state |> Bigraph.Big.of_string 
  and ui_map = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=(-1);
      out_state_idx=(777);
      react_label="my react";
      participants=Bigraph.Iso.empty;
      residue=Bigraph.Fun.of_list [(0,2);(1,0);(2,1)];
      actual_out_state=Bigraph.Big.one
    }
  and first_new_ui = 3 in
  let know_states = [(777,output_state)] |> List.to_seq |> Hashtbl.of_seq in
  let result_semi_state,result_new_ui = Phase4.update ui_map applied_transition know_states first_new_ui
  and expected_out_state = {Tracking_bigraph.TTS.bigraph=output_state;index=applied_transition.out_state_idx}
  and expected_ui_map = [(2,0);(0,1);(1,2)] |> Ui.make_map_of_list
  and expected_new_ui_val = 3 in
  assert_equal 
    ~msg:"Result state is not equal to expected"
    ~cmp:(fun s1 s2 -> s1.Tracking_bigraph.TTS.index = s2.index && Bigraph.Big.equal s1.bigraph s2.bigraph)
    expected_out_state
    result_semi_state.state;
  assert_equal
    ~msg:"Result UI map not equal to expected"
    ~printer:Ui.map_to_string
    ~cmp:Ui.are_equal
    expected_ui_map
    result_semi_state.ui_map;
  assert_equal
    ~msg:"Result first new UI value not equal to expected"
    ~printer:(fun v -> string_of_int v)
    expected_new_ui_val
    result_new_ui
let test_update_2 _ = 
  let output_state = Phase4_tests_data.update_test_2_state |> Bigraph.Big.of_string 
  and ui_map = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=(-1);
      out_state_idx=(777);
      react_label="my react";
      participants=Bigraph.Iso.empty;
      residue=Bigraph.Fun.of_list [(1,0);(0,2)];
      actual_out_state=Bigraph.Big.one
    }
  and first_new_ui = 3 in
  let know_states = [(777,output_state)] |> List.to_seq |> Hashtbl.of_seq in
  let result_semi_state,result_new_ui = Phase4.update ui_map applied_transition know_states first_new_ui
  and expected_out_state = {Tracking_bigraph.TTS.bigraph=output_state;index=applied_transition.out_state_idx}
  and expected_ui_map = [(2,0);(0,1)] |> Ui.make_map_of_list
  and expected_new_ui_val = 3 in
  assert_equal 
    ~msg:"Result state is not equal to expected"
    ~cmp:(fun s1 s2 -> s1.Tracking_bigraph.TTS.index = s2.index && Bigraph.Big.equal s1.bigraph s2.bigraph)
    expected_out_state
    result_semi_state.state;
  assert_equal
    ~msg:"Result UI map not equal to expected"
    ~printer:Ui.map_to_string
    ~cmp:Ui.are_equal
    expected_ui_map
    result_semi_state.ui_map;
  assert_equal
    ~msg:"Result first new UI value not equal to expected"
    ~printer:(fun v -> string_of_int v)
    expected_new_ui_val
    result_new_ui
let test_update_3 _ = 
  let output_state = Phase4_tests_data.update_test_3_state |> Bigraph.Big.of_string 
  and ui_map = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=(-1);
      out_state_idx=(777);
      react_label="my react";
      participants=Bigraph.Iso.empty;
      residue=Bigraph.Fun.of_list [(0,2);(1,0);(2,1)];
      actual_out_state=Bigraph.Big.one
    }
  and first_new_ui = 3 in
  let know_states = [(777,output_state)] |> List.to_seq |> Hashtbl.of_seq in
  let result_semi_state,result_new_ui = Phase4.update ui_map applied_transition know_states first_new_ui
  and expected_out_state = {Tracking_bigraph.TTS.bigraph=output_state;index=applied_transition.out_state_idx}
  and expected_ui_map = [(2,0);(0,1);(1,2);(3,3)] |> Ui.make_map_of_list
  and expected_new_ui_val = 4 in
  assert_equal 
    ~msg:"Result state is not equal to expected"
    ~cmp:(fun s1 s2 -> s1.Tracking_bigraph.TTS.index = s2.index && Bigraph.Big.equal s1.bigraph s2.bigraph)
    expected_out_state
    result_semi_state.state;
  assert_equal
    ~msg:"Result UI map not equal to expected"
    ~printer:Ui.map_to_string
    ~cmp:Ui.are_equal
    expected_ui_map
    result_semi_state.ui_map;
  assert_equal
    ~msg:"Result first new UI value not equal to expected"
    ~printer:(fun v -> string_of_int v)
    expected_new_ui_val
    result_new_ui
let test_update_4 _ = 
  let output_state = Phase4_tests_data.update_test_4_state |> Bigraph.Big.of_string 
  and ui_map = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=(-1);
      out_state_idx=(777);
      react_label="my react";
      participants=Bigraph.Iso.empty;
      residue=Bigraph.Fun.of_list [(0,2);(1,0)];
      actual_out_state=Bigraph.Big.one
    }
  and first_new_ui = 3 in
  let know_states = [(777,output_state)] |> List.to_seq |> Hashtbl.of_seq in
  let result_semi_state,result_new_ui = Phase4.update ui_map applied_transition know_states first_new_ui
  and expected_out_state = {Tracking_bigraph.TTS.bigraph=output_state;index=applied_transition.out_state_idx}
  and expected_ui_map = [(2,0);(0,1);(3,2)] |> Ui.make_map_of_list
  and expected_new_ui_val = 4 in
  assert_equal 
    ~msg:"Result state is not equal to expected"
    ~cmp:(fun s1 s2 -> s1.Tracking_bigraph.TTS.index = s2.index && Bigraph.Big.equal s1.bigraph s2.bigraph)
    expected_out_state
    result_semi_state.state;
  assert_equal
    ~msg:"Result UI map not equal to expected"
    ~printer:Ui.map_to_string
    ~cmp:Ui.are_equal
    expected_ui_map
    result_semi_state.ui_map;
  assert_equal
    ~msg:"Result first new UI value not equal to expected"
    ~printer:(fun v -> string_of_int v)
    expected_new_ui_val
    result_new_ui
let test_perform_phase_1 () = 
  let previous_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_invalid_previous_state |> Bigraph.Big.of_string; index=(-1)}
  and current_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string; index=2} 
  and previous_state_mapping = [(0,0)] |> Ui.make_map_of_list
  and current_state_mapping = Ui.empty_map (* not used in this test *)
  and mapping_on_redex = [(0,0);(1,1)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=1; 
      out_state_idx=1;
      react_label="my react";
      participants=[(0,0);(1,1)] |> Bigraph.Iso.of_list;
      residue=Bigraph.Fun.of_list [(0,0);(1,1);(2,2)];
      actual_out_state=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string
    }
  and first_new_ui = 2 in
  let known_transitions = [(applied_transition.in_state_idx,applied_transition.react_label),applied_transition] |> List.to_seq |> Hashtbl.of_seq 
  and known_states = [(-1,Phase4_tests_data.perform_phase_test_invalid_previous_state |> Bigraph.Big.of_string);(1,Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string);(-2,Phase4_tests_data.perform_phase_test_invalid_current_state |> Bigraph.Big.of_string);(2,Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string)] |> List.to_seq |> Hashtbl.of_seq 
  and previous_constructed_state = {Phase4.state = previous_state;ui_map=previous_state_mapping}
  and current_constructed_state = {Phase4.state = current_state;ui_map=current_state_mapping} 
  and mapped_trans = {Phase4.transition=applied_transition;ui_map_on_redex=mapping_on_redex} in
  Phase4.perform_phase
    ~previous_state:previous_constructed_state
    ~current_state:current_constructed_state
    mapped_trans
    first_new_ui
    known_states
    known_transitions 
let test_perform_phase_1_wrapped _ =
  assert_raises
    ~msg:"Previous state should not be updatable"
    (Phase4.Not_updateable "Previous state is not updateable") test_perform_phase_1
let test_perform_phase_2 () = 
  let previous_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string; index=1}
  and current_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string; index=2} 
  and previous_state_mapping = [(0,1);(1,0)] |> Ui.make_map_of_list
  and current_state_mapping = Ui.empty_map (* not used in this test *)
  and mapping_on_redex = [(0,0);(1,1)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=1; 
      out_state_idx=1;
      react_label="my react";
      participants=[(0,0);(1,1)] |> Bigraph.Iso.of_list;
      residue=Bigraph.Fun.of_list [(0,0);(1,1)];
      actual_out_state=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string
    } 
  and first_new_ui = 2 in
  let known_transitions = [(applied_transition.in_state_idx,applied_transition.react_label),applied_transition] |> List.to_seq |> Hashtbl.of_seq 
  and known_states = [(-1,Phase4_tests_data.perform_phase_test_invalid_previous_state |> Bigraph.Big.of_string);(1,Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string);(-2,Phase4_tests_data.perform_phase_test_invalid_current_state |> Bigraph.Big.of_string);(2,Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string)] |> List.to_seq |> Hashtbl.of_seq
  and previous_constructed_state = {Phase4.state = previous_state;ui_map=previous_state_mapping}
  and current_constructed_state = {Phase4.state = current_state;ui_map=current_state_mapping} 
  and mapped_trans = {Phase4.transition=applied_transition;ui_map_on_redex=mapping_on_redex} in
  Phase4.perform_phase
    ~previous_state:previous_constructed_state
    ~current_state:current_constructed_state
    mapped_trans
    first_new_ui
    known_states
    known_transitions
let test_perform_phase_2_wrapped _ =
  assert_raises
    ~msg:"Previous state should not be updatable"
    (Phase4.Not_updateable "Previous state is not updateable") test_perform_phase_2
let test_perform_phase_3 () = 
  let previous_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string; index=1}
  and current_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_invalid_current_state |> Bigraph.Big.of_string; index=(-2)} 
  and previous_state_mapping = [(0,0);(1,1)] |> Ui.make_map_of_list
  and current_state_mapping = [(0,0);(2,1);(3,2)] |> Ui.make_map_of_list
  and mapping_on_redex = [(0,0);(1,1)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=1;
      out_state_idx=1;
      react_label="my react";
      participants=[(0,0);(1,1)] |> Bigraph.Iso.of_list;
      residue=Bigraph.Fun.of_list [(0,0);(1,1)];
      actual_out_state=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string
    }
  and transition_for_current_state =
    {
      Tracking_bigraph.TTS.in_state_idx=2;
      out_state_idx=2;
      react_label="my react";
      participants=[(0,0);(1,1)] |> Bigraph.Iso.of_list;
      residue=Bigraph.Fun.of_list [(0,0);(1,1);(2,2)];
      actual_out_state=Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string
    }
  and first_new_ui = 4 in
  let known_transitions = [(applied_transition.in_state_idx,applied_transition.react_label),applied_transition;(transition_for_current_state.in_state_idx,transition_for_current_state.react_label),transition_for_current_state] |> List.to_seq |> Hashtbl.of_seq 
  and known_states = [(-1,Phase4_tests_data.perform_phase_test_invalid_previous_state |> Bigraph.Big.of_string);(1,Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string);(-2,Phase4_tests_data.perform_phase_test_invalid_current_state |> Bigraph.Big.of_string);(2,Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string)] |> List.to_seq |> Hashtbl.of_seq 
  and previous_constructed_state = {Phase4.state = previous_state;ui_map=previous_state_mapping}
  and current_constructed_state = {Phase4.state = current_state;ui_map=current_state_mapping} 
  and mapped_trans = {Phase4.transition=applied_transition;ui_map_on_redex=mapping_on_redex} in
  Phase4.perform_phase
    ~previous_state:previous_constructed_state
    ~current_state:current_constructed_state
    mapped_trans
    first_new_ui
    known_states
    known_transitions
let test_perform_phase_3_wrapped _ =
  assert_raises
    ~msg:"Current state should not be updatable"
    (Phase4.Not_updateable "Currently constructed state is not updateable") test_perform_phase_3
let test_perform_phase_4 () = 
  let previous_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string; index=1}
  and current_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string; index=2} 
  and previous_state_mapping = [(0,0);(1,1)] |> Ui.make_map_of_list
  and current_state_mapping = [(0,0);(1,2);(2,1)] |> Ui.make_map_of_list
  and mapping_on_redex = [(0,0);(1,1)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=1;
      out_state_idx=1;
      react_label="my react";
      participants=[(0,0);(1,1)] |> Bigraph.Iso.of_list;
      residue=Bigraph.Fun.of_list [(0,0);(1,1)];
      actual_out_state=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string
    }
  and transition_for_current_state =
  {
    Tracking_bigraph.TTS.in_state_idx=2;
    out_state_idx=2;
    react_label="my react";
    participants=[(0,0);(1,1)] |> Bigraph.Iso.of_list;
    residue=Bigraph.Fun.of_list [(0,0);(1,1);(2,2)];
    actual_out_state=Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string
  } 
  and first_new_ui = 3 in
  let known_transitions = [(applied_transition.in_state_idx,applied_transition.react_label),applied_transition;(transition_for_current_state.in_state_idx,transition_for_current_state.react_label),transition_for_current_state] |> List.to_seq |> Hashtbl.of_seq 
  and known_states = [(-1,Phase4_tests_data.perform_phase_test_invalid_previous_state |> Bigraph.Big.of_string);(1,Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string);(-2,Phase4_tests_data.perform_phase_test_invalid_current_state |> Bigraph.Big.of_string);(2,Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string)] |> List.to_seq |> Hashtbl.of_seq 
  and previous_constructed_state = {Phase4.state = previous_state;ui_map=previous_state_mapping}
  and current_constructed_state = {Phase4.state = current_state;ui_map=current_state_mapping} 
  and mapped_trans = {Phase4.transition=applied_transition;ui_map_on_redex=mapping_on_redex} in
  Phase4.perform_phase
    ~previous_state:previous_constructed_state
    ~current_state:current_constructed_state
    mapped_trans
    first_new_ui
    known_states
    known_transitions
let test_perform_phase_4_wrapped _ =
  assert_raises
    ~msg:"Current state should not be updatable"
    (Phase4.Not_updateable "Currently constructed state is not updateable") test_perform_phase_4
let test_perform_phase_5 _ = 
  let previous_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string; index=1}
  and current_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string; index=2} 
  and previous_state_mapping = [(0,0);(1,1)] |> Ui.make_map_of_list
  and current_state_mapping = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
  and mapping_on_redex = [(0,0);(1,1)] |> Ui.make_map_of_list
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=1;
      out_state_idx=1;
      react_label="my react";
      participants=[(0,0);(1,1)] |> Bigraph.Iso.of_list;
      residue=Bigraph.Fun.of_list [(0,0);(1,1)];
      actual_out_state=Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string
    }
  and transition_for_current_state =
  {
    Tracking_bigraph.TTS.in_state_idx=2;
    out_state_idx=2;
    react_label="my react";
    participants=[(0,0);(1,1)] |> Bigraph.Iso.of_list;
    residue=Bigraph.Fun.of_list [(0,0);(1,1);(2,2)];
    actual_out_state=Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string
  } 
  and first_new_ui = 3 in
  let known_transitions = [(applied_transition.in_state_idx,applied_transition.react_label),applied_transition;(transition_for_current_state.in_state_idx,transition_for_current_state.react_label),transition_for_current_state] |> List.to_seq |> Hashtbl.of_seq 
  and known_states = [(-1,Phase4_tests_data.perform_phase_test_invalid_previous_state |> Bigraph.Big.of_string);(1,Phase4_tests_data.perform_phase_test_correct_previous_state |> Bigraph.Big.of_string);(-2,Phase4_tests_data.perform_phase_test_invalid_current_state |> Bigraph.Big.of_string);(2,Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string)] |> List.to_seq |> Hashtbl.of_seq 
  and previous_constructed_state = {Phase4.state = previous_state;ui_map=previous_state_mapping}
  and current_constructed_state = {Phase4.state = current_state;ui_map=current_state_mapping} 
  and mapped_trans = {Phase4.transition=applied_transition;ui_map_on_redex=mapping_on_redex} in
  let result_state,result_first_new_ui = 
    Phase4.perform_phase
      ~previous_state:previous_constructed_state
      ~current_state:current_constructed_state
      mapped_trans
      first_new_ui
      known_states
      known_transitions 
  and expected_result_state = {Tracking_bigraph.TTS.bigraph=Phase4_tests_data.perform_phase_test_correct_current_state |> Bigraph.Big.of_string; index=2} 
  and expected_result_mapping = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
  and expected_first_new_ui = 3 in
  assert_equal
    ~msg:"Result state is not equal to expected"
    ~cmp:(fun s1 s2-> Bigraph.Big.equal s1.Tracking_bigraph.TTS.bigraph s2.bigraph)
    expected_result_state
    result_state.state;
  assert_equal
    ~msg:"Result mapping is not equal to expected"
    ~printer:Ui.map_to_string
    ~cmp:Ui.are_equal
    expected_result_mapping
    result_state.ui_map;
  assert_equal
    ~msg:"Result first new UI not equal to expected"
    ~printer:(fun i -> string_of_int i)
    expected_first_new_ui
    result_first_new_ui


let failed_test _ =
  assert_equal true false
let suite =
    "Phase 4" >::: [
        "Is update with trans possible test 1">:: test_is_update_with_trans_possible_1;
        "Is update with trans possible test 2">:: test_is_update_with_trans_possible_2;
        "Is update with trans possible test 3">:: test_is_update_with_trans_possible_3;
        "Is update possible test 1">:: test_is_update_possible_1;
        "Is update possible test 2">:: test_is_update_possible_2;
        "Update test 1 - no new or deleted objects">:: test_update_1;
        "Update test 2 - no new objects, one deleted">:: test_update_2;
        "Update test 3 - new object, no deleted">:: test_update_3;
        "Update test 4 - new object, one deleted">:: test_update_4;
        "Update test 4 - new object, one deleted">:: test_update_4;
        "Perform phase test 1 - previous state is unupadteable because of lack applicable transition">:: test_perform_phase_1_wrapped;
        "Perform phase test 2 - previous state is unupadteable because of incorrect UI mapping ">:: test_perform_phase_2_wrapped;
        "Perform phase test 3 - previous state is upadteable but current isn't because of lack of applicable transition">:: test_perform_phase_3_wrapped;
        "Perform phase test 4 - previous state is upadteable but current isn't because of incorrect UI mapping">:: test_perform_phase_4_wrapped;
        "Perform phase test 5 - both previous state and current state are upadteable ">:: test_perform_phase_5
    ]

let () =
  run_test_tt_main suite