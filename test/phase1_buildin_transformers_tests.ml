open OUnit2
open Ssp_verification
let test_pattern_detection_1 _ =
    let pattern_big = "{(0, AT3:0),(1, X:0)}\n0 2 0\n01\n00" |> Bigraph.Big.of_string 
    and state_big = "{(0, AT1:0),(1, AT2:0),(2, AT3:0),(3, X:0)}\n0 4 0\n0001\n0000\n0000\n0000" |> Bigraph.Big.of_string in
    let pattern = {Ssp_bridge.Patterns.bigraph=pattern_big; description="agent in AT3"} 
    and state = {Phase3.state={Tracking_bigraph.TTS.bigraph=state_big;index=3};ui_map=Ui.empty_map;sat_config=[||];time=777} in
    let current_res = {Phase1.is_successful=true;value=[],[];error_message=None} 
    and current_part_res = state,[]
    and transformer = Phase1.BuildinTransformers.disqualify_results_if_pattern_detected [pattern] in
    let result = Phase1.ResultTransformer.apply current_part_res current_res transformer
    and expected_result = current_res in
    assert_equal
        ~msg:"Result after transformation is not equalt to expected"
        expected_result
        result
let test_pattern_detection_2 _ =
    let pattern_big = "{(0, AT3:0),(1, X:0)}\n0 2 0\n01\n00" |> Bigraph.Big.of_string 
    and state_big = "{(0, AT1:0),(1, AT2:0),(2, AT3:0),(3, X:0)}\n0 4 0\n0000\n0000\n0001\n0000" |> Bigraph.Big.of_string in
    let pattern = {Ssp_bridge.Patterns.bigraph=pattern_big; description="agent in AT3"} 
    and state = {Phase3.state={Tracking_bigraph.TTS.bigraph=state_big;index=3};ui_map=Ui.empty_map;sat_config=[||];time=777} in
    let current_res = {Phase1.is_successful=true;value=[],[];error_message=None} 
    and current_part_res = state,[]
    and transformer = Phase1.BuildinTransformers.disqualify_results_if_pattern_detected [pattern] in
    let result = Phase1.ResultTransformer.apply current_part_res current_res transformer
    and expected_result = {Phase1.is_successful=false;value=[],[];error_message=Some "agent in AT3"}  in
    assert_equal
        ~msg:"Result after transformation is not equalt to expected"
        expected_result
        result
let test_pattern_detection_3 _ =
    let pattern_1_big = "{(0, AT3:0),(1, X:0)}\n0 2 0\n01\n00" |> Bigraph.Big.of_string 
    and pattern_2_big = "{(0, AT2:0),(1, X:0)}\n0 2 0\n01\n00" |> Bigraph.Big.of_string 
    and state_big = "{(0, AT1:0),(1, AT2:0),(2, AT3:0),(3, X:0)}\n0 4 0\n0000\n0000\n0001\n0000" |> Bigraph.Big.of_string in
    let pattern_1 = {Ssp_bridge.Patterns.bigraph=pattern_1_big; description="pattern1"} 
    and pattern_2 = {Ssp_bridge.Patterns.bigraph=pattern_2_big; description="pattern2"} 
    and state = {Phase3.state={Tracking_bigraph.TTS.bigraph=state_big;index=3};ui_map=Ui.empty_map;sat_config=[||];time=777} in
    let current_res = {Phase1.is_successful=true;value=[],[];error_message=None} 
    and current_part_res = state,[]
    and transformer_1 = Phase1.BuildinTransformers.disqualify_results_if_pattern_detected [pattern_1] 
    and transformer_2 = Phase1.BuildinTransformers.disqualify_results_if_pattern_detected [pattern_2] in
    let stacked_transformer_1 = Phase1.ResultTransformer.stack transformer_1 transformer_2 
    and stacked_transformer_2 = Phase1.ResultTransformer.stack transformer_2 transformer_1 in
    let result_1 = Phase1.ResultTransformer.apply current_part_res current_res stacked_transformer_1
    and result_2 = Phase1.ResultTransformer.apply current_part_res current_res stacked_transformer_2
    and expected_result = {Phase1.is_successful=false;value=[],[];error_message=Some "pattern1"}  in
    assert_equal
        ~msg:"Result 1 after transformation is not equalt to expected"
        expected_result
        result_1;
    assert_equal
        ~msg:"Result 2 after transformation is not equalt to expected"
        expected_result
        result_2
let suite =
    "Phase 1 result transformers" >::: [
        "Pattern detection test 1 - pattern is not present">:: test_pattern_detection_1;
        "Pattern detection test 2 - pattern is present">:: test_pattern_detection_2;
        "Pattern detection test 3 - stack">:: test_pattern_detection_3;
    ]

let () =
  run_test_tt_main suite