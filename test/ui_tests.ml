open OUnit2
open Ssp_verification
let test_transform_codom_1 _ =
  let transformed = Ui.make_map_of_list [(7,3);(21,9);(777,21)]
  and transformation = Ui.make_map_of_list [(3,0);(9,1);(21,2)] in
  let result = Ui.transform_codom false ~transformed_mapping:transformed ~codom_mapping:transformation
  and expected_result = Ui.make_map_of_list [(7,0);(21,1);(777,2)] in
  assert_equal
    ~msg:"Result mapping is not equal to expected"
    ~printer:(fun s -> let set_as_str = Ui.UIMappingsSet.elements s |> List.map (fun (ui,v)-> "("^(string_of_int ui)^","^(string_of_int v)^")" ) |> String.concat ";" in "{"^set_as_str^"}" )
    ~cmp:Ui.UIMappingsSet.equal
    expected_result
    result
let test_transform_codom_2 _ =
  let transformed = Ui.make_map_of_list [(7,3);(21,9);(777,21)]
  and transformation = Ui.make_map_of_list [(3,0);(9,1)] in
  let result = Ui.transform_codom true ~transformed_mapping:transformed ~codom_mapping:transformation
  and expected_result = Ui.make_map_of_list [(7,0);(21,1);(777,21)] in
  assert_equal
    ~msg:"Result mapping is not equal to expected"
    ~printer:(fun s -> let set_as_str = Ui.UIMappingsSet.elements s |> List.map (fun (ui,v)-> "("^(string_of_int ui)^","^(string_of_int v)^")" ) |> String.concat ";" in "{"^set_as_str^"}" )
    ~cmp:Ui.UIMappingsSet.equal
    expected_result
    result
let test_transform_codom_3 _ =
  let transformed = Ui.make_map_of_list [(7,3);(21,9);(777,21)]
  and transformation = Ui.make_map_of_list [(3,0);(9,1)] in
  let result = Ui.transform_codom false ~transformed_mapping:transformed ~codom_mapping:transformation
  and expected_result = Ui.make_map_of_list [(7,0);(21,1)] in
  assert_equal
    ~msg:"Result mapping is not equal to expected"
    ~printer:(fun s -> let set_as_str = Ui.UIMappingsSet.elements s |> List.map (fun (ui,v)-> "("^(string_of_int ui)^","^(string_of_int v)^")" ) |> String.concat ";" in "{"^set_as_str^"}" )
    ~cmp:Ui.UIMappingsSet.equal
    expected_result
    result
let test_if_is_subset_1 _ = 
  let map1 = Ui.make_map_of_list [(0,0);(1,1);(2,2)]
  and map2 = Ui.make_map_of_list [(0,0);(1,1);(2,2)] in
  let result = Ui.is_subset ~target:map1 ~subset:map2
  and expected_result = true in
  assert_equal
    ~msg:"Provided map should be considered a subset of the target map"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let test_if_is_subset_2 _ = 
  let map1 = Ui.make_map_of_list [(0,0);(1,1);(2,2)]
  and map2 = Ui.make_map_of_list [(0,0);(1,1)] in
  let result = Ui.is_subset ~target:map1 ~subset:map2
  and expected_result = true in
  assert_equal
    ~msg:"Provided map should be considered a subset of the target map"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let test_if_is_subset_3 _ = 
  let map1 = Ui.make_map_of_list [(0,0);(1,1);(2,2)]
  and map2 = Ui.make_map_of_list [(0,0);(1,1);(2,3)] in
  let result = Ui.is_subset ~target:map1 ~subset:map2
  and expected_result = false in
  assert_equal
    ~msg:"Provided map should not be considered a subset of the target map"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let suite =
    "Unique Identifiers" >::: [
        "Transform codom test 1">:: test_transform_codom_1;
        "Transform codom test 2">:: test_transform_codom_2;
        "Transform codom test 3">:: test_transform_codom_3;
        "Is subset test 1">:: test_if_is_subset_1;
        "Is subset test 2">:: test_if_is_subset_2;
        "Is subset test 3">:: test_if_is_subset_3;
    ]

let () =
  run_test_tt_main suite