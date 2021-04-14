val gen_ui2redex_map : Bigraph.Iso.t -> Ui.map -> Ui.map
val transform_ui_map : Tracking_bigraph.TTS.trans_exported -> Ui.map -> int -> Phase4.states_mapped_by_idx -> Ui.map * Ui.map * int
val extract_time_change : State_space.sat -> Ssp.State.trans_fun_raw -> Common.IntSet.t * int
val perform_phase : Ssp.State.trans_fun_raw list -> Ui.map -> num_of_agents:int -> first_new_ui:int -> Phase4.states_mapped_by_idx -> Phase3.trans_mapped_by_idx -> Phase3.extended_walk