type sat = Ssp.Template_state.t
type trans_fun = Ssp.Template_state.trans_fun
type walk = trans_fun list
let init_sat noa = 
    Array.init noa (fun i -> (i+1),0 )
let update_sat state (agents_involved,time_shift) = 
    Array.map (fun (aid,t)-> if Common.IntSet.mem aid agents_involved then aid,t+time_shift else aid,t) state
let elements_in_future state current_moment = 
    Array.fold_left (fun result (aid,t) -> if t > current_moment then Common.IntSet.add aid result else result) Common.IntSet.empty state