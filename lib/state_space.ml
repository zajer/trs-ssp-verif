type t = int array
type trans_fun = {func:t->t; transition_idx:int}
type walk = trans_fun list
module IntSet = Set.Make(Int)
let update_agents_times state (agents_involved,time_shift) = 
    Array.map (fun (aid,t)-> if IntSet.mem aid agents_involved then aid,t+time_shift else aid,t) state
let agents_in_future state current_moment = 
    Array.fold_left (fun result (aid,t) -> if t > current_moment then IntSet.add aid result else result) IntSet.empty state