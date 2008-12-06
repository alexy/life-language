let tuple = (1, Some 2, Some 3, Some 4)
let list = Array.to_list (Obj.magic tuple)
(Obj.magic (List.tl list) : int option list)
