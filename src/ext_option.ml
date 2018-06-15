let with_default default = function
  | Some a -> a
  | None -> default