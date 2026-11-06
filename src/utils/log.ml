let myfile flag x = if flag then Printf.printf "myfile: %s\n" x
let myfiles flag xs = if flag then Base.List.iter ~f:(myfile true) xs
let str flag x = if flag then Printf.printf "string: %s\n" x
let int flag x = if flag then Printf.printf "int: %d\n" x
let float flag x = if flag then Printf.printf "float: %f\n" x
