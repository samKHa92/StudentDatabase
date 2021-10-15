type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list;
}

type database = student list

let insert s db = s::db

let rec find_by_id id db = match db with [] -> []
  | x::xs -> if x.id = id 
  then [x] 
  else find_by_id id xs

let rec find_by_last_name name db = match db with [] -> []
  | x::xs -> if x.last_name = name
    then x::find_by_last_name name xs
    else find_by_last_name name xs

let rec remove_by_id id db = match db with [] -> []
  | x::xs -> if x.id = id then xs else x::remove_by_id id xs


let rec count_in_semester semester db = match db with [] -> 0
  | x::xs -> if x.semester = semester then 1 + count_in_semester semester xs
  else count_in_semester semester xs


let student_avg_grade id db = 
  let rec list_avg sum n l = match l with [] -> sum /. n
    | (_,g)::xs -> list_avg (sum+.g) (n +. 1.0) xs 

  in
  match find_by_id id db with
  | [{grades = []}] -> 0.0
  | [x] -> list_avg 0.0 0.0 x.grades
  | _ -> 0.0



let course_avg_grade course db =
  let rec iterate_grades (sum,n) l = match l with [] -> (sum,n)
  | (c,g) :: xs -> if c = course then iterate_grades(sum +. g, n +. 1.0) xs else iterate_grades (sum,n) xs
  in
    let rec iterate_students (sum,n) l = match l with [] -> (sum,n)
    | x :: xs -> iterate_students(iterate_grades (sum,n) x.grades) xs
    in
      let sum,n = iterate_students(0.0, 0.0) db in
      if n = 0.0 then 0.0 else sum /. n



let rec interleave3 list1 list2 list3 = 
  let rec interleave2 list1 list2 =
      match list1 with [] -> list2
      | x::xs -> x:: interleave2 list2 xs
        in
        match list1 with [] -> interleave2 list2 list3
        | x::xs -> x::interleave3 list2 list3 xs





