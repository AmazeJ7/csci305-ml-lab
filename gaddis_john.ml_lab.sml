(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Johnny Gaddis
* johnnygaddis777@gmail.com
* cmd /c 'sml < ml_lab.sml' New Jersey Flavor on Windows
***************************************************************)

(* Function to increment an array *)
fun f [] = [] (* Returns an empty list if passed an empty list *)
  | f (x::xs) = (x + 1) :: (f xs); (* Adds one to the head value and then recursively calls the function on the list's tail value *)

(* Datatype to represent sets *)
datatype 'element set = Empty
  | Set of 'element * 'element set;

(* Function to determine if an element is part of a set *)
fun isMember e Empty = false (* If the set is empty return false *)
  | isMember e (Set(item, set)) = (* Grab an item out of the set *)
  if e = item then true (* Compare the items and return true for a match *)
	else isMember e set; (* If not true recursively call the tail of the set *)

(* Function to turn a list into a set *)
fun list2Set [] = Empty (* Return empty for an empty list *)
  | list2Set list = foldr (fn (item, set) => (* For every item in the list *)
  if not (isMember item set) then Set(item, set) (* If the item is not in the set add it to the set *)
  else set) (* If it's a member don't add it *)
  Empty list;

(* Function to take two sets and union the two*)
fun union Empty Empty = Empty (* If both sets are empty return empty*)
  | union set Empty = set (* If either set is empty return the other set*)
  | union Empty set = set
  | union (Set(item, set)) (Set(item2, set2)) = (* Represent the sets and items *)
  if not (isMember item (Set(item2, set2))) then Set(item, (union set (Set(item2, set2))))
  (* If the item from the first set is not a part of the second then add it and recursively call the tail of set *)
  else union set (Set(item2, set2));
  (* Else recursivly call the tail of set *)

(* Function to take two sets and intersect the two*)
fun intersect Empty Empty = Empty (* If both sets are empty return empty*)
  | intersect set Empty = Empty (* If either set is empty return the other set*)
  | intersect Empty set = Empty
  | intersect (Set(item, set)) (Set(item2, set2)) = (* Represent the sets and items *)
  if (isMember item (Set(item2, set2))) then Set(item, (intersect set (Set(item2, set2))))
  (* If the item from the first set is a part of the second then add it and recursively call it *)
  else intersect set (Set(item2, set2));
  (* Else recursivly call the tail of set *)

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
