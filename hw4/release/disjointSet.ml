type element = int
type node = {mutable parent : element;
						 mutable rank : int;
						 mutable size : int}

type universe = node array

let createUniverse (size : int) : universe =
  Array.init size (fun i -> {parent = i; rank = 1; size = 1})

let rec find (s : universe) (e : element) : element =
	let n = Array.get s e in
		if n.parent = e then e
		else
			(n.parent <- (find s n.parent);
			 n.parent)

let union (s : universe) (e1 : element) (e2 : element) : unit =
  let r1 = find s e1 in
  let r2 = find s e2 in
	let n1 = s.(r1) in
	let n2 = s.(r2) in
    if r1 <> r2 then
        if n1.rank < n2.rank then
			    (n1.parent <- e2;
			     n2.size <- n1.size + n2.size)
		    else
			    (n2.parent <- e1;
			     n1.size <- n2.size + n1.size;
			     if n1.rank = n2.rank then
				     n1.rank <- n1.rank + 1)

let size (s : universe) (e : element) : int =
	s.(find s e).size
