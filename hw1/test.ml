let buckets equiv lst =
  let rec helperf alst blst =
    match alst with
      | [] -> List.rev blst
      | h::t -> helperf t 
	  (let rec bucket clst rlst =
	     match clst with
	       | [] -> ([h]::rlst)
	       | h2::t2 -> if equiv h (List.hd h2) then ((h::h2)::rlst)
		 else bucket t2 (blst)
	   in bucket blst [])
in helperf lst []

