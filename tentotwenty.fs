namespace fsharpproblems

open onetoten

type 'a Encoding = Multiple of int * 'a | Single of 'a

module tentotwenty =
    ///11-Modified run-length encoding.
    let modlen list = 
        let gen l =
            match List.length l with
                | 1 -> Single(List.head l)
                | n -> Multiple(n,List.head l)
        pack list |> List.map gen
    ///12-Decode a run-length encoded list.
    let decode list =
        let rec create v n acc =
            match n with
            | 0 -> acc
            | x -> create v (x - 1) (v :: acc)
        let generate = function
            | Single(e) -> [e]
            | Multiple(n,e) -> create e n []
        List.fold (fun acc elem -> acc @ generate elem) [] list
    /// alternative solution
    let decode2 list =
        let generate = function
            | Single(e) -> [e]
            | Multiple(n,e) -> List.replicate n e
        List.fold (fun acc elem -> acc @ generate elem) [] list
    ///13-Run-length encoding of a list (direct solution).
    let rlencode list =
        let create acc count e x = 
            match count with 
                | 1 -> (acc @ [Single(e)],1,x)
                | n -> (acc @ [Multiple(n,e)],1,x)                
        let encode (acc, count, e) elem = 
            match elem with
            | x when x = e -> (acc, count + 1, e)
            | x -> create acc count e x                
        let (acc, count, e) = List.fold encode ([], 1, List.head list) (List.tail list)
        let (result, _, _) = create acc count e e
        result
    ///14-Duplicate the elements of a list.
    let duplicate list =
        List.foldBack (fun elem acc -> elem :: (elem :: acc)) list []
    ///15-Replicate the elements of a list a given number of times.
    let replicate v n =
        let rec repl v n acc =
            match n with
            | 0 -> acc
            | x -> repl v (x - 1) (v :: acc)
        repl v n []
    ///16-Drop every N'th element from a list.
    let dropEvery list n =
        let drop (acc, count) elem =
            match count with
            | x when x = n -> (acc, 1)
            | x -> (acc @ [elem], count + 1)
        let (acc,count) = List.fold drop ([],1) list 
        acc
    ///17-Split a list into two parts; the length of the first part is given.
    let split list n =
        let rec split0 acc list n =
            match list with 
            | [] -> (acc,[])
            | l when n = 0 -> (acc, l)
            | first :: rest -> split0 (acc @ [first]) rest (n - 1) 
        split0 [] list n
    ///18-Extract a slice from a list.
    let slice list m n =
        match n with
        | 0 -> []
        | n ->
            let (temp, _) = split list n
            let (_,result) = split temp (m - 1)
            result 
    ///19-Rotate a list N places to the left.        
    let rotate list n =
        let (a,b) = 
            match n with 
            | 0 -> (list,[])
            | n when n > 0 -> split list n
            | n -> split list (List.length list - (-n))
        b @ a
    ///20-Remove the K'th element from a list.
    let removeAt n list =
        let rec remove count acc rest =
            match rest with
            | [] -> failwith "Empty List!"
            | first :: rest when count = n -> (first, acc @ rest)
            | first :: rest -> remove (count + 1) (acc @ [first]) rest 
        remove 0 [] list  