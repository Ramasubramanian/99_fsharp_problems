namespace fsharpproblems
module onetoten = 
/// My solution to first 10 problems of 99 fsharp problems. Tried to use recursion 
/// and pattern matching as far as possible and avoid in-built List utility functions
/// Link - http://www.fssnip.net/an

    /// 01-find the last element of a list.
    let rec findLast list = 
        match list with
        | [] -> failwith "Empty List"
        | [head] -> head
        | head :: tail -> findLast tail


    let rec findLastButN n list =
        match n with 
        | 0 -> findLast list
        | n when n <= List.length list -> 
            match list with
            | [] -> failwith "Empty List"
            | head :: tail -> if List.length tail = n then head else findLastButN n tail
        | n -> failwith "Less elements in list!"

    /// 02-find last but one element of the list.
    let findLastButOne list = findLastButN 2 list


    /// 03-find the element at specified index.
    let rec elementAt n list =
        match n with
        | n when n <= List.length list -> 
            match list with
            | [] -> failwith "Empty list"
            | head :: tail -> if n = 1 then head else elementAt (n - 1) tail 
        | n -> failwith "Less elements in list!"

    /// 04-find length of a specified list.
    let rec len list =
        match list with
        | [] -> 0
        | [x] -> 1
        | head :: tail -> 1 + (len tail)

    /// 05-reverse a list.
    let reverse list = 
        let rec reverse0 list seed = 
            match list with 
            | [] -> seed
            | head :: tail -> reverse0 tail (head :: seed)
        reverse0 list []

    /// 06-check whether a given list is a palindrome.
    let isPalindrome list = 
        let mid = (len list) / 2
        let rec reverseTake n list seed = 
            match list with 
            | [] -> (seed,[])
            | head :: tail -> if n = 0 then (seed,head :: tail) else reverseTake (n - 1) tail (head :: seed)
        let reversed, original = reverseTake mid list []
        let result = 
            match len reversed with
            | n when n = len original -> reversed = original
            | n ->
                match original with  
                | head :: tail -> reversed = tail
                | _ -> false
        result

    /// 07-Flatten a nested list structure.
    type 'a NestedList = List of 'a NestedList list | Elem of 'a

    let flatten list =
        let rec flatIt acc = function
            | Elem x -> acc @ [x]
            | List l -> List.fold (fun acc e -> flatIt acc e) acc l
        flatIt [] list 

    /// 08-Eliminate consecutive duplicates of list elements.
    let compress list = 
        let start = List.head list
        let rec comp prev list acc =
            match list with 
            | [] -> acc
            | first :: rest when first <> prev -> comp first rest (acc @ [first]) 
            | first :: rest -> comp first rest acc
        comp start (List.tail list) [start]   

    /// 09-Pack consecutive duplicates of list elements into sublists.
    let pack list = 
        let start = List.head list
        let rec pck prev list subacc mainacc =
            match list with 
            | [] -> mainacc @ [subacc]
            | first :: rest when first = prev -> pck first rest (first :: subacc) mainacc  
            | first :: rest -> pck first rest [first] (mainacc @ [subacc]) 
        pck start (List.tail list) [start] []    

    /// 10-Run-length encoding of a list.
    let encode list = 
        pack list |> List.map (fun l -> (List.length l,List.head l)) 

