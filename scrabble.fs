open System
open System.Diagnostics

let letterPoints = function
| 'q' | 'z' -> 10 | 'j' | 'x' -> 8
| 'k' -> 5 | 'f' | 'h' | 'v' | 'w' | 'y' -> 4
| 'b' | 'c' | 'm' | 'p' -> 3 | 'd' | 'g' -> 2
| _ -> 1

let rec canMakeWord = function
| ([],_) -> true | (_,[]) -> false
| (wl::xw,l::xs) when wl=l -> canMakeWord (xw,xs)
| (wl::_ as w,l::xs) when l<wl -> canMakeWord (w,xs)
| _ -> false

let getOrderedLetters = Seq.sort >> List.ofSeq
let scoreWord = Seq.sumBy letterPoints

let findHighestScoreWord wordList letters =
    let orderedLetters = getOrderedLetters letters
    let isValidWord (_,w,_) = (w,orderedLetters) |> canMakeWord
    wordList |> List.find isValidWord |> (fun (_,_,x) -> x)

let N = int(Console.In.ReadLine())
let words = 
    [ for _ in 0 .. N - 1 do
        let word = Console.In.ReadLine()
        if word.Length <= 7 
        then yield (scoreWord word,getOrderedLetters word,word) ] 
    |> List.sortByDescending (fun (x,_,_) -> x)

let LETTERS = Console.In.ReadLine()

let sw = Stopwatch.StartNew()
findHighestScoreWord words LETTERS |> Console.Out.Write
sw.Elapsed |> eprintfn "%A"