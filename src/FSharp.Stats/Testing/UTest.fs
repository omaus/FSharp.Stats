namespace FSharp.Stats.Testing

module UTest =

    open FSharp.Stats

    let inline private computeUtest (seq1: seq<'T>) (seq2: seq<'T>) =
        let sortedMerge = 
            (seq1 |> Seq.map (fun v -> float v,0), seq2 |> Seq.map (fun v -> float v,1)) // 0 = first group; 1 = second group
            ||> Seq.append
            |> Seq.sortByDescending (fun (v,groupIndex) -> v)
            |> Array.ofSeq
        // let abundance = // method for equal ranks instead of mean ranks when identical values occur.
        //     sortedMerge
        //     |> Array.map (
        //         fun v -> Array.filter (fun v2 -> v2 = v) sortedMerge
        //         >> Array.length
        //     )
        // let myMap = sortedMerge |> Array.mapi (fun i x -> x, i + 2 - Array.item i abundance) |> Map // wrong: must return mean of ranksums with equal ranks, not always the same rank!
        // let rankedMerge = sortedMerge |> Array.map (fun (v,group) -> float myMap.[(v,group)],v,group)
        let rankedMerge = // method for mean ranks instead of equal ranks when identical values occur.
            sortedMerge |> Array.map fst |> Rank.rankAverage 
            |> fun res -> 
                (sortedMerge,res)
                ||> Array.map2 (fun (v,group) rank -> rank,v,group)
        let calcRankSum group = 
            rankedMerge
            |> Array.filter (fun (rank,v,group') -> group' = group)
            |> Array.fold (fun state (rank,v,group') -> state + rank) 0.
        let rankSumSeq1 = calcRankSum 0
        let rankSumSeq2 = calcRankSum 1
        let seq1Length = Seq.length seq1 |> float
        let seq2Length = Seq.length seq2 |> float
        let u1 = seq1Length * seq2Length + (seq1Length * (seq1Length + 1.) / 2.) - rankSumSeq1
        let u2 = seq1Length * seq2Length + (seq2Length * (seq2Length + 1.) / 2.) - rankSumSeq2
        let uMin = min u1 u2
        let z = (uMin - seq1Length * seq2Length / 2.) / System.Math.Sqrt (seq1Length * seq2Length * (seq1Length + seq2Length + 1.) / 12.)
        z
        
    /// Computes a one-tailed U-test.
    let inline oneTailed (seq1: seq<'T>) (seq2: seq<'T>) =
        let z = computeUtest seq1 seq2
        (Distributions.Continuous.normal 0. 1.).CDF z

    /// Computes a two-tailed U-test.
    let inline twoTailed (seq1: seq<'T>) (seq2: seq<'T>) =
        let z = computeUtest seq1 seq2
        (Distributions.Continuous.normal 0. 1.).CDF z * 2.