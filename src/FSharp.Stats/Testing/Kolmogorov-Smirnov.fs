namespace FSharp.Stats.Testing

module Kolmogorov_Smirnov =
    
    /// Tests the input values with the Kolmogorov-Smirnov Goodness of Fit-test and returns a p-Value.
    let goF values =
        // parameters
        let values = [|151.;154.;156.;156.;157.;158.;160.;161.;161.;161.;161.;162.;162.;162.;163.;163.;164.;164.;164.;165.;165.;166.;166.;167.;168.;169.;170.;171.;171.;172.|]
        // flow
        let lVals = Array.length values |> float
        let meanVals = FSharp.Stats.Seq.mean values
        let stdVals = FSharp.Stats.Seq.stDev values
        let rankedNormVals = values |> Array.sort |> Array.mapi (fun i _ -> float i / lVals)
        //let meanVals = FSharp.Stats.Seq.mean rankedNormVals
        //let stdVals = FSharp.Stats.Seq.stDev rankedNormVals
        let cum = Array.map (fun x -> (FSharp.Stats.Distributions.Continuous.normal meanVals stdVals).CDF x) rankedNormVals
        let deltaCumVals = (cum,values) ||> Array.map2 (fun c v -> System.Math.Abs (c - v))
        Array.max deltaCumVals