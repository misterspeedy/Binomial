module Binomial

// This algorithm is necessary because the standard formula for Cumulative Binomial Distribution
// involves calculating and dividing some factorials, which very easily leads to overlow.
//
// The algorithm below is Microsoft's solution to this problem in Excel (reference
// http://support.microsoft.com/kb/827459).  Here we do not bother to use Microsoft's approach
// of using a different algorithm below n=1030 as the algorithm below appears to produce
// correct values at any reasonable n.
//

let CumulativeDistribution (x : int) (n : int) (p : float) =
    let EssentiallyZero = 10E-12
    let m = (float(n) * p) |> truncate |> int

    let CalcCurrent value k = 
        if k > m then 
            value * float(n - k + 1) * p / (float(k) * (1. - p))
        else
            value * float(k + 1) * (1. - p) / (float(n - k) * p)

    let Done k = if k > m then k > n else k < 0
    let NextK k = if k > m then k + 1 else k - 1

    let rec Calculate k totalUnscaledProbability previous unscaled =
        let current = CalcCurrent previous k
        let totalUnscaledProbability' = totalUnscaledProbability + current
        let unscaled' =
            if (k <= x) then
                unscaled + current
            else
                unscaled
        if (Done k) && (current <= EssentiallyZero) then
            unscaled', totalUnscaledProbability'
        else
            Calculate (NextK k) totalUnscaledProbability' current unscaled'   

    let InitialUnscaled = if (m <= x) then 1. else 0.

    let UnscaledResultAboveM, TotalUnscaledProbabilityAboveM = Calculate (m+1) 1. 1. InitialUnscaled
    let UnscaledResult, TotalUnscaledProbability = Calculate (m-1) TotalUnscaledProbabilityAboveM 1. UnscaledResultAboveM

    UnscaledResult / TotalUnscaledProbability