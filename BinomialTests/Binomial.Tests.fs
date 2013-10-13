module BinomialTests

open System
open NUnit.Framework
open FsUnit

[<TestFixture>] 
type ``Given the CumulativeDistribution function`` () =
    let errorMargin = 1e-10

    [<TestCase(1,1,0.1,1.0000000000)>]
    [<TestCase(1,10,0.1,0.7360989291)>]
    [<TestCase(1,100,0.1,0.0003216881)>]
    [<TestCase(1,100,0.01,0.7357619789)>]
    [<TestCase(12,345,0.012345,0.9995592084)>]
    [<TestCase(10,1029,0.02,0.0074346572)>]
    [<TestCase(10,1030,0.02,0.0073522357)>]
    [<TestCase(10,1031,0.02,0.0072706707)>]
    [<TestCase(179,12798,0.012013,0.9797611704)>]
    [<TestCase(12798,12798,0.012013,1.0)>]
    member t.
        ``the function produces the correct result`` (x, n, p, expected) =
            let actual = Binomial.CumulativeDistribution x n p
            actual |> should (equalWithin errorMargin) expected

