module cw6
#if INTERACTIVE
#r @"..\packages\NUnit.2.6.4\lib\nunit.framework.dll"
#endif

    open NUnit.Framework
    open FsCheck.NUnit
    open FsCheck
    
//**************
//**For Task 4**
//**************
    [<TestFixture>]
    type ``Given two lists`` () =
       [<Property>]
       member this.
         ``Whether the length is equal when appending and concatenating`` (xs: int list)(ys: int list) =
         (List.append xs ys).Length = (List.concat [xs;ys]).Length
            
       [<Property>]
       member this.
          ``Whether reversing and concat is the same as concat and reversing`` (xs: int list)(ys: int list) =
          List.concat([List.rev xs; List.rev ys]) = List.rev(List.concat[ys;xs])

//Task 2
    [<TestFixture>]
    type ``Given a palindrome`` () =

       [<Property>]
       member this.
         ``Whether is a palindrome`` (xs: int list) =
            xs = (List.rev xs)

//Task 3
    [<TestFixture>]
    type ``Given two palindromes`` () =
        let toPalindrome xs =
              let len       = List.length xs
              let suffixLen = len / 2
              let prefixLen = if 2 * suffixLen = len then suffixLen else suffixLen + 1
              let take n xs = Seq.toList (Seq.take n xs)
              take prefixLen xs @ List.rev (take suffixLen xs)

        [<Property>]
        member this.isPalindrome (xs: int list) =
            toPalindrome xs = List.rev (toPalindrome xs)

//**************
//**For Task 5**
//**************
    
    type Client = 
      { Name : string; Income : int ; YearsInJob : int
        UsesCreditCard : bool;  CriminalRecord : bool }

    type QueryInfo =
      { Title     : string
        Check     : Client -> bool
        Positive  : Decision
        Negative  : Decision }

    and Decision = 
       | Result of string
       | Query  of QueryInfo

    let rec tree =
       Query  {Title = "More than €40k"
               Check = (fun cl -> cl.Income > 40000)
               Positive = moreThan40
               Negative = lessThan40}
    and moreThan40 =
       Query  {Title = "Has criminal record"
               Check = (fun cl -> cl.CriminalRecord)
               Positive = Result "no"
               Negative = Result "yes"}
    and lessThan40 =
       Query  {Title = "Years in job"
               Check = (fun cl -> cl.YearsInJob > 1)
               Positive = Result "yes"
               Negative = usesCreditCard}
    and usesCreditCard =
       Query  {Title = "Uses credit card"
               Check = (fun cl -> cl.UsesCreditCard)
               Positive = Result "yes"
               Negative = Result "no"}

    let rec testClientTree client tree =
        match tree with
        | Result msg  -> if msg = "yes" then true else false
        | Query qinfo -> let result, case = 
                             if qinfo.Check(client) then
                                 "yes", qinfo.Positive
                             else
                                 "no", qinfo.Negative
                         testClientTree client case
    
    [<TestFixture>]
    type ``Given a client`` () =

        let john1 = {Name = "John Doe"; Income = 40001 ; YearsInJob = 1 ; UsesCreditCard = true ; CriminalRecord = false }
        let john2 = {Name = "John Doe"; Income = 0 ; YearsInJob = 0 ; UsesCreditCard = false ; CriminalRecord = false }
        let john5 = {Name = "John Doe"; Income = 40001 ; YearsInJob = 2 ; UsesCreditCard = true ; CriminalRecord = true }
        let john6 = {Name = "John Doe"; Income = 100 ; YearsInJob = 1 ; UsesCreditCard = true ; CriminalRecord = false }

        [<Property>]
        member this.decide (client:Client)(tree: Decision) = testClientTree client tree

        [<Test>]
        member this.``No income but years at work and credit card gives loan`` () =
            Assert.AreEqual (testClientTree john6 tree, true)

        [<Test>]
        member this.``Income>40k and no criminal record gives loan`` () =
            Assert.AreEqual (testClientTree john1 tree, true)

        [<Test>]
        member this.``Income<40k does not give loan`` () =
            Assert.AreEqual (testClientTree john2 tree, false)

        [<Test>]
        member this.``Criminal record does not give loan`` () =
            Assert.AreEqual (testClientTree john5 tree, false)