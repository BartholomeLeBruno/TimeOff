module TimeOff.Tests

open Expecto
open System

let getCurrentDay () =
  DateTime.Today

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)
        
    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide getCurrentDay userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days start and end overlaps" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 4); HalfDay = PM }
      }
   
      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }

    test "Requests on 2 distinct days start and end overlaps version 2" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 4); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
      }
   
      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }

    test "Requests are equals" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }
   
      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }

    test "Request 2 is into the request 1" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 4); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
      }
   
      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }

    test "Request 1 is into the request 2" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 4); HalfDay = PM }
      }
   
      Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }
   
      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }

    test "Seq of request don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      let request3 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 3); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
      }
      let resultseq = [request2;request1]
      Expect.isFalse (Logic.overlapsWithAnyRequest resultseq request3) "The requests don't overlap"
    }

    test "Seq of request overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 4); HalfDay = PM }
      }

      let request3 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 3); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
      }
      let resultseq = [request2;request1]
      Expect.isTrue (Logic.overlapsWithAnyRequest resultseq request3) "The requests overlap"
    }
  ]

//[<Tests>]
//let creationTests =
//  testList "Creation tests" [
//    test "A request is created" {
//      let request = {
//        UserId = "jdoe"
//        RequestId = Guid.NewGuid()
//        Start = { Date = DateTime(2020, 01, 05); HalfDay = AM }
//        End = { Date = DateTime(2020, 01, 05); HalfDay = PM } }
//
//      Given [ ]
//      |> ConnectedAs (Employee "jdoe")
//      |> When (RequestTimeOff request)
//      |> Then (Ok [RequestCreated request]) "The request should have been created"
//    }
//  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 05); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 05); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let validationTestsNotAsManager =
  testList "Validation tests not as manager" [
    test "A request is not validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 05); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 05); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request should'nt have been validated"
    }
  ]


[<Tests>]
let cancelTests =
  testList "Cancel tests" [
    test "A request is canceled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 05); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 05); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCanceled request]) "The request should have been canceled"
    }
  ]

[<Tests>]
let vacationTests = 
  testList "Calcul Vacation tests" [
    test "A user should have 7.5 vacation available" {
      let expected = 7.5
      let date = DateTime(2020,04,01)
      Expect.equal expected (Logic.getTheoricallAvailableVacation (date)) "A user Should have 7.5 vacation theorically"
    }
    test "A user should have taken 2 days since the beginning of the year" {
      let user = "jdoe"
      let date = DateTime(2020,04,01)
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 06); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 07); HalfDay = PM } }
      let vacations = 
        Map.empty.
          Add("jdoe", [| request |] :> seq<TimeOffRequest>)
      let expected = 2.        
      Expect.equal (Logic.getEffectifVacation user vacations date) expected "A user should have taken 2 days"
    }
  ]