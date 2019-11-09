﻿namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    | ValidatePendingCancelRequest of UserId * Guid
    | PendingCancelRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | ValidatePendingCancelRequest (userId, _) -> userId
        | PendingCancelRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCanceled of TimeOffRequest
    | RequestCancelPending of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCanceled request -> request
        | RequestCancelPending request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest 
        | Canceled of TimeOffRequest
        | Validated of TimeOffRequest
        | PendingCancelValidation of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | PendingCancelValidation request
            | Validated request -> request
            | Canceled request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _ -> true
            | PendingCancelValidation _ ->true
            | Validated _ -> true
            | Canceled _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCanceled request -> Canceled request
        | RequestCancelPending request -> Canceled request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith (request1: TimeOffRequest) (request2: TimeOffRequest) =
        (request1.Start.Date = request2.Start.Date) || (request1.End.Date = request2.End.Date) //TODO: write a function that checks if 2 requests overlap

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        let mutable result = false
        for s in otherRequests do 
           result <- overlapsWith request s
        result //TODO: write this function using overlapsWith

    let createRequest activeUserRequests  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= DateTime.Today.AddDays(1.) then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let cancelRequest requestState =
      match requestState with
        | PendingValidation request ->
            Ok [RequestCanceled request]
        | _ ->
            Error "Request cannot be canceled"

    let validatePendingCancelRequest requestState =
        match requestState with
        | PendingCancelValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
    let pendingCancelRequest requestState =
      match requestState with
        | PendingCancelValidation request ->
            Ok [RequestCancelPending request]
        | _ ->
            Error "Request cannot be canceled"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request
                
            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
                    
            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                let mutable isCancel = false
                if user = Manager then
                    cancelRequest requestState
                else                              
                    let activeUserRequests =
                        userRequests
                        |> Map.toSeq
                        |> Seq.map (fun (_, state) -> state)
                        |> Seq.where (fun state -> state.IsActive)
                        |> Seq.map (fun state -> state.Request)     
                    for request in activeUserRequests do
                        if request.RequestId = requestId then
                            if request.Start.Date <= DateTime.Today then
                                isCancel <- true
                    if isCancel then                   
                        cancelRequest requestState
                    else
                        Error "Unauthorized"

            | ValidatePendingCancelRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validatePendingCancelRequest requestState

            | PendingCancelRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else                            
                    let activeUserRequests =
                        userRequests
                        |> Map.toSeq
                        |> Seq.map (fun (_, state) -> state)
                        |> Seq.where (fun state -> state.IsActive)
                        |> Seq.map (fun state -> state.Request)  
                    
                    Error "Unauthorized"