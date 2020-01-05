﻿namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    | ValidatePendingCancelRequest of UserId * Guid
    | CancelPendingCancelRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | ValidatePendingCancelRequest (userId, _) -> userId
        | CancelPendingCancelRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCanceled of TimeOffRequest
    | RequestCancelPendingCanceled of TimeOffRequest
    | RequestCancelPendingValidated of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCanceled request -> request
        | RequestCancelPendingCanceled request -> request
        | RequestCancelPendingValidated request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest 
        | Canceled of TimeOffRequest
        | Validated of TimeOffRequest
        | PendingCancelValidation of TimeOffRequest 
        | PendingCancelValidated of TimeOffRequest
        | PendingCancelCanceled of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Canceled request -> request
            | PendingCancelValidation request -> request
            | PendingCancelValidated request -> request
            | PendingCancelCanceled request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _ -> true
            | PendingCancelValidation _ -> true
            | PendingCancelValidated _ -> false
            | PendingCancelCanceled _ -> true
            | Validated _ -> true
            | Canceled _ -> false

    type UserRequestsState = Map<Guid, RequestState>
    let getCurrentDay () =
        DateTime.Today

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCanceled request -> Canceled request
        | RequestCancelPendingCanceled request -> Canceled request
        | RequestCancelPendingValidated request -> Validated request

    let getBetweenDate (firstDate: DateTime) (secondDate: DateTime) =
        let mutable theDate = 0
        let mutable testableDate = firstDate
        while firstDate.Date <> secondDate.Date do
            testableDate <- firstDate.AddDays(1.)
            if firstDate.DayOfWeek <> DayOfWeek.Sunday || firstDate.DayOfWeek <> DayOfWeek.Saturday then
                theDate <- theDate + 1;
        theDate

    // Calcul du cumul des congés
    let getTheoricallAvailableVacation (user: UserId) =
        let mutable availableVacation = 0.
        if (getCurrentDay().Month > 1) then
            availableVacation <-  2.5 * (float)(getCurrentDay().Month - 1)
        availableVacation

    // Calcul des congés pris l'année précèdente
    let getPastYearVacation (user: UserId) (allrequests: Vacations) =
        let mutable availableVacation = 2.5 * 12.
        let userRequests =
            allrequests
            |> Map.tryFind user
        for request in userRequests.Value do
            if((request.Start.Date.Year - 1) = (getCurrentDay().Year - 1)) then
                availableVacation <- availableVacation - (float)(getBetweenDate request.Start.Date request.End.Date) 
        availableVacation            

    // Calcul congé effectif
    let getEffectifVacation (user: UserId) (allrequests: Vacations) = 
        let mutable availableVacation = 2.5 * (float)(getCurrentDay().Month)
        let userRequests =
            allrequests
            |> Map.tryFind user
        for request in userRequests.Value do
            if(request.Start.Date.Year = getCurrentDay().Year) then
                availableVacation <- availableVacation - (float)(getBetweenDate request.Start.Date request.End.Date) 
        availableVacation

    // Calcul congés prévu
    let getAlreadyTakenVacation (user: UserId) (allrequests: Vacations) =
        let mutable availableVacation = 0.
        let userRequests =
            allrequests
            |> Map.tryFind user
        for request in userRequests.Value do
            if request.Start.Date.Year = getCurrentDay().Year && request.Start.Date.Day > getCurrentDay().Day then
                availableVacation <- availableVacation + (float)(getBetweenDate request.Start.Date request.End.Date)
        availableVacation            

    // Calcul Solde disponible
    let getAvailableVacation (user: UserId) (allrequests: Vacations) =
        let theoricallAvailableVacation = getTheoricallAvailableVacation user
        let pastYearVacation = getPastYearVacation user allrequests
        let effectifVaction = getEffectifVacation user allrequests
        let alreadyTakenVaction = getAlreadyTakenVacation user allrequests
        (theoricallAvailableVacation + (float) pastYearVacation) - (effectifVaction + alreadyTakenVaction)        

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith (request1: TimeOffRequest) (request2: TimeOffRequest) =
        let mutable overlap = false
        if request1.Start.Date < request2.Start.Date && request1.End.Date < request2.End.Date && request1.End.Date > request2.Start.Date then 
            overlap <- true
        if request2.Start.Date < request1.Start.Date && request2.End.Date < request1.End.Date && request2.End.Date > request1.Start.Date then 
            overlap <- true
        if request1.End.Date <= request2.End.Date && request1.Start.Date >= request2.Start.Date  then
            overlap <- true
        if request1.Start.Date < request2.Start.Date && request1.End.Date > request2.End.Date  then
            overlap <- true 

        overlap

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        let mutable result = false
        for s in otherRequests do 
           if not result then
                result <- overlapsWith request s       
        result

    let createRequest (getCurrentTime : unit->DateTime) activeUserRequests  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= getCurrentTime().AddDays(1.) then
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
            Ok [RequestCancelPendingValidated request]
        | _ ->
            Error "Request cannot be validated"

    let cancelPendingCancelRequest requestState =
      match requestState with
        | PendingCancelValidation request ->
            Ok [RequestCancelPendingCanceled request]
        | _ ->
            Error "Request cannot be canceled"

    let decide (getCurrentTime : unit->DateTime) (userRequests: UserRequestsState) (user: User) (command: Command) =
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
                   
                createRequest (getCurrentTime : unit->DateTime) activeUserRequests request
                
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
                            if request.Start.Date <= getCurrentTime() then
                                isCancel <- true
                            else
                                isCancel <- false
                    if isCancel then                   
                        cancelRequest requestState
                    else
                        cancelPendingCancelRequest requestState

            | ValidatePendingCancelRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validatePendingCancelRequest requestState

            | CancelPendingCancelRequest (_, requestId) ->

                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                let mutable isCancel = false

                if user <> Manager then
                    Error "Unauthorized"
                else                            
                    let activeUserRequests =
                        userRequests
                        |> Map.toSeq
                        |> Seq.map (fun (_, state) -> state)
                        |> Seq.where (fun state -> state.IsActive)
                        |> Seq.map (fun state -> state.Request)  
                    

                    for request in activeUserRequests do
                        if request.RequestId = requestId then
                            isCancel <- true
                    
                    if isCancel then
                        cancelPendingCancelRequest requestState
                    else
                        Error "Unauthorized"