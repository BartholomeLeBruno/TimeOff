namespace TimeOff

open System

// First, we define our domain
type UserId = string

type UserType =
    | Employee of UserId
    | Manager

type User =  {
    User: UserType
    EntryDate: DateTime
}

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

[<CLIMutable>]
type Vacations = {
    Vacation: Map<UserId, List<TimeOffRequest>>
}