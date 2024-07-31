type CuisineType =
    | Korean
    | Turkish

type RestaurantDetails = {
    Name: string
    Cuisine: CuisineType
    Cost: float
}

type MovieCategory =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

type ActivityType =
    | BoardGame
    | ChillOut
    | Movie of MovieCategory
    | Dining of CuisineType
    | RoadTrip of int * float

let computeCost activity =
    match activity with
    | BoardGame -> 0.0, 0
    | ChillOut -> 0.0, 0
    | Movie category ->
        match category with
        | Regular -> 12.0, 0
        | IMAX -> 17.0, 0
        | DBOX -> 20.0, 0
        | RegularWithSnacks | IMAXWithSnacks | DBOXWithSnacks ->
            let baseRate =
                match category with
                | RegularWithSnacks -> 12.0
                | IMAXWithSnacks -> 17.0
                | DBOXWithSnacks -> 20.0
                | _ -> 0.0
            baseRate + 5.0, 0
    | Dining cuisine ->
        match cuisine with
        | Korean -> 70.0, 0
        | Turkish -> 65.0, 0
    | RoadTrip (distance, fuelCostPerKm) ->
        let totalCost = float distance * fuelCostPerKm
        totalCost, distance

let sampleActivities = [
    BoardGame
    ChillOut
    Movie Regular
    Movie IMAX
    Movie DBOX
    Movie RegularWithSnacks
    Dining Korean
    Dining Turkish
    RoadTrip (100, 1.25)
]

sampleActivities
|> List.iter (fun activity ->
    match computeCost activity with
    | cost, distance when distance > 0 ->
        printfn "Activity: RoadTrip (%d km), Cost: %.2f CAD" distance cost
    | cost, _ ->
        printfn "Activity: %A, Cost: %.2f CAD" activity cost)
