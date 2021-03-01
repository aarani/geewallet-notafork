namespace GeeTower.API

open System

type TowerAPIError = | UnsupportedCurrency

type TowerAPIResponseOrError<'T> =
    | TowerAPIResponse of 'T
    | TowerAPIError of TowerAPIError
