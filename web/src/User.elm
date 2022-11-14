module User exposing (User, UserId(..))


type UserId
    = UserId String


type alias User =
    { userId : UserId
    }
