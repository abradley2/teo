module User exposing (User, UserId(..), userId)


type UserId
    = UserId String


userId : User -> String
userId user =
    case user.userId of
        UserId id ->
            id


type alias User =
    { userId : UserId
    }
