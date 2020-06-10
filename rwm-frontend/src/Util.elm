module Util exposing (updateWith)

updateWith : (subModel -> model) 
          -> (subMsg -> msg)
          -> ( subModel, Cmd subMsg )
          -> ( model, Cmd msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
