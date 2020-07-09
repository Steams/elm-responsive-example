module Session exposing ( Session, Flags, init)

import Browser.Navigation as Nav


init : Flags -> Nav.Key -> Session
init flags navKey =
    {  navKey = navKey, flags = flags}

type alias Flags = {width: Int, height: Int}
type alias Session =
    {  navKey : Nav.Key , flags : Flags}
