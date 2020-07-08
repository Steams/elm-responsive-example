module Session exposing ( Session, init)

import Browser.Navigation as Nav


init : () -> Nav.Key -> Session
init () navKey =
    {  navKey = navKey }

type alias Session =
    {  navKey : Nav.Key }
