module Logout (Endpoints, handlers, button) where

import Lucid (Html, action_, body_, button_, form_, h1_, head_, html_, method_, p_, renderBS, script_, title_, type_)
import Network.HTTP.Types.Header (hSetCookie)
import Servant (Get, Handler, JSON, NoContent, Post, err400, errBody, throwError, (:<|>) (..), (:>))
import Session.Cookie qualified
import Util.Redirect (Path (VerifyLoggedOut), VerifyLoggedOut, redirectTo)
import Web.Cookie (renderSetCookieBS)

type Endpoints =
  "logout" :> Post '[JSON] NoContent
    :<|> VerifyLoggedOut :> Get '[JSON] NoContent

handlers :: Handler NoContent :<|> Handler NoContent
handlers = buttonHandler :<|> verifyHandler

buttonHandler :: Handler NoContent
buttonHandler = redirectTo [(hSetCookie, renderSetCookieBS Session.Cookie.deleteJWTSet)] VerifyLoggedOut

verifyHandler :: Handler NoContent
verifyHandler = throwError err400 {errBody = renderBS htmlResponse}
  where
    htmlResponse :: Html ()
    htmlResponse = html_ $ do
      head_ $ title_ "Logout Failed"
      body_ $ do
        h1_ "Logout Failed"
        p_ "You are still logged in. Please try logging out again."
        button

button :: Html ()
button = form_ [method_ "post", action_ "/logout"] $ do
  button_ [type_ "submit"] "Logout"
  script_
    "document.addEventListener('DOMContentLoaded', function() {\
    \  const logoutForm = document.getElementById('logoutForm');\
    \  logoutForm.addEventListener('submit', function(event) {\
    \    event.preventDefault();\
    \    fetch('/logout', { method: 'GET' })\
    \      .then(response => {\
    \        if (response.redirected) {\
    \          window.location.href = response.url;\
    \        }\
    \      });\
    \  });\
    \});"
