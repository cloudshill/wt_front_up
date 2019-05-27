'use strict';

require("foundation-sites-loader")
require("./styles/wingtask.scss");
require("./elm-mdc.js");

const {Elm} = require('../src/Main');

var flags = {
  flags: 6,
  session: (localStorage.session || null),
  uuidSeed: Math.floor(Math.random()*0x0FFFFFFF)
}

var app = Elm.Main.init(flags);

app.ports.storeSession.subscribe(function(session) {
  localStorage.session = session;
});

window.addEventListener("storage", function(event) {
  if (event.storageArea === localStorage && event.key === "session") {
    app.ports.onSessionChange.send(event.newValue);
  }
}, false);

window.addEventListener("scroll", function(event) {
    app.ports.onScroll.send( { pageX: 0, pageY : window.pageYOffset } )
});
