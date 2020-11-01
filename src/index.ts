import { Elm } from "./elm/UI"

var worker = new Worker("/worker.js");

Elm.UI.init({
    node: document.getElementById("main"),
    flags: {}
})
