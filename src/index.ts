import { Elm } from "./elm/UI.elm"

var worker: Worker = new Worker("/worker.ts");

worker.addEventListener("message", function (e) {
    switch (e.data.request) {
        case 'plot':
            app.ports.plotted.send(e.data.response);
            break;
    }
});

var app = Elm.UI.init({
    node: document.getElementById("main"),
    flags: 1
});

app.ports.plot.subscribe(expression => {
    worker.postMessage(JSON.stringify({
        request: "plot",
        expression: expression
    }));
});
