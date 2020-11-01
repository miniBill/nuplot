import { Elm } from "./elm/Worker.elm";

const ctx: Worker = self as any;

// @ts-ignore Allow Elm to work in a Worker
ctx.document = { body: {} };

function plot(data: { request: string; expression: string; }): void {
    var worker = Elm.Worker.init();
    worker.ports.plotted.subscribe(result =>
        ctx.postMessage({
            request: data.request,
            response: {
                expression: data.expression,
                lines: result
            }
        }));
    worker.ports.plot.send(data.expression);
}

self.addEventListener('message', function (e) {
    switch (e.data.request) {
        case 'plot':
            plot(JSON.parse(e.data));
            break;
    }
})
