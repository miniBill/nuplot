import { PlotResult } from "./PlotResult"


export const Elm: {
    Worker: {
        init: () => WorkerInstance
    }
}


export type WorkerInstance = {
    ports: {
        plot: {
            send: ((expression: string) => void)
        },
        plotted: {
            subscribe: ((callback: (result: PlotResult) => void) => void)
        }
    }
}
