import { PlotResult } from "./PlotResult"


export const Elm: {
    UI: {
        init: (config: { node: HTMLElement, flags: {} }) => UIInstance
    }
}


export type UIInstance = {
    ports: {
        plot: {
            subscribe: ((callback: ((expression: string) => void)) => void)
        },
        plotted: {
            send: (result: PlotResult) => void
        }
    }
}
