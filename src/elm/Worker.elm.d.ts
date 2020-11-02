export const Elm: {
    Worker: {
        init: () => WorkerInstance
    }
}

export type WorkerInstance = {
    ports: {
        toWorker: {
            send: ((json: string) => void)
        },
        fromWorker: {
            subscribe: ((callback: (json: string) => void) => void)
        }
    }
}
