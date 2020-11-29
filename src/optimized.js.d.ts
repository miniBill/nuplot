export const Elm: {
  UI: {
    init: (config: { node: HTMLElement; flags: {} }) => UIInstance;
  };
};

export type UIInstance = {
  ports: {
    toWorker: {
      subscribe: (callback: (json: string) => void) => void;
    };
    fromWorker: {
      send: (json: string) => void;
    };
  };
};
