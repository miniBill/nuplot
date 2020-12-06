export const Elm: ElmType;

export type ElmType = {
  UI: {
    init: (config: { node: HTMLElement; flags: {} }) => UIInstance;
  };
};

export type UIInstance = {};
