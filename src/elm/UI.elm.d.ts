export const Elm: ElmType;

export type ElmType = {
  UI: {
    init: (config: { node: HTMLElement; flags: any }) => UIInstance;
  };
};

export type UIInstance = {
  ports: {
    save: {
      subscribe(callback: (value: any) => void): void;
    };
  };
};
