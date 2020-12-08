export const Elm: ElmType;

export type ElmType = {
  UI: {
    init: (config: {
      node: HTMLElement;
      flags: { [key: string]: string };
    }) => UIInstance;
  };
};

export type UIInstance = {
  ports: {
    save: {
      subscribe(
        callback: (param: { key: string; value: string }) => void
      ): void;
    };
  };
};
