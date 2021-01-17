export const Elm: ElmType;

export type ElmType = {
  UI: {
    init: (config: {
      node: HTMLElement;
      flags: {
        saved: { [key: string]: string };
        hasClipboard: boolean;
        languages: readonly string[];
        rootUrl: string;
      };
    }) => UIInstance;
  };
};

export type UIInstance = {
  ports: {
    persist: {
      subscribe(
        callback: (param: { key: string; value: string }) => void
      ): void;
    };
    save: {
      subscribe(callback: (id: string) => void): void;
    };
    copy: {
      subscribe(callback: (id: string) => void): void;
    };
  };
};
