export const Elm: ElmType;

export type ElmType = {
  UI: {
    init: (config: {
      node: HTMLElement;
      flags: {
        saved: { [key: string]: string };
        hasClipboard: boolean;
        hasFullscreen: boolean;
        languages: readonly string[];
        rootUrl: string;
        googleAccessToken: string;
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
    fullscreen: {
      subscribe(callback: (id: string) => void): void;
    };
    exitFullscreen: {
      subscribe(callback: (id: string) => void): void;
    };
    saveGoogleAccessToken: {
      subscribe(callback: (token: string) => void): void;
    };
    saveGoogleAccessTokenAndCloseWindow: {
      subscribe(callback: (token: string) => void): void;
    };
    openWindow: {
      subscribe(callback: (url: string) => void): void;
    };
    gotGoogleAccessToken: {
      send(token: string): void;
    };
    isFullscreen: {
      send(isFullscreen: boolean): void;
    };
  };
};
