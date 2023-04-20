import { Elm } from "../elm/UI.elm";
import { init } from "./main";

init(Elm);

if ("serviceWorker" in navigator)
  navigator.serviceWorker.register(
    new URL("../service-worker.ts", import.meta.url),
    { type: "module" }
  );
