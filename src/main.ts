import { ElmType } from "./elm/UI.elm";

export function init(Elm: ElmType) {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  Elm.UI.init({
    node: node,
    flags: {},
  });
}
