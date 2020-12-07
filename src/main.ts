import { ElmType } from "./elm/UI.elm";

export function init(Elm: ElmType) {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  var saved: { [key: string]: string } = {};
  for (var i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (key === null) continue;
    const value = localStorage.getItem(key);
    if (value === null) continue;
    saved[key] = value;
  }
  console.log("init with flags: ", saved);

  var app = Elm.UI.init({
    node: node,
    flags: saved,
  });
  app.ports.save.subscribe((param) => {
    const { key, value } = param;
    console.log("localStorage.setItem(", key, ",", value, ")");
    localStorage.setItem(key, value);
  });
}