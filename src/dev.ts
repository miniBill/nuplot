import { Elm } from "./elm/UI.elm";
import { init } from "./main";
import { NuPlot } from "./nuplot";

customElements.define("nu-plot", NuPlot);

init(Elm);
