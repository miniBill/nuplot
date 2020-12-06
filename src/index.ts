import { Elm } from "./optimized.js";
import { NuPlot } from "./nuplot";
import { init } from "./main";

customElements.define("nu-plot", NuPlot);

init(Elm);
