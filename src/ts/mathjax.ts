declare global {
  interface Window {
    MathJax: {
      texReset(): void;

      svgStylesheet(): string;

      getMetricsFor(node: HTMLElement, display: boolean): Options;

      tex2svgPromise(src: string, options: Options): Promise<HTMLElement>;

      tex2chtmlPromise(src: string, options: Options): Promise<HTMLElement>;

      startup: {
        typeset: boolean;

        document: {
          clear(): void;
          updateDocument(): void;
          outputJax: {
            svgStyles: HTMLStyleElement;
          };
        };
      };
    };
  }
}

export type Options = {
  display: boolean;
  scale: number;
  lineWidth: number;
  containerWidth: number;
};

export class MathJaxElement extends HTMLElement {
  wrapper: HTMLElement;

  src = "";
  containerWidth = 1024;

  constructor() {
    super();

    // Create a shadow root
    const shadowRoow = this.attachShadow({ mode: "open" }); // sets and returns 'this.shadowRoot'

    this.wrapper = document.createElement("div");
    this.wrapper.style.overflowX = "auto";

    // attach the created elements to the shadow DOM
    shadowRoow.append(this.wrapper);
  }

  attributeChangedCallback(name: string, _oldValue: string, newValue: string) {
    switch (name) {
      case "tex-src":
        if (!newValue) return;
        this.src = newValue || "";
        this.render();
        break;
      case "container-width":
        if (!newValue) return;
        this.containerWidth = +newValue;
        this.render();
        break;
    }
  }

  render() {
    if (this.src == "") return;
    this.wrapper.innerHTML = "Rendering...";
    if (!window.MathJax || !("texReset" in window.MathJax)) {
      requestAnimationFrame(this.render.bind(this));
      return;
    }
    window.MathJax.texReset();
    this.wrapper.style.maxWidth = this.containerWidth + "px";
    requestAnimationFrame(() => {
      var options = window.MathJax.getMetricsFor(this.wrapper, false);

      window.MathJax.tex2svgPromise(this.src, options)
        .then((node) => {
          this.wrapper.innerHTML =
            "<style>mjx-assistive-mml { display: none !important; }</style>";
          this.wrapper.appendChild(node);
          window.MathJax.startup.document.clear();
          window.MathJax.startup.document.updateDocument();
        })
        .catch((err) => {
          this.wrapper.innerHTML = "";
          const errorNode = document.createElement("pre");
          errorNode.appendChild(document.createTextNode(err.message));
          errorNode.style.whiteSpace = "pre-wrap";
          this.wrapper.appendChild(errorNode);
        });
    });
  }

  static get observedAttributes() {
    return ["tex-src", "container-width"];
  }
}
