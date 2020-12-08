declare global {
  interface Window {
    MathJax: {
      texReset(): void;

      getMetricsFor(node: HTMLElement): Options;

      tex2svgPromise(src: string, options: Options): Promise<HTMLElement>;

      startup: {
        document: {
          clear(): void;
          updateDocument(): void;
        };
      };
    };
  }
}

export type Options = {
  display: boolean;
  scale: number;
};

export class MathJaxElement extends HTMLElement {
  wrapper: HTMLElement;

  src = "";

  constructor() {
    super();

    // Create a shadow root
    const shadowRoow = this.attachShadow({ mode: "closed" }); // sets and returns 'this.shadowRoot'

    this.wrapper = document.createElement("div");

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
    }
  }

  render() {
    this.wrapper.innerHTML = "Rendering...";
    if (!window.MathJax) {
      requestAnimationFrame(this.render.bind(this));
      return;
    }
    window.MathJax.texReset();
    var options = window.MathJax.getMetricsFor(this.wrapper);
    options.scale = 1;
    options.display = false;

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
        const errorNode = document
          .createElement("pre")
          .appendChild(document.createTextNode(err.message));
        this.wrapper.appendChild(errorNode);
      });
  }

  static get observedAttributes() {
    return ["tex-src"];
  }
}
