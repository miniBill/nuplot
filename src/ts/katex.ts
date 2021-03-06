declare global {
  interface Window {
    katex: {
      render(tex: string, node: HTMLElement): void;
    };
  }
}

export type Options = {
  display: boolean;
  scale: number;
  lineWidth: number;
  containerWidth: number;
};

export class KaTeXElement extends HTMLElement {
  wrapper: HTMLElement;

  src = "";

  constructor() {
    super();

    // Create a shadow root
    const shadowRoow = this.attachShadow({ mode: "open" }); // sets and returns 'this.shadowRoot'

    this.wrapper = document.createElement("div");
    this.wrapper.style.overflowX = "auto";
    this.wrapper.style.overflowY = "hidden";

    var link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.css";
    link.integrity =
      "sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X";
    link.crossOrigin = "anonymous";

    var style = document.createElement("style");
    style.innerHTML = ".katex { padding: 0.5em }";

    // attach the created elements to the shadow DOM
    shadowRoow.append(this.wrapper);

    shadowRoow.append(link);
    shadowRoow.append(style);
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
        this.wrapper.style.maxWidth = newValue + "px";
        break;
    }
  }

  render() {
    if (this.src == "") return;
    this.wrapper.innerHTML = "Rendering...";
    if (!window.katex) {
      debugger;
      requestAnimationFrame(this.render.bind(this));
      return;
    }
    requestAnimationFrame(() => {
      try {
        window.katex.render(this.src, this.wrapper);
      } catch (err) {
        this.wrapper.innerHTML = "";
        const errorNode = document.createElement("pre");
        errorNode.appendChild(document.createTextNode(err.message));
        errorNode.style.whiteSpace = "pre-wrap";
        this.wrapper.appendChild(errorNode);
      }
    });
  }

  static get observedAttributes() {
    return ["tex-src", "container-width"];
  }
}
