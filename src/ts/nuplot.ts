import declarations from "../shaders/declarations.frag";

export class NuPlot extends HTMLElement {
  wrapper: HTMLElement;

  canvas: HTMLCanvasElement;
  gl: WebGLRenderingContext | null = null;
  program: WebGLProgram | null = null;
  fragment_shader: WebGLShader | null = null;

  src = "void main() { gl_FragColor = vec4(0); }";

  whiteLines = 6;
  completelyReal = 0;
  minDepth = 25;
  currDepth = 25;
  maxDepth = 400;

  /* these hold the state of zoom operation */
  zoom_center!: number[];
  target_zoom_center!: number[];
  viewport_width!: number;
  zoom_factor!: number;
  label: HTMLElement;

  pendingRaf = -1;
  pendingTimeout = -1;
  wasZooming = false;

  constructor() {
    super();

    this.reinit_zoom();

    // Create a shadow root
    const shadowRoow = this.attachShadow({ mode: "open" }); // sets and returns 'this.shadowRoot'

    this.wrapper = document.createElement("div");

    this.label = this.wrapper.appendChild(document.createElement("div"));

    var checkboxLabel = this.wrapper.appendChild(
      document.createElement("label")
    );

    this.canvas = this.wrapper.appendChild(document.createElement("canvas"));
    this.initCanvas();

    // attach the created elements to the shadow DOM
    shadowRoow.append(this.wrapper);
  }

  reinit_zoom() {
    this.zoom_center = [0.0, 0.0];
    this.target_zoom_center = [0.0, 0.0];
    this.viewport_width = 2 * Math.PI;
    this.zoom_factor = 1;
  }

  private initCanvas() {
    this.canvas.width = this.canvas.height = 400;

    this.gl = this.canvas.getContext("webgl");

    if (this.gl == null) return;

    const vertex_shader = `precision highp float;
    attribute vec2 a_Position;
    void main() {
      gl_Position = vec4(a_Position.x, a_Position.y, 0.0, 1.0);
    }`;

    this.program = this.gl.createProgram();

    if (this.program == null) {
      console.error("invalid program");
      return;
    }

    if (!this.compileAndAttachShader(this.gl.VERTEX_SHADER, vertex_shader))
      return;

    const fragment_shader = this.buildFragmentShader(this.src);
    this.fragment_shader = this.compileAndAttachShader(
      this.gl.FRAGMENT_SHADER,
      fragment_shader
    );
    if (!this.fragment_shader) return;

    this.gl.linkProgram(this.program);
    this.gl.useProgram(this.program);

    /* create a vertex buffer for a full-screen triangle */
    var vertex_buf = this.gl.createBuffer();
    this.gl.bindBuffer(this.gl.ARRAY_BUFFER, vertex_buf);
    this.gl.bufferData(
      this.gl.ARRAY_BUFFER,
      new Float32Array([-1, -1, 3, -1, -1, 3]),
      this.gl.STATIC_DRAW
    );

    /* set up the position attribute */
    var position_attrib_location = this.gl.getAttribLocation(
      this.program,
      "a_Position"
    );
    this.gl.enableVertexAttribArray(position_attrib_location);
    this.gl.vertexAttribPointer(
      position_attrib_location,
      2,
      this.gl.FLOAT,
      false,
      0,
      0
    );

    /* input handling */
    this.canvas.onmousedown = this.canvasOnmousedown.bind(this);
    this.canvas.oncontextmenu = (e) => false;
    this.canvas.onmouseup = this.canvasOnmouseup.bind(this);
    this.canvas.onmousemove = this.canvasOnmousemove.bind(this);

    /* display initial frame */
    this.rafRenderFrame();
  }

  private buildFragmentShader(expr: string) {
    const res = `${declarations}\n${expr}`;
    return res;
  }

  compileAndAttachShader(type: number, src: string): WebGLShader | null {
    if (!this.gl || !this.program) return null;
    const shader = this.gl.createShader(type);

    if (shader == null) {
      console.error("invalid shader");
      return null;
    }

    if (!this.sourceAndCompile(shader, src)) return null;

    this.gl.attachShader(this.program, shader);

    return shader;
  }

  canvasOnmousedown(e: MouseEvent) {
    if (e.buttons == 4) {
      // central wheel
      this.reinit_zoom();
    } else {
      const zoom_speed = 0.02;
      this.zoom_factor = e.buttons & 1 ? 1 - zoom_speed : 1 + zoom_speed;
    }
    this.rafRenderFrame(true);
    return true;
  }

  canvasOnmousemove(e: MouseEvent) {
    this.target_zoom_center[0] = e.offsetX / this.canvas.width;
    this.target_zoom_center[1] = 1 - e.offsetY / this.canvas.height;

    return true;
  }

  canvasOnmouseup(e: MouseEvent) {
    this.zoom_factor = 1.0;
  }

  delayedRenderFrame(delay: number) {
    if (this.pendingRaf >= 0 || this.pendingTimeout >= 0) return;
    this.pendingTimeout = window.setTimeout(() => {
      this.pendingTimeout = -1;
      this.pendingRaf = window.requestAnimationFrame(
        this.renderFrame.bind(this)
      );
    }, delay);
  }

  rafRenderFrame(rightNow: boolean = false) {
    if (this.pendingRaf >= 0) return;
    if (this.pendingTimeout >= 0)
      if (rightNow) {
        window.clearTimeout(this.pendingTimeout);
        this.pendingTimeout = -1;
      } else {
        return;
      }
    this.pendingRaf = window.requestAnimationFrame(this.renderFrame.bind(this));
  }

  renderFrame() {
    this.pendingRaf = -1;

    if (this.gl == null || this.program == null) return;

    if (this.zoom_factor != 1.0) {
      const minx = this.zoom_center[0] - this.viewport_width / 2;

      let viewport_height =
        (this.viewport_width * this.gl.canvas.height) / this.gl.canvas.width;

      const miny = this.zoom_center[1] - viewport_height / 2;

      const targetX = this.target_zoom_center[0] * this.viewport_width + minx;
      const targetY = this.target_zoom_center[1] * viewport_height + miny;

      this.viewport_width *= this.zoom_factor;
      viewport_height *= this.zoom_factor;

      if (this.zoom_factor < 1) {
        const newMinx =
          targetX - this.target_zoom_center[0] * this.viewport_width;
        this.zoom_center[0] = newMinx + this.viewport_width / 2;

        const newMiny = targetY - this.target_zoom_center[1] * viewport_height;
        this.zoom_center[1] = newMiny + viewport_height / 2;
      }

      if (this.currDepth == this.minDepth) {
        this.doRender();
      } else {
        this.currDepth = this.minDepth;
        this.reloadFragmentShader(this.buildFragmentShader(this.src));
      }
      this.rafRenderFrame();
      this.wasZooming = true;
    } else if (this.currDepth < this.maxDepth) {
      this.doRender();
      if (!this.wasZooming) {
        this.currDepth *= 2;
        this.currDepth = Math.min(this.currDepth, this.maxDepth);
        this.reloadFragmentShader(this.buildFragmentShader(this.src));
      }
      this.delayedRenderFrame(this.currDepth == this.minDepth ? 1000 : 100);
      this.wasZooming = false;
    } else if (this.currDepth >= this.maxDepth) {
      this.doRender();
      this.currDepth = this.minDepth;
      this.reloadFragmentShader(this.buildFragmentShader(this.src));
      this.wasZooming = false;
    } else {
      this.doRender();
      this.wasZooming = false;
    }
  }

  doRender() {
    if (this.gl == null || this.program == null) return;

    this.gl.viewport(0, 0, this.gl.canvas.width, this.gl.canvas.height);

    /* bind inputs & render frame */
    this.uniform1f("u_whiteLines", this.whiteLines);
    this.uniform1f("u_completelyReal", this.completelyReal);
    this.uniform1f("u_viewportWidth", this.viewport_width);
    this.uniform1f("u_canvasWidth", this.canvas.width);
    this.uniform1f("u_canvasHeight", this.canvas.height);
    this.uniform2f("u_zoomCenter", this.zoom_center[0], this.zoom_center[1]);

    this.gl.clearColor(0.0, 0.0, 0.0, 1.0);
    this.gl.clear(this.gl.COLOR_BUFFER_BIT);
    this.gl.drawArrays(this.gl.TRIANGLES, 0, 3);
  }

  private uniform1f(name: string, value: number) {
    if (!this.gl || !this.program) return;

    var uniform_location = this.gl.getUniformLocation(this.program, name);
    this.gl.uniform1f(uniform_location, value);
  }

  private uniform2f(name: string, arg1: number, arg2: number) {
    if (!this.gl || !this.program) return;

    var uniform_location = this.gl.getUniformLocation(this.program, name);
    this.gl.uniform2f(uniform_location, arg1, arg2);
  }

  attributeChangedCallback(name: string, _oldValue: string, newValue: string) {
    switch (name) {
      case "canvas-width":
        if (!newValue) return;
        this.canvas.width = +newValue;
        this.rafRenderFrame();
        break;

      case "canvas-height":
        if (!newValue) return;
        this.canvas.height = +newValue;
        this.rafRenderFrame();
        break;

      case "white-lines":
        if (!newValue) return;
        this.whiteLines = +newValue;
        this.rafRenderFrame();
        break;

      case "completely-real":
        if (!newValue) return;
        this.completelyReal = +newValue;
        this.rafRenderFrame();
        break;

      case "expr-src":
        if (newValue == this.src) return;

        //console.log("src changed");
        //console.info(newValue);
        this.src = newValue;
        this.reloadFragmentShader(this.buildFragmentShader(this.src));
        this.rafRenderFrame();
        break;
    }
  }

  reloadFragmentShader(src: string) {
    if (!src || !this.gl || !this.program || !this.fragment_shader) return;

    this.sourceAndCompile(this.fragment_shader, src);

    this.gl.linkProgram(this.program);
  }

  static withLines(input: string): string {
    return input
      .split("\n")
      .map((l, i) => (i + 1).toString().padStart(3) + " " + l)
      .join("\n");
  }

  sourceAndCompile(shader: WebGLShader, built: string) {
    if (!this.gl) return;

    built = `#define MAXDEPTH ${this.currDepth.toString()}
${built}`;

    this.gl.shaderSource(shader, built);
    this.gl.compileShader(shader);

    var compiled = this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS);

    this.label.innerHTML = "";

    if (!compiled) {
      var preNode = document.createElement("pre");
      preNode.style.whiteSpace = "pre-wrap";
      preNode.innerText = `Error compiling shader, log:

${this.gl.getShaderInfoLog(shader)}

Source:

${NuPlot.withLines(built)}`;
      this.label.appendChild(preNode);
      console.error("Error compiling shader, source:\n", built);
      console.error("Log:\n", this.gl.getShaderInfoLog(shader));
      return null;
    }
    return compiled;
  }

  static get observedAttributes() {
    return [
      "expr-src",
      "canvas-width",
      "canvas-height",
      "white-lines",
      "completely-real",
    ];
  }
}
