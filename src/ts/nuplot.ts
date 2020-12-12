import declarations from "../shaders/declarations.frag";
import main from "../shaders/main.frag";

export class NuPlot extends HTMLElement {
  wrapper: HTMLElement;

  canvas: HTMLCanvasElement;
  gl: WebGLRenderingContext | null = null;
  program: WebGLProgram | null = null;
  fragment_shader: WebGLShader | null = null;

  src =
    "vec3 pixel(float deltaX, float deltaY, float x, float y) { return vec3(0,0,0); }";

  whiteLines = 6;
  completelyReal = 0;

  /* these hold the state of zoom operation */
  zoom_center!: number[];
  target_zoom_center!: number[];
  viewport_width!: number;
  stop_zooming!: boolean;
  zoom_factor!: number;
  label: HTMLElement;

  constructor() {
    super();

    this.reinit_zoom();

    // Create a shadow root
    const shadowRoow = this.attachShadow({ mode: "open" }); // sets and returns 'this.shadowRoot'

    this.wrapper = document.createElement("div");
    this.label = this.wrapper.appendChild(document.createElement("div"));
    this.canvas = this.wrapper.appendChild(document.createElement("canvas"));

    this.initCanvas();

    // attach the created elements to the shadow DOM
    shadowRoow.append(this.wrapper);
  }

  reinit_zoom() {
    this.zoom_center = [0.0, 0.0];
    this.target_zoom_center = [0.0, 0.0];
    this.viewport_width = 2 * Math.PI;
    this.stop_zooming = true;
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

    const fragment_shader = this.buildFragmentShader(this.src);

    this.program = this.gl.createProgram();

    if (this.program == null) {
      console.error("invalid program");
      return;
    }

    if (!this.compileAndAttachShader(this.gl.VERTEX_SHADER, vertex_shader))
      return;

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
    window.requestAnimationFrame(this.renderFrame.bind(this));
  }

  private buildFragmentShader(expr: string) {
    return `${declarations}\n${expr}\n/* Main */\n${main}`;
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
      this.stop_zooming = false;
      const zoom_speed = 0.02;
      this.zoom_factor = e.buttons & 1 ? 1 - zoom_speed : 1 + zoom_speed;
    }
    this.renderFrame();
    return true;
  }

  canvasOnmousemove(e: MouseEvent) {
    this.target_zoom_center[0] = e.offsetX / this.canvas.width;
    this.target_zoom_center[1] = 1 - e.offsetY / this.canvas.height;

    return true;
  }

  canvasOnmouseup(e: MouseEvent) {
    this.stop_zooming = true;
  }

  renderFrame() {
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

    if (this.stop_zooming) return;

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

    window.requestAnimationFrame(this.renderFrame.bind(this));
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
        window.requestAnimationFrame(this.renderFrame.bind(this));
        break;

      case "canvas-height":
        if (!newValue) return;
        this.canvas.height = +newValue;
        window.requestAnimationFrame(this.renderFrame.bind(this));
        break;

      case "white-lines":
        if (!newValue) return;
        this.whiteLines = +newValue;
        break;

      case "completely-real":
        if (!newValue) return;
        this.completelyReal = +newValue;
        break;

      case "expr-src":
        if (newValue == this.src) return;

        //console.log("src changed");
        //console.info(newValue);
        this.src = newValue;
        this.reloadFragmentShader(this.buildFragmentShader(this.src));
        break;
    }
  }

  reloadFragmentShader(src: string) {
    if (!src || !this.gl || !this.program || !this.fragment_shader) return;

    this.sourceAndCompile(this.fragment_shader, src);

    this.gl.linkProgram(this.program);

    window.requestAnimationFrame(this.renderFrame.bind(this));
  }

  static withLines(input: string): string {
    return input
      .split("\n")
      .map((l, i) => (i + 1).toString().padStart(3) + " " + l)
      .join("\n");
  }

  sourceAndCompile(shader: WebGLShader, built: string) {
    if (!this.gl) return;

    this.gl.shaderSource(shader, built);
    this.gl.compileShader(shader);

    var compiled = this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS);
    if (!compiled) {
      this.label.innerHTML = `<pre>
Error compiling shader, log:
${this.gl.getShaderInfoLog(shader)}

Source:
${NuPlot.withLines(built)}
      </pre>`;
      console.error("Error compiling shader, source:\n", built);
      console.error("Log:\n", this.gl.getShaderInfoLog(shader));
      return null;
    }

    this.label.innerHTML = "";
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
