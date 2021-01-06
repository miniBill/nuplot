import { throws } from "assert";
import declarations from "../shaders/declarations.frag";

declare class ClipboardItem {
  constructor(data: { [mimeType: string]: Blob });
}
declare global {
  interface Clipboard {
    write(items: ClipboardItem[]): void;
  }
}

type Point = {
  x: number;
  y: number;
};

export class NuPlot extends HTMLElement {
  wrapper: HTMLElement;
  label: HTMLElement;
  canvas: HTMLCanvasElement;

  gl: WebGLRenderingContext | null = null;
  program: WebGLProgram | null = null;
  fragment_shader: WebGLShader | null = null;

  src = "void main() { gl_FragColor = vec4(0); }";

  whiteLines = 6;
  completelyReal = 0;
  is3D = 0;
  minIterations = 25;
  currIterations = 400;
  maxIterations = 400;

  hadPointersDown = false;
  pendingRequestAnimationFrame = -1;
  pendingTimeout = -1;

  pointers: { [pointerId: number]: Point } = {};
  originalPointers: { [pointerId: number]: Point } = {};

  phi = 0;
  theta = 0;
  originalPhi = 0;
  originalTheta = 0;

  center!: Point;
  viewportWidth!: number;

  originalCenter!: Point;
  originalViewportWidth!: number;

  constructor() {
    super();

    this.resetZoom();

    // Create a shadow root
    const shadowRoow = this.attachShadow({ mode: "open" }); // sets and returns 'this.shadowRoot'

    this.wrapper = document.createElement("div");
    this.wrapper.style.position = "relative";

    this.label = this.wrapper.appendChild(document.createElement("div"));

    this.canvas = this.wrapper.appendChild(document.createElement("canvas"));
    this.initCanvas();

    const saveButton = this.wrapper.appendChild(document.createElement("div"));
    saveButton.innerText = "💾";
    saveButton.style.position = "absolute";
    saveButton.style.top = "10px";
    saveButton.style.right = "10px";
    saveButton.style.cursor = "pointer";
    saveButton.onclick = this.onSaveButtonClick.bind(this);

    if (typeof ClipboardItem !== "undefined") {
      const copyButton = this.wrapper.appendChild(
        document.createElement("div")
      );
      copyButton.innerText = "⎘";
      copyButton.style.position = "absolute";
      copyButton.style.top = "7px";
      copyButton.style.right = "40px";
      copyButton.style.cursor = "pointer";
      copyButton.style.padding = "2px 5px";
      copyButton.style.background = "white";
      copyButton.onclick = this.onCopyButtonClick.bind(this);
    }

    // attach the created elements to the shadow DOM
    shadowRoow.append(this.wrapper);
  }

  onSaveButtonClick(ev: MouseEvent) {
    this.withFilledCanvas(true, () => {
      const name = window.prompt("Exported image name:", "export.png");
      if (!name) return;

      const dataUrl = this.canvas.toDataURL();
      var downloadElement = document.createElement("a");
      downloadElement.setAttribute("href", dataUrl);
      downloadElement.setAttribute("download", name);
      downloadElement.click();
    });
  }

  onCopyButtonClick(ev: MouseEvent) {
    this.withFilledCanvas(false, () =>
      this.canvas.toBlob((blob) => {
        if (blob != null)
          navigator.clipboard.write([new ClipboardItem({ "image/png": blob })]);
      })
    );
  }

  withFilledCanvas(changeSize: boolean, callback: () => void) {
    let oldWidth = 0;
    let oldHeight = 0;

    if (changeSize) {
      const widthString = window.prompt("Exported image width:", "1920");

      if (!widthString) return;
      const heightString = window.prompt("Exported image height:", "1080");

      if (!heightString) return;

      oldWidth = this.canvas.width;
      oldHeight = this.canvas.height;
      this.canvas.width = +(widthString || 1920);
      this.canvas.height = +(heightString || 1080);
    }

    const oldIterations = this.currIterations;
    this.currIterations = this.maxIterations;

    this.reloadFragmentShader(this.buildFragmentShader(this.src));
    this.render();
    callback();

    if (changeSize) {
      this.canvas.width = oldWidth;
      this.canvas.height = oldHeight;

      this.render();
    }

    this.currIterations = oldIterations;
    this.reloadFragmentShader(this.buildFragmentShader(this.src));
  }

  resetZoom() {
    this.center = this.originalCenter = { x: 0, y: 0 };
    this.viewportWidth = this.originalViewportWidth = 2 * Math.PI;
    this.phi = this.originalPhi = 0;
    this.theta = this.originalTheta = 0;
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
      if (process.env.NODE_ENV === "development")
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
    this.canvas.onpointerdown = (e) => this.canvasOnPointerDown(e);
    this.canvas.oncontextmenu = () => false;
    this.canvas.onpointerup = (e) => this.canvasOnPointerUp(e);
    this.canvas.onpointercancel = (e) => this.canvasOnPointerUp(e);
    this.canvas.onpointermove = (e) => this.canvasOnPointerMove(e);
    this.canvas.onwheel = (e) => this.canvasOnWheel(e);
    this.canvas.ondblclick = () => this.resetZoom();
    this.canvas.style.touchAction = "none";

    /* display initial frame */
    this.renderOnAnimationFrame();
  }

  private buildFragmentShader(expr: string) {
    const res = `${declarations}\n${expr}`;
    return res;
  }

  compileAndAttachShader(type: number, src: string): WebGLShader | null {
    if (!this.gl || !this.program) return null;
    const shader = this.gl.createShader(type);

    if (shader == null) {
      if (process.env.NODE_ENV === "development")
        console.error("invalid shader");
      return null;
    }

    if (!this.sourceAndCompile(shader, src)) return null;

    this.gl.attachShader(this.program, shader);

    return shader;
  }

  fromScreen(x: number, y: number): Point {
    return {
      x: NuPlot.project(x, 0, this.canvas.width, this.minx(), this.maxx()),
      y: NuPlot.project(y, 0, this.canvas.height, this.miny(), this.maxy()),
    };
  }

  minx(): number {
    return this.center.x - this.viewportWidth / 2.0;
  }

  maxx(): number {
    return this.center.x + this.viewportWidth / 2.0;
  }

  miny(): number {
    return this.center.y - this.viewportHeight / 2.0;
  }

  maxy(): number {
    return this.center.y - this.viewportHeight / 2.0;
  }

  get viewportHeight() {
    return (this.viewportWidth / this.canvas.width) * this.canvas.height;
  }

  static project(
    x: number,
    froml: number,
    fromu: number,
    tol: number,
    tou: number
  ): number {
    return ((x - froml) / (fromu - froml)) * (tou - tol) + tol;
  }

  canvasOnWheel(e: WheelEvent): boolean {
    // newCenter.x == oldCenter.x + oldWidth * (1 - k) * (e.x / canW - 1 / 2)
    // newMiny - oldMiny == (e.y / canH) * (oldHeight * (1 - k))
    // (e.y / canH) * (k * oldHeight) + newMiny == (e.y / canH) * oldHeight + oldMiny

    const k = 1 + e.deltaY * 0.003;

    this.center = {
      x:
        this.center.x +
        this.viewportWidth * (1 - k) * (e.offsetX / this.canvas.width - 0.5),
      y:
        this.center.y +
        this.viewportHeight * (1 - k) * (0.5 - e.offsetY / this.canvas.height),
    };

    this.viewportWidth *= k;
    this.renderOnAnimationFrame(true);
    // Intercept it
    return false;
  }

  canvasOnPointerDown(e: PointerEvent) {
    this.resetOriginal();

    if (e.buttons == 4) {
      // central wheel
      this.resetZoom();
    } else {
      this.canvas.setPointerCapture(e.pointerId);
      this.pointers[e.pointerId] = this.originalPointers[e.pointerId] = {
        x: e.offsetX,
        y: e.offsetY,
      };
    }
    this.renderOnAnimationFrame(true);
    return true;
  }

  canvasOnPointerMove(e: PointerEvent) {
    if (!(e.pointerId in this.originalPointers))
      // Central mouse button
      return;

    const original = this.originalPointers[e.pointerId];
    const pointer = { x: e.offsetX, y: e.offsetY };
    this.pointers[e.pointerId] = pointer;

    if (this.is3D) {
      // TODO: 3D movement
    } else {
      switch (Object.keys(this.originalPointers).length) {
        case 1:
          const x =
            this.originalCenter.x +
            ((original.x - pointer.x) / this.canvas.width) * this.viewportWidth;
          const y =
            this.originalCenter.y +
            ((pointer.y - original.y) / this.canvas.height) *
              this.viewportHeight;
          this.center = { x: x, y: y };
          break;

        case 2:
          const otherId = +Object.keys(this.pointers).filter(
            (k) => +k != e.pointerId
          )[0];
          if (
            !(otherId in this.originalPointers) ||
            !(otherId in this.pointers)
          ) {
            console.info("Not found???");
            return;
          }
          const otherOriginal = this.originalPointers[otherId];
          const other = this.pointers[otherId];
          this.viewportWidth =
            (this.originalViewportWidth / NuPlot.distance(pointer, other)) *
            NuPlot.distance(original, otherOriginal);

          break;

        case 0:
        default:
          break;
      }
    }
    this.renderOnAnimationFrame(true);
  }

  static distance(l: Point, r: Point): number {
    const dx = l.x - r.x;
    const dy = l.y - r.y;
    return Math.sqrt(dx * dx + dy * dy);
  }

  canvasOnPointerUp(e: PointerEvent) {
    delete this.pointers[e.pointerId];
    this.resetOriginal();
  }

  private resetOriginal() {
    this.originalPhi = this.phi;
    this.originalTheta = this.theta;
    this.originalViewportWidth = this.viewportWidth;
    this.originalCenter = Object.assign({}, this.center);
    this.originalPointers = Object.assign({}, this.pointers);
  }

  renderWithDelay(delay: number) {
    if (this.pendingRequestAnimationFrame >= 0 || this.pendingTimeout >= 0)
      return;
    this.pendingTimeout = window.setTimeout(() => {
      this.pendingTimeout = -1;
      this.pendingRequestAnimationFrame = window.requestAnimationFrame(
        this.maybeRender.bind(this)
      );
    }, delay);
  }

  renderOnAnimationFrame(rightNow: boolean = false) {
    if (this.pendingRequestAnimationFrame >= 0) return;
    if (this.pendingTimeout >= 0)
      if (rightNow) {
        window.clearTimeout(this.pendingTimeout);
        this.pendingTimeout = -1;
      } else {
        return;
      }
    this.pendingRequestAnimationFrame = window.requestAnimationFrame(
      this.maybeRender.bind(this)
    );
  }

  maybeRender() {
    this.pendingRequestAnimationFrame = -1;

    if (this.gl == null || this.program == null) return;

    if (this.hasPointersDown()) {
      if (this.currIterations == this.minIterations) {
        this.render();
      } else {
        this.currIterations = this.minIterations;
        this.reloadFragmentShader(this.buildFragmentShader(this.src));
      }
      this.renderOnAnimationFrame();
      this.hadPointersDown = true;
    } else if (this.currIterations < this.maxIterations) {
      this.render();
      if (!this.hadPointersDown) {
        this.currIterations *= 4;
        this.currIterations = Math.min(this.currIterations, this.maxIterations);
        this.reloadFragmentShader(this.buildFragmentShader(this.src));
      }
      this.renderWithDelay(
        this.currIterations == this.minIterations ? 300 : 100
      );
      this.hadPointersDown = false;
    } else if (this.currIterations >= this.maxIterations) {
      this.render();
      this.currIterations = this.minIterations;
      this.reloadFragmentShader(this.buildFragmentShader(this.src));
      this.hadPointersDown = false;
    } else {
      this.render();
      this.hadPointersDown = false;
    }
  }

  private hasPointersDown() {
    return Object.keys(this.originalPointers).length > 0;
  }

  render() {
    if (this.gl == null || this.program == null) return;

    this.gl.viewport(0, 0, this.gl.canvas.width, this.gl.canvas.height);

    /* bind inputs & render frame */
    this.uniform1f("u_whiteLines", this.whiteLines);
    this.uniform1f("u_completelyReal", this.completelyReal);
    this.uniform1f("u_viewportWidth", this.viewportWidth);
    this.uniform1f("u_canvasWidth", this.canvas.width);
    this.uniform1f("u_canvasHeight", this.canvas.height);
    this.uniform2f("u_zoomCenter", this.center.x, this.center.y);
    this.uniform1f("u_phi", this.phi);
    this.uniform1f("u_theta", this.theta);

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
        break;

      case "canvas-height":
        if (!newValue) return;
        this.canvas.height = +newValue;
        break;

      case "white-lines":
        if (!newValue) return;
        this.whiteLines = +newValue;
        break;

      case "completely-real":
        if (!newValue) return;
        this.completelyReal = +newValue;
        break;

      case "is-3d":
        if (!newValue) return;
        this.is3D = +newValue;
        break;
    }
    this.renderOnAnimationFrame();
  }

  set exprSrc(newValue: string) {
    if (newValue == this.src) return;
    this.currIterations = this.maxIterations;
    this.src = newValue;
    this.reloadFragmentShader(this.buildFragmentShader(this.src));
    this.renderOnAnimationFrame();
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

    built = `#define MAX_ITERATIONS ${this.currIterations.toString()}
${built}`;
    if (
      process.env.NODE_ENV === "development" &&
      this.currIterations == this.maxIterations &&
      false
    )
      console.info(built);

    this.gl.shaderSource(shader, built);
    this.gl.compileShader(shader);

    var compiled = this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS);

    this.label.innerHTML = "";

    if (!compiled) {
      if (process.env.NODE_ENV === "development") {
        var preNode = document.createElement("pre");
        preNode.style.whiteSpace = "pre-wrap";
        preNode.innerText = `Error compiling shader, log:

${this.gl.getShaderInfoLog(shader)}

  Source:

${NuPlot.withLines(built)}`;
        this.label.appendChild(preNode);
        console.error("Error compiling shader, source:\n", built);
        console.error("Log:\n", this.gl.getShaderInfoLog(shader));
      } else {
        var errorNode = document.createElement("div");
        errorNode.innerText =
          "Error creating graph. Try contacting the author.";
        this.label.appendChild(errorNode);
      }
      return null;
    }
    return compiled;
  }

  static get observedAttributes() {
    return [
      "canvas-width",
      "canvas-height",
      "white-lines",
      "completely-real",
      "is-3d",
    ];
  }
}
