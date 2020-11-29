export class NuPlot extends HTMLElement {
  wrapper: HTMLElement;

  canvas: HTMLCanvasElement;
  gl: WebGLRenderingContext | null = null;
  program: WebGLProgram | null = null;
  fragment_shader: WebGLShader | null = null;

  src =
    "vec3 pixel(float deltaX, float deltaY, float x, float y) { return vec3(0,0,0); }";

  /* these hold the state of zoom operation */
  zoom_center!: number[];
  target_zoom_center!: number[];
  zoom_size!: number;
  stop_zooming!: boolean;
  zoom_factor!: number;

  constructor() {
    super();

    this.reinit_zoom();

    // Create a shadow root
    const shadowRoow = this.attachShadow({ mode: "open" }); // sets and returns 'this.shadowRoot'

    this.wrapper = document.createElement("div");

    this.canvas = this.wrapper.appendChild(document.createElement("canvas"));

    this.initCanvas();

    // attach the created elements to the shadow DOM
    shadowRoow.append(this.wrapper);
  }

  reinit_zoom() {
    this.zoom_center = [0.0, 0.0];
    this.target_zoom_center = [0.0, 0.0];
    this.zoom_size = 2 * Math.PI;
    this.stop_zooming = true;
    this.zoom_factor = 5.0;
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

    /* display initial frame */
    window.requestAnimationFrame(this.renderFrame.bind(this));
  }

  private buildFragmentShader(expr: string) {
    return `precision highp float;

    uniform vec2 u_zoomCenter;
    uniform float u_zoomSize;
    uniform float u_canvasWidth;
    uniform float u_canvasHeight;

    vec2 i(){
      return vec2(0,1);
    }

    vec2 by(vec2 a, vec2 b) {
      return vec2(a.x*b.x-a.y*b.y, a.x*b.y+a.y*b.x);
    }

    vec2 conj(vec2 a) {
      return vec2(a.x,-a.y);
    }

    vec2 div(vec2 a, vec2 b) {
      return vec2(((a.x*b.x+a.y*b.y)/(b.x*b.x+b.y*b.y)),((a.y*b.x-a.x*b.y)/(b.x*b.x+b.y*b.y)));
    }

    vec2 to_polar(vec2 c) {
      return vec2(sqrt(c.x*c.x+c.y*c.y), atan(c.y, c.x));
    }

    vec2 cexp(vec2 v) {
      return vec2(cos(v.y) * exp(v.x), sin(v.y) * exp(v.x));
    }

    vec2 clog(vec2 b) {
      vec2 polar_b = to_polar(b);
      return vec2(log(polar_b.x), polar_b.y);
    }

    vec2 cpow(vec2 b, vec2 e) {
      return cexp(by(clog(b), e));
    }

    float sinh(float x) {
      return 0.5 * (exp(x) - exp(-x));
    }

    float cosh(float x) {
      return 0.5 * (exp(x) + exp(-x));
    }

    vec2 csin(vec2 z) {
      return vec2(sin(z.x) * cosh(z.y), cos(z.x) * sinh(z.y));
    }

    vec2 ccos(vec2 z) {
      return vec2(cos(z.x) * cosh(z.y), sin(z.x) * sinh(z.y));
    }

    vec2 ctan(vec2 z) {
      return div(csin(z),ccos(z));
    }

    vec2 csinh(vec2 z) {
      return -(by(i(), csin(by(i(), z))));
    }

    vec2 ccosh(vec2 z) {
      return ccos(by(i(), z));
    }

    ${expr}

    vec3 palette(float x, float y) {
        float deltaX = u_zoomSize / u_canvasWidth;
        float deltaY = u_zoomSize / u_canvasHeight;
        vec3 white = pixel(deltaX, deltaY, x, y);
        vec3 yax = (x * (x - deltaX)) < 0.0 ? vec3(1,0,0) : vec3(0,0,0);
        vec3 xax = (y * (y - deltaY)) < 0.0 ? vec3(0,1,0) : vec3(0,0,0);
        return max(white, max(yax, xax));
    }
    void main() {
      float minsize = min(u_canvasWidth, u_canvasHeight);
      vec2 uv_centered = gl_FragCoord.xy - vec2(u_canvasWidth / 2.0, u_canvasHeight / 2.0);
      vec2 uv = uv_centered / vec2(minsize, minsize);
      vec2 c = u_zoomCenter - vec2(0.0,0.0) + uv * u_zoomSize;
      float x = c.x;
      float y = c.y;
      bool escaped = false;
      int iterations = 0;
      gl_FragColor = vec4(palette(x, y),1.0);
    }`;
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
      var x_part = e.offsetX / this.canvas.width;
      var y_part = e.offsetY / this.canvas.height;
      this.target_zoom_center[0] =
        this.zoom_center[0] - this.zoom_size / 2.0 + x_part * this.zoom_size;
      this.target_zoom_center[1] =
        this.zoom_center[1] + this.zoom_size / 2.0 - y_part * this.zoom_size;
      this.stop_zooming = false;
      const zoom_speed = 0.02;
      this.zoom_factor = e.buttons & 1 ? 1 - zoom_speed : 1 + 2 * zoom_speed;
    }
    this.renderFrame();
    return true;
  }

  canvasOnmouseup(e: MouseEvent) {
    this.stop_zooming = true;
  }

  renderFrame() {
    if (this.gl == null || this.program == null) return;

    this.gl.viewport(0, 0, this.gl.canvas.width, this.gl.canvas.height);

    /* bind inputs & render frame */
    this.uniform1f("u_zoomSize", this.zoom_size);
    this.uniform1f("u_canvasWidth", this.canvas.width);
    this.uniform1f("u_canvasHeight", this.canvas.height);
    this.uniform2f("u_zoomCenter", this.zoom_center[0], this.zoom_center[1]);

    this.gl.clearColor(0.0, 0.0, 0.0, 1.0);
    this.gl.clear(this.gl.COLOR_BUFFER_BIT);
    this.gl.drawArrays(this.gl.TRIANGLES, 0, 3);

    if (this.stop_zooming) return;

    this.zoom_size *= this.zoom_factor;

    /* move zoom center towards target */
    this.zoom_center[0] +=
      0.1 * (this.target_zoom_center[0] - this.zoom_center[0]);
    this.zoom_center[1] +=
      0.1 * (this.target_zoom_center[1] - this.zoom_center[1]);

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
        break;

      case "canvas-height":
        if (!newValue) return;
        this.canvas.height = +newValue;
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

  sourceAndCompile(shader: WebGLShader, built: string) {
    if (!this.gl) return;

    this.gl.shaderSource(shader, built);
    this.gl.compileShader(shader);

    var compiled = this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS);
    if (!compiled) {
      console.error("Error compiling shader");
      console.error("Source:\n", built);
      console.error("Log:\n", this.gl.getShaderInfoLog(shader));
      return null;
    }
    return compiled;
  }

  static get observedAttributes() {
    return ["expr-src", "canvas-width", "canvas-height"];
  }
}
