void main() {
  vec2 uv_centered = gl_FragCoord.xy - vec2(u_canvasWidth / 2.0, u_canvasHeight / 2.0);
  float u_viewportHeight = u_viewportWidth * u_canvasHeight / u_canvasWidth;
  
  vec2 uv = uv_centered / vec2(u_canvasWidth, u_canvasHeight) * vec2(u_viewportWidth, u_viewportHeight);
  vec2 c = u_zoomCenter + uv;
  float x = c.x;
  float y = c.y;
  bool escaped = false;
  int iterations = 0;

  float deltaX = u_viewportWidth / u_canvasWidth;
  float deltaY = u_viewportWidth / u_canvasHeight;
  vec3 px = pixel(deltaX, deltaY, x, y);

  gl_FragColor = vec4(palette(deltaX, deltaY, x, y, px), 1.0);
}
