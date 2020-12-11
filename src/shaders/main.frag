precision highp float;

uniform vec2 u_zoomCenter;
uniform float u_viewportWidth;
uniform float u_canvasWidth;
uniform float u_canvasHeight;

float ax(float coord, float delta) {
  return max(0.0, 1.0 - abs(coord/delta));
}

vec3 palette(float deltaX, float deltaY, float x, float y, vec3 pixel) {
    vec3 yax = ax(x, deltaX * 2.0) * vec3(0,1,0);
    vec3 xax = ax(y, deltaY * 2.0) * vec3(1,0,0);
    return max(pixel, max(yax, xax));
}

void main() {
  vec2 uv_centered = gl_FragCoord.xy - vec2(u_canvasWidth / 2.0, u_canvasHeight / 2.0);
  float u_viewportHeight = u_viewportWidth * u_canvasHeight / u_canvasWidth;
  
  vec2 canvasSize = vec2(u_canvasWidth, u_canvasHeight);
  vec2 viewportSize = vec2(u_viewportWidth, u_viewportHeight);
  vec2 uv = uv_centered / canvasSize * viewportSize;
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
