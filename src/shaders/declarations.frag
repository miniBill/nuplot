precision highp float;

uniform float u_whiteLines;
uniform float u_completelyReal;

vec3 hl2rgb(float h, float l)
{
    vec3 rgb = clamp(abs(mod(h*6.0+vec3(0.0,4.0,2.0),6.0)-3.0)-1.0,0.0,1.0);
    return l + (rgb - 0.5) * (1.0 - abs(2.0 * l - 1.0));
}

float thetaDelta(float theta) {
    if(u_whiteLines < 1.0)
        return 100.0;
    float thetaSix = theta * u_whiteLines;
    float thetaNeigh = 0.05;
    return abs(thetaSix - floor(thetaSix + 0.5)) / thetaNeigh;
}
