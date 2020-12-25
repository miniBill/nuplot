#include <sys/stat.h>

#include "head.h"
#include "out/tracer.cpp"

#undef main

void dput(double d) {
    int tp = clamp((int)(d * 255), 0, 255);
    printf("%d ", tp);
}

int main () {
    u_viewportWidth = 2 * M_PI;
    u_canvasWidth = 400;
    u_canvasHeight = 300;
    u_zoomCenter = vec2(0, 0);

    int traceYFrom = 271;
    int traceYTo = 273;
    int traceXFrom = 194;
    int traceXTo = 195;

    bool onlyTraced = false;
    int xfrom = onlyTraced ? traceXFrom : 0;
    int xto = onlyTraced ? traceXTo : u_canvasWidth;
    int yfrom = onlyTraced ? traceYFrom : 0;
    int yto = onlyTraced ? traceYTo : u_canvasHeight;

    mkdir("out", 0755); // Purposefully ignore output
    debugFile = fopen("out/debug.log", "w+");
    if(!debugFile) {
        fprintf(stderr, "Cannot open debug file!\n");
        return 1;
    }

    printf("P3\n");
    printf("%d %d 255\n", (int)(xto - xfrom), (int)(yto - yfrom));
    for(int y = yto - 1; y >= yfrom; y--)
        for(int x = xfrom; x < xto; x++) {
            gl_FragCoord.xy = vec2(x, y);
            trace = x >= traceXFrom && x < traceXTo && y >= traceYFrom && y < traceYTo;
            if(trace) fprintf(debugFile, "--[%d, %d]--\n", x, y);
            if(
                ((x == traceXFrom - 1 || x == traceXTo) && y >= traceYFrom - 1 && y <= traceYTo)
                ||
                ((y == traceYFrom - 1 || y == traceYTo) && x >= traceXFrom - 1 && x <= traceXTo)
            )
                gl_FragColor = vec4(1,0,0,0);
            else
                main_();
            if(trace)
                fprintf(debugFile, "{%d,%d,%d}\n",
                    int(255 * gl_FragColor.x),
                    int(255 * gl_FragColor.y),
                    int(255 * gl_FragColor.z));
            dput(gl_FragColor.x);
            dput(gl_FragColor.y);
            dput(gl_FragColor.z);
        }

    fclose(debugFile);

    return 0;
}
