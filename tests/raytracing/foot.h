
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

    int traceYFrom = 287;
    int traceYTo = 289;
    int traceXFrom = 284;
    int traceXTo = 286;

    bool onlyTraced = false;
    int xfrom = onlyTraced ? traceXFrom : 0;
    int xto = onlyTraced ? traceXTo : u_canvasWidth;
    int yfrom = onlyTraced ? traceYFrom : 0;
    int yto = onlyTraced ? traceYTo : u_canvasHeight;

    debugFile = fopen("debug.log", "w+");

    printf("P3\n");
    printf("%d %d 255\n", (int)(xto - xfrom), (int)(yto - yfrom));
    for(int y = yto - 1; y >= yfrom; y--)
        for(int x = xfrom; x < xto; x++) {
            gl_FragCoord.xy = vec2(x, y);
            trace = x >= traceXFrom && x < traceXTo && y >= traceYFrom && y < traceYTo;
            if(trace) fprintf(debugFile, "--[%d, %d]--\n", x, y);
            main_();
            dput(gl_FragColor.x);
            dput(gl_FragColor.y);
            dput(gl_FragColor.z);
        }

    fclose(debugFile);

    return 0;
}
