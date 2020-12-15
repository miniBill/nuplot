
#undef main

void dput(double d) {
    int tp = clamp((int)(d * 255), 0, 255);
    printf("%d ", tp);
}

int main () {
    u_viewportWidth = 2 * 3.14159;
    u_canvasWidth = 898;
    u_canvasHeight = 673;
    u_zoomCenter = vec2(0, 0);

    int traceYFrom = 557;
    int traceYTo = 559;
    int traceXFrom = 250;
    int traceXTo = 251;

    bool onlyTraced = true;
    int xfrom = onlyTraced ? traceXFrom : 0;
    int xto = onlyTraced ? traceXTo : u_canvasWidth;
    int yfrom = onlyTraced ? traceYFrom : u_canvasHeight;
    int yto = onlyTraced ? traceYTo : 0;

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
