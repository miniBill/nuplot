
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

    int yfrom = u_canvasHeight; // 562;
    int yto = 0; //556;
    int xfrom = 0; //245;
    int xto = u_canvasWidth; // 255;

    debugFile = fopen("debug.log", "w+");

    printf("P3\n");
    printf("%d %d 255\n", (int)(xto - xfrom), (int)(yfrom - yto));
    for(int y = yfrom - 1; y >= yto; y--)
        for(int x = xfrom; x < xto; x++) {
            gl_FragCoord.xy = vec2(x, y);
            main_();
            dput(gl_FragColor.x);
            dput(gl_FragColor.y);
            dput(gl_FragColor.z);
        }

    fclose(debugFile);

    return 0;
}
