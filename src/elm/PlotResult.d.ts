export type PlotResult = {
    expression: string;
    lines: ColoredShapes[];
};

export type ColoredShapes = {
    color: string,
    lines: Line[]
}

export type Line = {
    x1: number,
    y1: number,
    x2: number,
    y2: number
}
