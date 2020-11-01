export type PlotResult = {
    expression: string;
    lines: ColoredLines[];
};

export type ColoredLines = {
    color: string,
    lines: Line[]
}

export type Line = {
    x1: number,
    y1: number,
    x2: number,
    y2: number
}
