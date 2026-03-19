"use strict";
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class BrushSlider {
        constructor(scatterPlotMatrix, xOriented) {
            this.xOriented = false;
            this.dimIndexScale = d3.scalePoint();
            this.dimIndexScaleInvertFn = d3.scaleQuantize();
            this.inSelectionDrag = false;
            this.scatterPlotMatrix = scatterPlotMatrix;
            this.xOriented = xOriented;
            this.sliderClass = this.xOriented ? "xSlider" : "ySlider";
            d3.select(scatterPlotMatrix.bindto + " .MultiPlot svg").append("g")
                .attr("class", this.sliderClass);
        }
        update() {
            this.updateDimIndexScale();
            d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .axisGroup`).remove();
            d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .brushDim`).remove();
            if (this.xOriented) {
                this.buildChainGroup();
            }
            // Create the slider axis
            const axisGenerator = this.xOriented ? d3.axisBottom(this.dimIndexScale) : d3.axisRight(this.dimIndexScale);
            const axis = d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass}`).append("g")
                .attr("pointer-events", "none")
                .attr("class", "axisGroup")
                // Tick Values set to none to have no overlayed names
                .call(axisGenerator.tickSize(0).tickFormat(() => ""));
            const dx = this.xOriented ? spm.ScatterPlotMatrix.margin.l : spm.ScatterPlotMatrix.margin.l + spm.ScatterPlotMatrix.margin.r + this.scatterPlotMatrix.width - 16;
            const dy = this.xOriented ? spm.ScatterPlotMatrix.margin.t / 4.0 : spm.ScatterPlotMatrix.margin.t;
            d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass}`)
                .attr("transform", `translate(${dx}, ${dy})`);
            this.createBrush();
            axis.append("line")
                .attr("class", "locatorLine")
                .style("display", "none")
                .attr("pointer-events", "none");
            // Adapt slider to dimensions
            this.adjustBrushSelection();
        }
        buildChainGroup() {
            const thisBS = this;
            d3.select(`${this.scatterPlotMatrix.bindto} .chainGroup`).remove();
            const chainGroup = d3.select(this.scatterPlotMatrix.bindto + " .MultiPlot svg").append("g")
                .attr("class", "chainGroup")
                .attr("transform", `translate(${spm.ScatterPlotMatrix.margin.l + spm.ScatterPlotMatrix.margin.r + this.scatterPlotMatrix.width - 16}, ${spm.ScatterPlotMatrix.margin.t / 4.0})`)
                .on("mousedown", function () {
                chainGroup.classed("mousedown", true);
                d3.select(window).on("mouseup", function () { chainGroup.classed("mousedown", false); });
            })
                .on("click", function () {
                thisBS.scatterPlotMatrix.brushSlidersLinked = !thisBS.scatterPlotMatrix.brushSlidersLinked;
                chainGroup.select("path")
                    .attr("transform", `translate(0, ${thisBS.scatterPlotMatrix.brushSlidersLinked ? 0 : -2})`);
                const begin = thisBS.scatterPlotMatrix.xStartingDimIndex;
                const end = thisBS.scatterPlotMatrix.xStartingDimIndex + thisBS.scatterPlotMatrix.visibleDimCount - 1;
                thisBS.scatterPlotMatrix.updateVisibleDimensions(begin, end, thisBS.xOriented);
                thisBS.adjustOtherBrushSelection();
            });
            chainGroup.append("path")
                .attr("d", "M-3,0L-3,-6A3,3 0 0,1 3,-6L3,-3")
                .attr("transform", `translate(0, ${thisBS.scatterPlotMatrix.brushSlidersLinked ? 0 : -2})`);
            chainGroup.append("rect")
                .attr("x", -5)
                .attr("y", -2)
                .attr("width", 10)
                .attr("height", 9);
        }
        updateDimIndexScale() {
            const spData = this.scatterPlotMatrix.spData;
            const size = this.xOriented ? this.scatterPlotMatrix.width : this.scatterPlotMatrix.height;
            this.dimIndexScale
                .domain(d3.range(spData.dimensions.length))
                .range([0, size]);
            this.dimIndexScaleInvertFn
                .domain([0, size])
                .range(d3.range(spData.dimensions.length));
        }
        centerBrush(indexCenter, moveBrush) {
            const spData = this.scatterPlotMatrix.spData;
            const sizeDimVisible = this.scatterPlotMatrix.visibleDimCount;
            let sizeLeft = Math.round((sizeDimVisible - 1) / 2.0);
            let sizeRight = sizeDimVisible - 1 - sizeLeft;
            if (indexCenter - sizeLeft < 0) {
                sizeRight = sizeRight + (sizeLeft - indexCenter);
                sizeLeft = indexCenter;
            }
            if (indexCenter + sizeRight > spData.dimensions.length - 1) {
                sizeLeft = sizeLeft + (indexCenter + sizeRight - spData.dimensions.length + 1);
                sizeRight = spData.dimensions.length - 1 - indexCenter;
            }
            const begin = indexCenter - sizeLeft;
            const end = indexCenter + sizeRight;
            if (begin !== this.startingDimIndex() ||
                end !== this.startingDimIndex() + this.scatterPlotMatrix.visibleDimCount - 1) {
                this.scatterPlotMatrix.updateVisibleDimensions(begin, end, this.xOriented);
                d3.select(this.scatterPlotMatrix.bindto + " .mspTooltip").style("display", "none");
            }
            if (moveBrush) {
                this.adjustBrushSelection();
                this.adjustOtherBrushSelection();
            }
        }
        mouseDown(mouse) {
            const dimIndex = this.dimIndexScaleInvertFn(mouse[this.xOriented ? 0 : 1]);
            if (dimIndex) {
                this.centerBrush(dimIndex, true);
                d3.event.stopPropagation();
            }
        }
        mouseMove(mouse) {
            const dimIndex = this.dimIndexScaleInvertFn(mouse[this.xOriented ? 0 : 1]);
            if (dimIndex !== undefined) {
                const line = this.xOriented ? [[mouse[0], -8], [mouse[0], 8]] : [[-8, mouse[1]], [8, mouse[1]]];
                d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .locatorLine`)
                    .style("display", null)
                    .attr("x1", line[0][0])
                    .attr("y1", line[0][1])
                    .attr("x2", line[1][0])
                    .attr("y2", line[1][1]);
                const mspDivNode = d3.select(this.scatterPlotMatrix.bindto + " .mspDiv").node();
                const parentBounds = (mspDivNode === null) ? null : mspDivNode.getBoundingClientRect();
                const xParent = (parentBounds === null) ? 0 : parentBounds.x;
                const yParent = (parentBounds === null) ? 0 : parentBounds.y;
                const overlayNode = d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .overlay`).node();
                const overlayBound = (overlayNode === null) ? null : overlayNode.getBoundingClientRect();
                const xOverlay = (overlayBound === null) ? 0 : overlayBound.x;
                const yOverlay = (overlayBound === null) ? 0 : overlayBound.y;
                d3.select(this.scatterPlotMatrix.bindto + " .mspTooltip").remove();
                const mspDiv = d3.select(this.scatterPlotMatrix.bindto + " .mspDiv");
                const dx = this.xOriented ? -xParent + mouse[0] + 15 : -xParent - 15;
                const dy = this.xOriented ? -yParent - 15 : -yParent + mouse[1] + 15;
                const tooltip = mspDiv.append("div")
                    .attr("class", "mspTooltip")
                    .style("display", "block")
                    .style("left", (xOverlay + dx) + "px")
                    .style("top", (yOverlay + dy) + "px");
                tooltip.append("div")
                    .html(this.scatterPlotMatrix.spData.columns[this.scatterPlotMatrix.spData.dimensions[dimIndex]].label);
            }
        }
        mouseExit() {
            d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .locatorLine`).style("display", "none");
            d3.select(this.scatterPlotMatrix.bindto + " .mspTooltip").style("display", "none");
        }
        createBrush() {
            const thisBS = this;
            this.inSelectionDrag = false;
            d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass}`).append("g")
                .attr("class", "brushDim")
                // Call 'd3.brushX/Y()' to create the SVG elements necessary to display the brush selection and to receive input events for interaction
                .call(this.brushBehavior())
                // Listen mouve events of 'overlay' group to center brush (if clicked) or show a tooltip
                .call(g => g.select(".overlay")
                // @ts-ignore
                .on("mousedown touchstart", function () { thisBS.mouseDown(d3.mouse(this)); })
                // @ts-ignore
                .on("mousemove", function () { thisBS.mouseMove(d3.mouse(this)); })
                // @ts-ignore
                .on("mouseout", function () { thisBS.mouseExit(d3.mouse(this)); }))
                // Listen mouve events of 'selection' group to update 'drag' flag
                .call(g => g.select(".selection")
                .on("mousedown", function () { thisBS.inSelectionDrag = true; })
                .on("mouseup", function () { thisBS.inSelectionDrag = false; }));
        }
        brushBehavior() {
            const thisBS = this;
            return this.buildBrushBehavior()
                .handleSize(4)
                // Set brushable area
                .extent(this.xOriented
                ? [
                    [0, -10],
                    [thisBS.scatterPlotMatrix.width, 10]
                ]
                : [
                    [-10, 0],
                    [10, thisBS.scatterPlotMatrix.height]
                ])
                // When the brush moves (such as on mousemove), brush is dragged or a brush bound is moved
                .on("brush", function () {
                const selection = d3.event.selection;
                if (thisBS.inSelectionDrag) {
                    // if brush is dragged, use 'centerBrush' to keep unchanged the number of selected columns
                    const brushCenter = (selection[0] + selection[1]) / 2.0;
                    const centerIndex = thisBS.dimIndexScaleInvertFn(brushCenter);
                    if (centerIndex) {
                        thisBS.centerBrush(centerIndex, false);
                    }
                }
                else {
                    const begin = thisBS.dimIndexScaleInvertFn(selection[0]);
                    const end = thisBS.dimIndexScaleInvertFn(selection[1]);
                    if (begin !== thisBS.startingDimIndex() ||
                        end !== thisBS.startingDimIndex() + thisBS.scatterPlotMatrix.visibleDimCount - 1) {
                        thisBS.scatterPlotMatrix.updateVisibleDimensions(begin, end, thisBS.xOriented);
                    }
                }
                thisBS.adjustOtherBrushSelection();
            })
                // At the end of a brush gesture (such as on mouseup), adjust brush selection
                .on("end", function () {
                thisBS.inSelectionDrag = false;
                thisBS.adjustBrushSelection();
                thisBS.adjustOtherBrushSelection();
            });
        }
        adjustBrushSelection() {
            // Adjust brush selection (to make it corresponds to the limits of the bands associated to each column)
            d3.select(`${this.scatterPlotMatrix.bindto} .${this.sliderClass} .brushDim`).call(this.buildBrushBehavior().move, [
                this.dimIndexScale(this.startingDimIndex()),
                this.dimIndexScale(this.startingDimIndex() + this.scatterPlotMatrix.visibleDimCount - 1)
            ]);
        }
        adjustOtherBrushSelection() {
            // Adjust selection of the other brush
            const otherVisibleDimIndex = this.xOriented ? this.scatterPlotMatrix.yStartingDimIndex : this.scatterPlotMatrix.xStartingDimIndex;
            const otherBrushBehavior = this.xOriented ? d3.brushY() : d3.brushX();
            const otherSlideClass = this.xOriented ? "ySlider" : "xSlider";
            d3.select(`${this.scatterPlotMatrix.bindto} .${otherSlideClass} .brushDim`).call(otherBrushBehavior.move, [
                this.dimIndexScale(this.scatterPlotMatrix.brushSlidersLinked ? this.startingDimIndex() : otherVisibleDimIndex),
                this.dimIndexScale(otherVisibleDimIndex + this.scatterPlotMatrix.visibleDimCount - 1)
            ]);
        }
        // private equals(array1: Array<any>, array2: Array<any>) {
        //     return array1.length === array2.length && array1.every((value, index) => value === array2[index]);
        // }
        startingDimIndex() {
            return this.xOriented ? this.scatterPlotMatrix.xStartingDimIndex : this.scatterPlotMatrix.yStartingDimIndex;
        }
        buildBrushBehavior() {
            return this.xOriented ? d3.brushX() : d3.brushY();
        }
    }
    spm.BrushSlider = BrushSlider;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class Column {
        constructor(dim, dimIndex, spData, label, categories, ioType) {
            this.dimIndex = dimIndex;
            this.dim = dim;
            this.label = label;
            this.categories = categories;
            this.myDomain = d3.extent(spData.sampleData, function (row) { return +row[dim]; });
            this.ioType = ioType;
            const data = spData.sampleData.map(function (row) { return row[dim]; });
            this.sd = d3.deviation(data);
            const sorteddata = data.filter(d => d !== null && !isNaN(d)).sort(d3.ascending);
            this.p25 = d3.quantile(sorteddata, 0.25);
            this.p75 = d3.quantile(sorteddata, 0.75);
        }
        domain() {
            if (typeof this.myDomain[0] === "undefined" || typeof this.myDomain[1] === "undefined") {
                console.error("Wrong domain for ", this.dim);
                return [0, 1];
            }
            if (this.categories === null) {
                return this.myDomain;
            }
            else {
                return [this.myDomain[0] - 0.4, this.myDomain[1] + 0.6];
            }
        }
        formatedRowValue(row) {
            return this.formatedValue(row[this.dim]);
        }
        formatedValue(value) {
            if (this.categories) {
                if (value >= 0 && value < this.categories.length) {
                    return Number.isInteger(value.valueOf()) ? this.categories[value.valueOf()].toString() : "";
                }
                console.warn(value, " is not valid, it should be between 0 and ", this.categories.length);
                return "";
            }
            else {
                return spm.ExpFormat.format(value);
            }
        }
        axisTicks() {
            if (this.categories) {
                return this.categories.length;
            }
            else {
                return 4;
            }
        }
        labelText() {
            return this.label.replace(/<br>/gi, " ");
        }
        isInput() {
            return this.ioType === Column.INPUT;
        }
        isOutput() {
            return this.ioType === Column.OUTPUT;
        }
    }
    Column.INPUT = "Input";
    Column.OUTPUT = "Output";
    spm.Column = Column;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class CorrPlot {
        constructor(spData, config) {
            this.xPlot = 0;
            this.yPlot = 0;
            this.width = 0;
            this.height = 0;
            this.axisVisibility = { xTitle: true, xValues: true, yTitle: true, yValues: true };
            this.repType = CorrPlot.CIRCLES_REP;
            this.spData = spData;
            this.bindto = config.bindto;
            this.index = config.index;
            this.xColumn = spData.columns[spData.dimensions[0]];
            this.yColumn = spData.columns[spData.dimensions[0]];
            this.zColumn = null;
            this.row = config.row;
            this.col = config.col;
            this.corrCsId = config.corrPlotCsId;
            this.categoricalCsId = config.categoricalCsId;
            this.axisVisibility = config.axisVisibility;
            this.repType = config.corrPlotType;
            this.style = config.style;
            this.catColorScale = spm.SpConst.CATEGORIAL_CS[this.categoricalCsId];
        }
        setXColumn(column) {
            this.xColumn = column;
        }
        setYColumn(column) {
            this.yColumn = column;
        }
        setZColumn(column) {
            this.zColumn = column;
        }
        formatXValue(value) {
            return this.xColumn.formatedValue(value);
        }
        formatYValue(value) {
            return this.yColumn.formatedValue(value);
        }
        formatZValue(value) {
            return this.zColumn === null ? "No Z axis" : this.zColumn.formatedValue(value);
        }
        // eslint-disable-next-line max-lines-per-function
        draw(updateType) {
            const thisPlot = this;
            const plotSelection = this.plotSelection();
            plotSelection.select(".corrPlotArea").remove();
            if (this.repType === CorrPlot.EMPTY_REP) {
                return;
            }
            this.updateZScale();
            const areaSelection = plotSelection.append("g")
                .attr("class", "corrPlotArea")
                .attr("transform", "translate(0," + CorrPlot.padding.t + ")");
            const cpBorder = areaSelection.append("rect")
                .attr("class", "cpBorder")
                .attr("width", this.width - CorrPlot.padding.r)
                .attr("height", this.height - CorrPlot.padding.t)
                .on("mouseover", function () {
                thisPlot.spData.dispatch.call(spm.SpData.HL_GRAPH_EVENT, undefined, thisPlot);
            })
                .on("mouseout", function () {
                const coord = d3.mouse(this);
                if (coord[0] < 0 || coord[0] > thisPlot.width
                    || coord[1] < 0 || coord[0] > thisPlot.width) {
                    thisPlot.spData.dispatch.call(spm.SpData.HL_GRAPH_EVENT, undefined, null);
                }
            });
            if (this.xColumn.categories === null && this.yColumn.categories === null) {
                if (this.repType === CorrPlot.CIRCLES_REP) {
                    this.drawCCTreemap(updateType, areaSelection);
                }
                else {
                    this.drawCorrValues(updateType, areaSelection);
                    cpBorder.classed("cpNaBorder", true);
                }
            }
            else {
                this.drawNA(updateType, areaSelection);
                cpBorder.classed("cpNaBorder", true);
            }
            // Compute 'xPlot' and 'yPlot' (useful for canvas)
            const mspDivNode = d3.select(this.bindto + " .mspDiv").node();
            const parentBounds = (mspDivNode === null) ? null : mspDivNode.getBoundingClientRect();
            const xParent = (parentBounds === null) ? 0 : parentBounds.x;
            const yParent = (parentBounds === null) ? 0 : parentBounds.y;
            const spRectNode = cpBorder.node();
            const spRectBounds = (spRectNode === null) ? null : spRectNode.getBoundingClientRect();
            const xSpRect = (spRectBounds === null) ? 0 : spRectBounds.x;
            const ySpRect = (spRectBounds === null) ? 0 : spRectBounds.y;
            this.xPlot = xSpRect - xParent;
            this.yPlot = ySpRect - yParent;
        }
        hlGraph(highlight) {
            const plotSelection = this.plotSelection();
            if (this.xColumn.categories === null && this.yColumn.categories === null) {
                plotSelection.select(".cpBorder")
                    .classed("hlGraph", highlight);
                // plotSelection.select("circle.Root")
                //     .classed("hlGraph", highlight);
            }
            else {
                plotSelection.select(".cpBorder")
                    .classed("hlGraph", highlight);
            }
        }
        plotSelection(plotSelection) {
            if (plotSelection) {
                return plotSelection;
            }
            const thisPlot = this;
            const mspGroup = d3.select(this.bindto + " .mspGroup");
            return mspGroup.selectAll(".corrPlot")
                .filter(function (plot) {
                return plot.row === thisPlot.row && plot.col === thisPlot.col;
            });
        }
        // eslint-disable-next-line max-lines-per-function
        drawCCTreemap(_updateType, areaSelection) {
            const thisPlot = this;
            const corrValues = this.corrValues();
            const packLayout = d3.pack()
                .size([this.width - CorrPlot.padding.r, this.height - CorrPlot.padding.t])
                .padding(2);
            const rootNode = d3.hierarchy(corrValues);
            rootNode.sum(function (_cv) {
                return 1;
            });
            packLayout(rootNode);
            const categoriesCount = (this.zColumn !== null && this.zColumn.categories) ? this.zColumn.categories.length : 0;
            // Draw circles which represent correlation values (values between 0 and 1)
            areaSelection
                .selectAll("circle.corrValues")
                .data(rootNode.descendants())
                .enter()
                .filter(node => categoriesCount !== 0 || node.data.clazz !== "Root")
                .append("circle")
                .style("pointer-events", "none")
                .attr("class", node => "corrValues " + node.data.clazz)
                .attr("cx", node => {
                // @ts-ignore - x property added by 'd3.pack'
                const cx = node.x;
                if (isNaN(cx)) {
                    console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), cx is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
                    return 0;
                }
                return cx;
            })
                .attr("cy", node => {
                // @ts-ignore - x property added by 'd3.pack'
                const cy = node.y;
                if (isNaN(cy)) {
                    console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), cy is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
                    return 0;
                }
                return cy;
            })
                .attr("r", node => {
                // @ts-ignore - r property added by 'd3.pack'
                const r = node.r * Math.abs(node.data.value);
                return isFinite(r) ? r : 0;
            })
                .attr("stroke", "none")
                .filter(node => node.data.clazz !== "Root")
                .attr("fill", function (node) {
                if (node.data.clazz === "All") {
                    return thisPlot.style.plotProperties.noCatColor;
                }
                return thisPlot.catColorScale(node.data.catIndex);
            });
            // Draw circles which represent a value of 1 for correlation values
            areaSelection
                .selectAll("circle.cvBorder")
                .data(rootNode.descendants())
                .enter()
                .filter(node => node.data.clazz !== "Root")
                .append("circle")
                .attr("class", node => "cvBorder " + node.data.clazz)
                .attr("cx", node => {
                // @ts-ignore - x property added by 'd3.pack'
                const cx = node.x;
                if (isNaN(cx)) {
                    if (node.data.keptCount > 1) {
                        console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), cx is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
                    }
                    return 0;
                }
                return cx;
            })
                .attr("cy", node => {
                // @ts-ignore - x property added by 'd3.pack'
                const cy = node.y;
                if (isNaN(cy)) {
                    if (node.data.keptCount > 1) {
                        console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), cy is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
                    }
                    return 0;
                }
                return cy;
            })
                .attr("r", node => {
                // @ts-ignore - r property added by 'd3.pack'
                const r = node.r;
                if (isNaN(r)) {
                    if (node.data.keptCount > 1) {
                        console.error(`corrPlot (${thisPlot.col}, ${thisPlot.row}), r is NaN (data '${node.data.label}' has ${node.data.keptCount} kept values)`);
                    }
                    return 0;
                }
                return r;
            })
                .attr("fill", "none")
                .attr("stroke", categoriesCount ? "black" : "none")
                .on("mouseover", function (node) {
                thisPlot.mouseoverCircle(node);
            })
                .on("mouseout", function () {
                thisPlot.mouseout();
            });
        }
        // eslint-disable-next-line max-lines-per-function
        drawCorrValues(_updateType, areaSelection) {
            const thisPlot = this;
            const corrValues = this.corrValues().children;
            const categoriesCount = (this.zColumn !== null && this.zColumn.categories) ? this.zColumn.categories.length : 0;
            const contColorScale = d3.scaleSequential(spm.SpConst.CONTINUOUS_CS[this.corrCsId])
                .domain(thisPlot.repType === CorrPlot.TEXT_REP ? [-1, 1] : [0, 1]);
            const corrGroup = areaSelection.selectAll("g.corrGroup").data(corrValues).enter()
                .append("g")
                .attr("class", "corrGroup")
                .on("mouseover", function (cv) {
                thisPlot.mouseoverText(cv);
            })
                .on("mouseout", function () {
                thisPlot.mouseout();
            });
            const valueText = corrGroup.append("text")
                .attr("class", "corrNumber")
                .style("fill", function (cv) {
                if (categoriesCount) {
                    return cv.clazz === "All"
                        ? thisPlot.style.plotProperties.noCatColor
                        : thisPlot.catColorScale(cv.catIndex);
                }
                else {
                    const color = contColorScale(thisPlot.repType === CorrPlot.ABS_TEXT_REP
                        ? Math.abs(cv.value)
                        : cv.value);
                    return color !== null && color !== void 0 ? color : spm.SpConst.CONTINUOUS_CS[thisPlot.corrCsId](0);
                }
            });
            if (categoriesCount) {
                const bandScale = d3.scaleBand()
                    .domain(d3.range(categoriesCount + 1))
                    .range([0, this.height - CorrPlot.padding.t])
                    .paddingInner(0.3)
                    .paddingOuter(1);
                corrGroup.append("rect")
                    .attr("class", "corrCat")
                    .attr("x", bandScale.bandwidth() / 2)
                    .attr("y", (_cv, i) => { var _a; return ((_a = bandScale(i)) !== null && _a !== void 0 ? _a : 0); })
                    .attr("width", bandScale.bandwidth())
                    .attr("height", bandScale.bandwidth())
                    .style("fill", cv => cv.clazz === "All"
                    ? "none"
                    : thisPlot.catColorScale(cv.catIndex));
                valueText
                    .attr("x", bandScale.bandwidth() * 2)
                    .attr("y", (_cv, i) => { var _a; return (_a = bandScale(i)) !== null && _a !== void 0 ? _a : 0; })
                    .attr("dy", bandScale.bandwidth() * 0.6)
                    .attr("font-size", bandScale.bandwidth())
                    .attr("dominant-baseline", "middle")
                    .text(cv => `${spm.ExpFormat.format(cv.value)}`);
            }
            else {
                valueText
                    .attr("x", (this.width - CorrPlot.padding.r) / 2)
                    .attr("y", (this.height - CorrPlot.padding.t) / 2)
                    .attr("font-size", (this.height - CorrPlot.padding.t) / 5 + "px")
                    .attr("text-anchor", "middle")
                    .attr("dominant-baseline", "middle")
                    .text(cv => `${spm.ExpFormat.format(cv.value)}`);
            }
        }
        mouseoverText(corrValues) {
            this.spData.dispatch.call(spm.SpData.HL_GRAPH_EVENT, undefined, this);
            const tooltipLocation = this.tooltipLocation();
            d3.select(this.bindto + " .mspTooltip").remove();
            const mspDiv = d3.select(this.bindto + " .mspDiv");
            const tooltip = mspDiv.append("div")
                .attr("class", "mspTooltip")
                .style("display", "block")
                .style("left", tooltipLocation[0] + "px")
                .style("top", tooltipLocation[1] + "px");
            tooltip.append("div")
                .attr("class", "title")
                .html("Correlation Values");
            switch (corrValues.clazz) {
                case "All":
                    this.updateTooltipWithAllCorrelation(corrValues);
                    break;
                case "Cat":
                    this.updateTooltipWithCatCorrelation(corrValues);
                    break;
                default:
                    break;
            }
            this.updateXYTooltip();
        }
        tooltipLocation() {
            const mspDivNode = d3.select(this.bindto + " .mspDiv").node();
            const parentBounds = (mspDivNode === null) ? null : mspDivNode.getBoundingClientRect();
            const xParent = (parentBounds === null) ? 0 : parentBounds.x;
            const plotGroup = d3.select(this.bindto + " .mspGroup").node();
            const elementBounds = (plotGroup === null) ? null : plotGroup.getBoundingClientRect();
            const xRect = (elementBounds === null) ? 0 : elementBounds.x;
            const wRect = (elementBounds === null) ? 0 : elementBounds.width;
            return [xRect - xParent + wRect + 5, this.yPlot];
        }
        drawNA(_updateType, areaSelection) {
            const corrText = "NA";
            areaSelection.append("text")
                .attr("class", "naCorrNumber")
                .attr("x", (this.width - CorrPlot.padding.r) / 2)
                .attr("y", (this.height - CorrPlot.padding.t) / 2)
                .attr("font-size", (this.height - CorrPlot.padding.t) / 5 + "px")
                .attr("text-anchor", "middle")
                .attr("dominant-baseline", "middle")
                .text(corrText);
        }
        // eslint-disable-next-line max-lines-per-function
        corrValues() {
            const corrValues = {
                label: "Root",
                catIndex: NaN,
                clazz: "Root",
                children: [],
                value: NaN,
                pointsCount: NaN,
                keptCount: NaN
            };
            // About Pearson correlation for all points
            const corr = CorrPlot.getPearsonCorrelation(this.spData.cutData().map(row => row[this.xColumn.dim]), this.spData.cutData().map(row => row[this.yColumn.dim]));
            corrValues.children.push({
                label: "All",
                catIndex: NaN,
                clazz: "All",
                children: [],
                value: corr,
                pointsCount: this.spData.sampleData.length,
                keptCount: this.spData.cutData().length
            });
            // About Pearson correlation for each category of points
            let corrCount = 1;
            const zColumn = this.zColumn;
            if (zColumn !== null && zColumn.categories) {
                const categories = zColumn.categories;
                categories.forEach((cat, i) => {
                    const filteredData = this.spData.cutData().filter(row => {
                        const catIndex = row[zColumn.dim];
                        return categories[catIndex] === cat;
                    });
                    const catCorr = CorrPlot.getPearsonCorrelation(filteredData.map(row => row[this.xColumn.dim]), filteredData.map(row => row[this.yColumn.dim]));
                    if (isFinite(catCorr)) {
                        const catData = this.spData.sampleData.filter(row => {
                            const catIndex = row[zColumn.dim];
                            return categories[catIndex] === cat;
                        });
                        corrValues.children.push({
                            label: (i + 1).toString(),
                            catIndex: i,
                            clazz: "Cat",
                            children: [],
                            value: catCorr,
                            pointsCount: catData.length,
                            keptCount: filteredData.length
                        });
                        corrCount = corrCount + 1;
                    }
                });
            }
            return corrValues;
        }
        // eslint-disable-next-line max-lines-per-function
        mouseoverCircle(node) {
            this.spData.dispatch.call(spm.SpData.HL_GRAPH_EVENT, undefined, this);
            const tooltipLocation = this.tooltipLocation();
            d3.select(this.bindto + " .mspTooltip").remove();
            const mspDiv = d3.select(this.bindto + " .mspDiv");
            const tooltip = mspDiv.append("div")
                .attr("class", "mspTooltip")
                .style("display", "block")
                .style("left", tooltipLocation[0] + "px")
                .style("top", tooltipLocation[1] + "px");
            tooltip.append("div")
                .attr("class", "title")
                .html("Correlation Circular Treemap");
            if (typeof node.data === "undefined") {
                return;
            }
            switch (node.data.clazz) {
                case "All":
                    this.updateTooltipWithAllCorrelation(node.data);
                    break;
                case "Cat":
                    this.updateTooltipWithCatCorrelation(node.data);
                    break;
                default:
                    break;
            }
            this.updateXYTooltip();
        }
        updateXYTooltip() {
            const subTipDiv = d3.select(this.bindto + " .mspTooltip").append("div")
                .attr("class", "subTipDiv");
            subTipDiv.append("div")
                .html(`x: ${this.xColumn.label.replace(/<br>/gi, " ")}`);
            subTipDiv.append("div")
                .html(`y: ${this.yColumn.label.replace(/<br>/gi, " ")}`);
        }
        updateTooltipWithAllCorrelation(corrValues) {
            const subTipDiv = d3.select(this.bindto + " .mspTooltip").append("div")
                .attr("class", "subTipDiv");
            subTipDiv.append("div")
                .html(`<span class='swatch' style='background:"none"'>&nbsp;</span> <span class='strongValue'>${spm.ExpFormat.format(corrValues.value)}</span>`);
            const zColumn = this.zColumn;
            if (zColumn !== null && zColumn.categories) {
                subTipDiv.append("div")
                    .html(`Correlation, whatever the value of ${zColumn.label.replace(/<br>/gi, " ")}`);
            }
            subTipDiv.append("div")
                .html(`${corrValues.keptCount} sample points (${corrValues.pointsCount - corrValues.keptCount} filtered points)`);
        }
        updateTooltipWithCatCorrelation(corrValues) {
            const zColumn = this.zColumn;
            if (!zColumn || !zColumn.categories) {
                console.error("'mouseoverCircleCat' called, but Z column is not categorial");
                return;
            }
            const thisPlot = this;
            const subTipDiv = d3.select(this.bindto + " .mspTooltip").append("div")
                .attr("class", "subTipDiv");
            const category = zColumn.categories[corrValues.catIndex];
            const color = thisPlot.catColorScale(corrValues.catIndex);
            subTipDiv.append("div")
                .html(`<span class='swatch' style='background:${color}'>&nbsp;</span> <span class='strongValue'>${spm.ExpFormat.format(corrValues.value)}</span>`);
            subTipDiv.append("div")
                .html(`Correlation when ${zColumn.labelText()} is: ${CorrPlot.brIze(category)}`);
            subTipDiv.append("div")
                .html(`${corrValues.keptCount} sample points (${corrValues.pointsCount - corrValues.keptCount} filtered points)`);
        }
        static brIze(category) {
            const splittedCategory = category.toString().split(", ");
            if (splittedCategory.length > 1) {
                return "<br>" + splittedCategory.join(",<br>") + "<br>";
            }
            return splittedCategory[0];
        }
        mouseout() {
            d3.select(this.bindto + " .mspTooltip").style("display", "none");
        }
        updateZScale() {
            const zColumn = this.zColumn;
            if (zColumn !== null && zColumn.categories !== null) {
                const zMax = zColumn.domain()[1];
                this.catColorScale = spm.SpConst.CATEGORIAL_CS[this.categoricalCsId]
                    .domain(d3.range(zMax));
            }
        }
        static getPearsonCorrelation(x, y) {
            // Source: https://memory.psych.mun.ca/tech/js/correlation.shtml
            let shortestArrayLength = 0;
            if (x.length === y.length) {
                shortestArrayLength = x.length;
            }
            else if (x.length > y.length) {
                shortestArrayLength = y.length;
                console.error("x has more items in it, the last " + (x.length - shortestArrayLength) + " item(s) will be ignored");
            }
            else {
                shortestArrayLength = x.length;
                console.error("y has more items in it, the last " + (y.length - shortestArrayLength) + " item(s) will be ignored");
            }
            let sum_x = 0;
            let sum_y = 0;
            let sum_xy = 0;
            let sum_x2 = 0;
            let sum_y2 = 0;
            for (let i = 0; i < shortestArrayLength; i++) {
                sum_x += x[i];
                sum_y += y[i];
                sum_xy += x[i] * y[i];
                sum_x2 += x[i] * x[i];
                sum_y2 += y[i] * y[i];
            }
            const step1 = (shortestArrayLength * sum_xy) - (sum_x * sum_y);
            const step2 = (shortestArrayLength * sum_x2) - (sum_x * sum_x);
            const step3 = (shortestArrayLength * sum_y2) - (sum_y * sum_y);
            const step4 = Math.sqrt(step2 * step3);
            const answer = step1 / step4;
            return isFinite(answer) ? answer : NaN;
        }
    }
    CorrPlot.EMPTY_REP = "Empty";
    CorrPlot.CIRCLES_REP = "Circles";
    CorrPlot.TEXT_REP = "Text";
    CorrPlot.ABS_TEXT_REP = "AbsText";
    CorrPlot.padding = { r: 10, t: 10 };
    spm.CorrPlot = CorrPlot;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class DiagPlot {
        constructor(spData, config) {
            this.xPlot = 0;
            this.yPlot = 0;
            this.width = 0;
            this.height = 0;
            this.xScale = d3.scaleLinear();
            this.axisVisibility = { xTitle: true, xValues: true, yTitle: true, yValues: true };
            this.xAxis = d3.axisBottom(this.xScale)
                .tickFormat(DiagPlot.prototype.formatXValue.bind(this));
            this.yScale = d3.scaleLinear();
            this.yAxis = d3.axisLeft(this.yScale)
                .tickFormat(spm.ExpFormat.format);
            this.brush = null;
            this.dblClickTimeout = null;
            this.distribPlot = null;
            this.spData = spData;
            this.bindto = config.bindto;
            this.index = config.index;
            this.xColumn = spData.columns[spData.dimensions[0]];
            this.zColumn = spData.columns[spData.dimensions[0]];
            this.row = config.row;
            this.col = config.col;
            this.mouseMode = config.mouseMode;
            this.continuousCsId = config.continuousCsId;
            this.categoricalCsId = config.categoricalCsId;
            this.distribType = config.distribType;
            this.axisVisibility = config.axisVisibility;
            this.contColorScale = d3.scaleSequential(spm.SpConst.CONTINUOUS_CS[this.continuousCsId]);
            this.catColorScale = spm.SpConst.CATEGORIAL_CS[this.categoricalCsId];
            this.style = config.style;
        }
        setXColumn(column) {
            this.xColumn = column;
            this.distribPlot = null;
        }
        setZColumn(column) {
            this.zColumn = column;
        }
        formatXValue(value) {
            return this.xColumn.formatedValue(value);
        }
        formatZValue(value) {
            return this.zColumn === null ? "No Z axis" : this.zColumn.formatedValue(value);
        }
        draw(updateType) {
            this.xScale.range([0, this.width - spm.ScatterPlot.padding.r]);
            this.yScale.range([this.height, spm.ScatterPlot.padding.t]);
            this.updateColorScales();
            const plotSelection = this.plotSelection();
            this.drawSpRect(updateType, plotSelection);
            this.drawDistribPlots(updateType, plotSelection);
            this.updateXScaleDomain();
            this.updateYScaleDomain();
            this.drawXAxis(updateType, plotSelection);
            this.drawYAxis(updateType, plotSelection);
            this.drawBrush(updateType, plotSelection);
        }
        drawSpRect(updateType, plotSelection) {
            if (updateType & DiagPlot.INIT) {
                // Add a rect to allow highligthing
                const xScaleRange = this.xScale.range();
                const yScaleRange = this.yScale.range();
                const spRect = plotSelection.select(".spArea")
                    .append("rect")
                    .attr("class", "spRect")
                    .attr("x", xScaleRange[0])
                    .attr("y", yScaleRange[1])
                    .attr("width", xScaleRange[1] - xScaleRange[0])
                    .attr("height", yScaleRange[0] - yScaleRange[1]);
                // Compute 'xPlot' and 'yPlot' (useful for canvas)
                const mspDivNode = d3.select(this.bindto + " .mspDiv").node();
                const parentBounds = (mspDivNode === null) ? null : mspDivNode.getBoundingClientRect();
                const xParent = (parentBounds === null) ? 0 : parentBounds.x;
                const yParent = (parentBounds === null) ? 0 : parentBounds.y;
                const spRectNode = spRect.node();
                const spRectBounds = (spRectNode === null) ? null : spRectNode.getBoundingClientRect();
                const xSpRect = (spRectBounds === null) ? 0 : spRectBounds.x;
                const ySpRect = (spRectBounds === null) ? 0 : spRectBounds.y;
                this.xPlot = xSpRect - xParent;
                this.yPlot = ySpRect - yParent;
            }
        }
        hlGraph(highlight) {
            const plotSelection = this.plotSelection();
            plotSelection.select(".spRect")
                .classed("hlGraph", highlight);
        }
        // eslint-disable-next-line max-lines-per-function
        drawDistribPlots(updateType, plotSelection) {
            const distribGroup = plotSelection.select(".distribGroup");
            // Horizontal Distrib Plot
            if (updateType & DiagPlot.INIT || !this.distribPlot) {
                this.distribPlot = new spm.DistributionPlot(this.spData, this.xColumn, {
                    bindto: this.bindto,
                    orientation: spm.DistributionPlot.HOR,
                    mouseMode: this.mouseMode,
                    categoricalCsId: this.categoricalCsId,
                    distribType: this.distribType,
                    style: this.style
                }, this.index, this.xPlot, this.yPlot);
                this.distribPlot.generate(distribGroup, "#tile-clip");
            }
            if (updateType & (DiagPlot.INIT | DiagPlot.SHAPE | DiagPlot.RANGE | DiagPlot.DOMAIN | DiagPlot.Z_AXIS)) {
                this.distribPlot
                    .valuesScaleRange(this.xScale.range())
                    .computePlot(this.zColumn)
                    .distribScaleRange(this.yScale.range());
            }
            if (updateType & (DiagPlot.INIT | DiagPlot.PALETTE | DiagPlot.Z_AXIS)) {
                this.distribPlot.colorScale(this.catColorScale);
            }
            this.distribPlot.update(updateType, distribGroup);
        }
        updateXScaleDomain() {
            this.xScale
                .range([0, this.width - spm.ScatterPlot.padding.r])
                .domain(this.xColumn.domain());
            if (this.xColumn.categories === null) {
                this.xScale.nice();
            }
            this.xAxis.scale(this.xScale)
                .ticks(this.xColumn.axisTicks())
                .tickSize(-this.height + spm.ScatterPlot.padding.t);
        }
        updateYScaleDomain() {
            if (this.distribPlot === null) {
                console.error("updateYScaleDomain, 'distribPlot' is null");
            }
            else {
                this.yScale
                    .domain(this.distribPlot.useHistogramRep()
                    ? this.distribPlot.mainDistrib.cutHistoScale.domain()
                    : this.distribPlot.mainDistrib.cutDensityScale.domain())
                    .nice();
                this.yAxis.scale(this.yScale)
                    .ticks(5)
                    .tickSize(-this.width + spm.ScatterPlot.padding.r);
            }
        }
        updateColorScales() {
            const zColumn = this.zColumn;
            if (zColumn === null) {
                return;
            }
            if (zColumn.categories === null) {
                const [zMin, zMax] = zColumn.domain();
                this.contColorScale = d3.scaleSequential(spm.SpConst.CONTINUOUS_CS[this.continuousCsId])
                    .domain([zMin, zMax]);
            }
            else {
                const zMax = zColumn.domain()[1];
                this.catColorScale = spm.SpConst.CATEGORIAL_CS[this.categoricalCsId]
                    .domain(d3.range(zMax));
            }
        }
        drawXAxis(updateType, plotSelection) {
            if (updateType & (DiagPlot.INIT | DiagPlot.RANGE | DiagPlot.DOMAIN | DiagPlot.SHAPE)) {
                const axesGroup = plotSelection.select(".axesGroup");
                if (updateType & DiagPlot.INIT) {
                    axesGroup.append("g")
                        .attr("class", "x axis")
                        .attr("transform", "translate(0," + this.height + ")");
                    if (this.axisVisibility.xTitle) {
                        const x = (this.width - spm.ScatterPlot.padding.r) / 2;
                        const y = this.height + DiagPlot.margin.b / 2;
                        axesGroup.append("text")
                            .attr("class", "x scatterlabel")
                            .attr("x", x)
                            .attr("y", y)
                            .attr("text-anchor", "middle")
                            .attr("dominant-baseline", "middle");
                    }
                }
                axesGroup.select(".x.axis").call(this.xAxis)
                    .selectAll(".tick text")
                    .attr("transform", "rotate(45)")
                    .style("text-anchor", "start")
                    .attr("display", this.axisVisibility.xValues ? "block" : "none");
                if (this.axisVisibility.xTitle) {
                    axesGroup.select(".x.scatterlabel").text(this.xColumn.label.replace(/<br>/gi, " "));
                }
            }
        }
        drawYAxis(updateType, plotSelection) {
            if (updateType & (DiagPlot.INIT | DiagPlot.RANGE | DiagPlot.DOMAIN | DiagPlot.SHAPE)) {
                const axesGroup = plotSelection.select(".axesGroup");
                if (updateType & DiagPlot.INIT) {
                    axesGroup.append("g")
                        .attr("class", "y axis");
                    if (this.axisVisibility.xTitle) {
                        const x = -DiagPlot.margin.l * 0.7;
                        const y = (this.height + spm.ScatterPlot.padding.t) / 2;
                        axesGroup.append("text")
                            .attr("class", "y scatterlabel")
                            .attr("transform", "translate(" + x + "," + y + ")rotate(270)")
                            .attr("dominant-baseline", "baseline")
                            .attr("text-anchor", "middle");
                    }
                }
                axesGroup.select(".y.axis").call(this.yAxis)
                    .selectAll(".tick text").attr("display", this.axisVisibility.yValues ? "block" : "none");
                if (this.axisVisibility.yTitle) {
                    axesGroup.select(".y.scatterlabel").text(this.xColumn.label.replace(/<br>/gi, " "));
                }
            }
        }
        fixBrush() {
            const plotSelection = this.plotSelection();
            if (this.mouseMode === spm.SpConst.tooltipMouse.key) {
                plotSelection.selectAll(".selection").style("display", "none");
                plotSelection.selectAll(".handle").style("display", "none");
                plotSelection.selectAll(".overlay").style("pointer-events", "auto");
            }
            else {
                plotSelection.selectAll(".selection").style("display", null);
                plotSelection.selectAll(".handle").style("display", null);
                plotSelection.selectAll(".overlay").style("pointer-events", "all");
            }
            if (this.mouseMode === spm.SpConst.filterMouse.key) {
                this.drawBrush(spm.ScatterPlot.RANGE, plotSelection);
            }
        }
        drawBrush(updateType, plotSelection) {
            const thisPlot = this;
            const spArea = plotSelection.select(".spArea");
            if (updateType & DiagPlot.INIT || !this.brush) {
                this.brush = d3.brush()
                    // At the end of a brush gesture (such as on mouseup), apply an action according to 'mouseMode'
                    .on("end", () => {
                    const brushZone = d3.event.selection;
                    thisPlot.brushend(brushZone);
                });
                // Set brushable area
                const xExtent = [
                    thisPlot.xScale.range()[0],
                    thisPlot.yScale.range()[1]
                ];
                const yExtent = [
                    thisPlot.xScale.range()[1],
                    thisPlot.yScale.range()[0]
                ];
                this.brush.extent([xExtent, yExtent]);
                // Create the SVG elements necessary to display the brush selection and to receive input events for interaction
                spArea.call(this.brush);
            }
        }
        // eslint-disable-next-line max-lines-per-function
        brushend(brushZone) {
            if (!this.brush) {
                return;
            }
            const thisPlot = this;
            if (this.mouseMode === spm.SpConst.zoomMouse.key) {
                // zoom on selected zone, or unzoom when a double-click is detected
                if (!brushZone && !this.dblClickTimeout) {
                    this.dblClickTimeout = setTimeout(function () {
                        thisPlot.dblClickTimeout = null;
                    }, spm.SpConst.dblClickDelay);
                    return;
                }
                const plotSelection = this.plotSelection();
                if (brushZone) {
                    this.xScale.domain([brushZone[0][0], brushZone[1][0]].map(this.xScale.invert));
                    this.yScale.domain([brushZone[1][1], brushZone[0][1]].map(this.yScale.invert));
                    // Remove the brush selection
                    plotSelection.select(".spArea").call(this.brush.clear);
                }
                else {
                    this.xScale.domain(this.xColumn.domain());
                    if (this.distribPlot) {
                        this.yScale.domain(this.distribPlot.mainDistrib.cutDensityScale.domain());
                    }
                }
                if (this.xColumn.categories === null) {
                    this.xScale.nice();
                }
                // Zoom for axes
                this.drawXAxis(DiagPlot.DOMAIN, plotSelection);
                this.drawYAxis(DiagPlot.DOMAIN, plotSelection);
                // Zoom for brush
                this.drawBrush(DiagPlot.DOMAIN, plotSelection);
                // Zoom for distribution plot
                if (this.distribPlot) {
                    this.distribPlot.valuesScale.domain(this.xScale.domain());
                    this.distribPlot.update(DiagPlot.DOMAIN, plotSelection.select(".distribGroup"));
                }
            }
        }
        plotSelection(plotSelection) {
            if (plotSelection) {
                return plotSelection;
            }
            const thisPlot = this;
            const mspGroup = d3.select(this.bindto + " .mspGroup");
            return mspGroup.selectAll(".diagPlot")
                .filter(function (plot) {
                return plot.row === thisPlot.row && plot.col === thisPlot.col;
            });
        }
        distribRepChange(newType) {
            this.distribType = newType;
            if (this.distribPlot) {
                this.distribPlot.distribType = newType;
            }
            const plotSelection = this.plotSelection();
            this.drawDistribPlots(DiagPlot.SHAPE, plotSelection);
            this.updateYScaleDomain();
            this.drawYAxis(DiagPlot.SHAPE, plotSelection);
        }
        changeMouseMode(mouseMode) {
            this.mouseMode = mouseMode;
            if (this.distribPlot) {
                this.distribPlot.mouseMode = mouseMode;
            }
        }
    }
    DiagPlot.margin = { l: 60, r: 10, b: 50, t: 5 };
    DiagPlot.cslRight = 30;
    DiagPlot.cslLeft = 10;
    DiagPlot.cslWidth = 20;
    DiagPlot.cslTotalWidth = DiagPlot.cslRight + DiagPlot.cslLeft + DiagPlot.cslWidth;
    DiagPlot.INIT = 1;
    DiagPlot.SHAPE = 1 << 1;
    DiagPlot.PALETTE = 1 << 2;
    DiagPlot.Z_AXIS = 1 << 3;
    DiagPlot.RANGE = 1 << 4;
    DiagPlot.DOMAIN = 1 << 5;
    spm.DiagPlot = DiagPlot;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class DistributionData {
        constructor(distributionPlot, zCatDescription) {
            this.uncutDensityScale = d3.scaleLinear();
            this.uncutDensity = null;
            this.cutDensityScale = d3.scaleLinear();
            this.cutDensity = null;
            this.uncutHistoScale = d3.scaleLinear();
            this.uncutBins = null;
            this.cutHistoScale = d3.scaleLinear();
            this.cutBins = null;
            this.plot = distributionPlot;
            this.zCatDescription = zCatDescription;
        }
        computePlot(violinCatDescription) {
            this.violinCatDescription = violinCatDescription;
            // Compute uncut plot
            const filteredUncutData = this.filterData(this.plot.spData.sampleData);
            this.computeUncutPlot(filteredUncutData);
            // Compute cut plot
            const filteredCutData = this.filterData(this.plot.spData.cutData());
            this.computeCutPlot(filteredCutData);
            // Adjust domain between cut and uncut
            if (filteredCutData.length > filteredUncutData.length / 2.0) {
                if (this.plot.useDensityRep()) {
                    const max = d3.max([this.cutDensityScale.domain(), this.uncutDensityScale.domain()], d => d[1]);
                    if (max) {
                        this.uncutDensityScale.domain([0, max]);
                        this.cutDensityScale.domain([0, max]);
                    }
                }
                if (this.plot.useHistogramRep()) {
                    this.cutHistoScale.domain(this.uncutHistoScale.domain());
                }
            }
            return this;
        }
        filterData(data) {
            const violinCatDescription = this.violinCatDescription;
            const filtered = violinCatDescription
                ? data.filter(function (row) {
                    return row[violinCatDescription.column.dim] === violinCatDescription.catIndex;
                })
                : data;
            const zCatDescription = this.zCatDescription;
            return zCatDescription
                ? filtered.filter(function (row) {
                    return row[zCatDescription.column.dim] === zCatDescription.catIndex;
                })
                : filtered;
        }
        computeUncutPlot(uncutData) {
            const thisData = this;
            const data = uncutData.map(function (row) { return row[thisData.plot.column.dim]; });
            if (this.plot.useDensityRep()) {
                this.uncutDensity = this.computeDensityPlot(data, this.uncutDensityScale);
            }
            if (this.plot.useHistogramRep()) {
                this.uncutBins = this.computeHistogramPlot(data, this.uncutHistoScale);
            }
            return this;
        }
        computeCutPlot(cutData) {
            const thisData = this;
            const data = cutData.map(function (row) { return row[thisData.plot.column.dim]; });
            if (this.plot.useDensityRep()) {
                this.cutDensity = this.computeDensityPlot(data, this.cutDensityScale);
            }
            if (this.plot.useHistogramRep()) {
                this.cutBins = this.computeHistogramPlot(data, this.cutHistoScale);
            }
            return this;
        }
        computeDensityPlot(data, densityScale) {
            const bandwidth = 0.9 * Math.min(this.plot.column.sd, Math.abs(this.plot.column.p75 - this.plot.column.p25) / 1.34) * Math.pow(data.length, -0.2);
            const domain = this.plot.valuesScale.domain();
            const thresholds = this.equiDepthThresholds(domain[0], domain[1], true);
            const density = DistributionData.kde(DistributionData.epanechnikov(bandwidth), thresholds, data);
            density.push([thresholds[thresholds.length - 1], 0]);
            density.unshift([thresholds[0], 0]);
            const densityMax = d3.max(density, point => point[1]);
            densityScale.domain([0, densityMax ? densityMax * 1.05 : 1]); // +5% to be sure to see top of curves
            return density;
        }
        computeHistogramPlot(data, histoScale) {
            const domain = this.plot.valuesScale.domain();
            const thresholds = this.plot.column.categories
                ? d3.range(spm.SpConst.CAT_RATIO * this.plot.column.categories.length)
                    .map(t => (t - 0.5) / spm.SpConst.CAT_RATIO)
                : this.equiDepthThresholds(domain[0], domain[1], false);
            const bins = d3.histogram()
                .domain(domain)
                .thresholds(thresholds)(data);
            const binMax = d3.max(bins, bin => bin.length);
            histoScale.domain([0, binMax ? binMax : 1]);
            return bins;
        }
        equiDepthThresholds(min, max, includeMax) {
            const binBounds = [];
            const depth = (max - min) / this.numbin();
            for (let j = 0; j < this.numbin(); j++) {
                binBounds.push(min + j * depth);
            }
            if (includeMax) {
                binBounds.push(max);
            }
            return binBounds;
        }
        numbin() {
            return Math.ceil(2.5 * Math.pow(this.plot.spData.sampleData.length, 0.25));
        }
        distribScaleRange(range) {
            this.uncutDensityScale.range(range);
            this.uncutHistoScale.range(range);
            this.cutDensityScale.range(range);
            this.cutHistoScale.range(range);
            return this;
        }
        static kde(kernel, thresholds, data) {
            return thresholds.map(t => [t, d3.mean(data, d => kernel(t - d))]);
        }
        static epanechnikov(bandwidth) {
            return (x1) => {
                const x = x1 / bandwidth;
                return (Math.abs(x) <= 1) ? 0.75 * (1 - x * x) / bandwidth : 0;
            };
        }
    }
    spm.DistributionData = DistributionData;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class DistributionPlot {
        constructor(spData, column, dpConfig, plotIndex, xPlot, yPlot) {
            this.valuesScale = d3.scaleLinear();
            this.zColumn = null;
            this.xPlot = 0;
            this.yPlot = 0;
            this.plotIndex = 0;
            this.bindto = dpConfig.bindto;
            this.spData = spData;
            this.mouseMode = dpConfig.mouseMode;
            this.distribType = dpConfig.distribType;
            this.column = column;
            this.orientation = dpConfig.orientation;
            this.style = dpConfig.style;
            this.plotIndex = plotIndex;
            this.xPlot = xPlot;
            this.yPlot = yPlot;
            this.valuesScale.domain(this.column.domain());
            if (this.column.categories === null) {
                this.valuesScale.nice();
            }
            this.mainDistrib = new spm.DistributionData(this);
            this.subDistribList = [];
            this.subColorScale = spm.SpConst.CATEGORIAL_CS[dpConfig.categoricalCsId];
        }
        generate(distribGroup, clipSelector) {
            const plotClass = this.orientation === DistributionPlot.HOR ? "hor distribPlot" : "ver distribPlot";
            const distribPlot = distribGroup.append("g").attr("class", plotClass);
            if (clipSelector) {
                distribPlot.attr("clip-path", "url(" + clipSelector + ")");
            }
            const uncut = distribPlot.append("g").attr("class", "uncut");
            const cut = distribPlot.append("g").attr("class", "cut");
            // About density plots
            cut.append("g").attr("class", "pdfGroup");
            cut.append("g").attr("class", "subPdfGroup");
            uncut.append("g").attr("class", "pdfGroup");
            uncut.append("g").attr("class", "subPdfGroup");
            // About histograms
            cut.append("g").attr("class", "histoGroup");
            cut.append("g").attr("class", "subHistoGroup");
            uncut.append("g").attr("class", "histoGroup");
            uncut.append("g").attr("class", "subHistoGroup");
            return this;
        }
        useDensityRep() {
            return (this.distribType & DistributionPlot.DENS_REP) !== 0 && this.column.categories === null;
        }
        useHistogramRep() {
            return (this.distribType & DistributionPlot.HISTO_REP) !== 0 || this.column.categories !== null;
        }
        colorScale(colorScale) {
            this.subColorScale = colorScale;
            return this;
        }
        update(updateType, distribGroup) {
            if (this.orientation === DistributionPlot.HOR) {
                this.updateHor(updateType, distribGroup);
            }
            else {
                this.updateVer(updateType, distribGroup);
            }
        }
        updateVer(_updateType, distribGroup) {
            this.updateVerDensityPlot(distribGroup, DistributionPlot.SUB_FILTER);
            this.updateVerDensityPlot(distribGroup, DistributionPlot.SUB_FILTER | DistributionPlot.CUT_FILTER);
            this.updateVerDensityPlot(distribGroup, DistributionPlot.CUT_FILTER);
            this.updateVerDensityPlot(distribGroup, DistributionPlot.NO_FILTER);
            this.updateVerHistogram(distribGroup, DistributionPlot.SUB_FILTER);
            this.updateVerHistogram(distribGroup, DistributionPlot.SUB_FILTER | DistributionPlot.CUT_FILTER);
            this.updateVerHistogram(distribGroup, DistributionPlot.CUT_FILTER);
            this.updateVerHistogram(distribGroup, DistributionPlot.NO_FILTER);
        }
        updateHor(_updateType, distribGroup) {
            this.updateHorDensityPlot(distribGroup, DistributionPlot.SUB_FILTER);
            this.updateHorDensityPlot(distribGroup, DistributionPlot.SUB_FILTER | DistributionPlot.CUT_FILTER);
            this.updateHorDensityPlot(distribGroup, DistributionPlot.CUT_FILTER);
            this.updateHorDensityPlot(distribGroup, DistributionPlot.NO_FILTER);
            this.updateHorHistogram(distribGroup, DistributionPlot.SUB_FILTER);
            this.updateHorHistogram(distribGroup, DistributionPlot.SUB_FILTER | DistributionPlot.CUT_FILTER);
            this.updateHorHistogram(distribGroup, DistributionPlot.CUT_FILTER);
            this.updateHorHistogram(distribGroup, DistributionPlot.NO_FILTER);
        }
        plotVisibility(repType, filtering) {
            // if plot is about data coming from cutoff
            if (filtering & DistributionPlot.CUT_FILTER) {
                // if plot is about data coming from sub data
                if (filtering & DistributionPlot.SUB_FILTER) {
                    // plot is visible if sub data are available
                    return this.subDistribList.length !== 0;
                }
                else {
                    // plot is visible if sub data are not available
                    return this.subDistribList.length === 0;
                }
            }
            // if plot is not about data coming from cutoff
            else {
                // plot is not visible if data comes from sub data
                if (filtering & DistributionPlot.SUB_FILTER) {
                    return false;
                }
                else {
                    // if plot is an histogram
                    if (repType & DistributionPlot.HISTO_REP) {
                        // plot is visible if data have same domain for cut and uncut
                        return DistributionPlot.equalsDomain(this.mainDistrib.cutHistoScale.domain(), this.mainDistrib.uncutHistoScale.domain());
                    }
                    else {
                        // plot is visible if it's a density plot and data comes from all data
                        return true;
                    }
                }
            }
        }
        updateHorDensityPlot(distribGroup, filtering) {
            const thisPlot = this;
            const dataList = filtering & DistributionPlot.SUB_FILTER ? this.subDistribList : [this.mainDistrib];
            const cutClass = filtering & DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
            const pdfClass = filtering & DistributionPlot.SUB_FILTER ? ".subPdfGroup" : ".pdfGroup";
            const plotGroup = distribGroup.select(`.hor ${cutClass} ${pdfClass}`);
            if (this.useDensityRep() && this.plotVisibility(DistributionPlot.DENS_REP, filtering)) {
                plotGroup.style("display", null);
            }
            else {
                plotGroup.style("display", "none");
                return;
            }
            const densPath = plotGroup.selectAll("path").data(dataList)
                .join(enter => enter.append("path"), update => update, exit => exit.remove())
                .attr("fill", (_data, dataIndex) => this.fillColor(dataIndex, filtering))
                .attr("d", function (data) {
                let density = filtering & DistributionPlot.CUT_FILTER ? data.cutDensity : data.uncutDensity;
                if (!density) {
                    console.error("DistributionPlot.update called but no density computed");
                    density = [[0, 0]];
                }
                const densityScale = filtering & DistributionPlot.CUT_FILTER ? data.cutDensityScale : data.uncutDensityScale;
                const lineGenerator = d3.line()
                    .curve(d3.curveNatural)
                    .x(d => {
                    const x = thisPlot.valuesScale(d[0]);
                    return DistributionPlot.undef2Nan(x);
                })
                    .y(d => {
                    const y = densityScale(d[1]);
                    return DistributionPlot.undef2Nan(y);
                });
                return lineGenerator(density);
            });
            thisPlot.densMouse(densPath, filtering);
        }
        updateVerDensityPlot(distribGroup, filtering) {
            const thisPlot = this;
            const dataList = filtering & DistributionPlot.SUB_FILTER ? this.subDistribList : [this.mainDistrib];
            const cutClass = filtering & DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
            const pdfClass = filtering & DistributionPlot.SUB_FILTER ? ".subPdfGroup" : ".pdfGroup";
            const plotGroup = distribGroup.select(`.ver ${cutClass} ${pdfClass}`);
            if (this.useDensityRep() && this.plotVisibility(DistributionPlot.DENS_REP, filtering)) {
                plotGroup.style("display", null);
            }
            else {
                plotGroup.style("display", "none");
                return;
            }
            const densPath = plotGroup.selectAll("path").data(dataList)
                .join(enter => enter.append("path"), update => update, exit => exit.remove())
                .attr("fill", (_data, dataIndex) => this.fillColor(dataIndex, filtering))
                .attr("d", function (data) {
                let density = filtering & DistributionPlot.CUT_FILTER ? data.cutDensity : data.uncutDensity;
                if (!density) {
                    console.error("DistributionPlot.update called but no density computed");
                    density = [[0, 0]];
                }
                const densityScale = filtering & DistributionPlot.CUT_FILTER ? data.cutDensityScale : data.uncutDensityScale;
                const lineGenerator = d3.line()
                    .curve(d3.curveBasis)
                    .x(d => {
                    const x = densityScale(d[1]);
                    return DistributionPlot.undef2Nan(x);
                })
                    .y(d => {
                    const y = thisPlot.valuesScale(d[0]);
                    return DistributionPlot.undef2Nan(y);
                });
                return lineGenerator(density);
            });
            thisPlot.densMouse(densPath, filtering);
        }
        // eslint-disable-next-line max-lines-per-function
        densMouse(densPath, filtering) {
            if (filtering & DistributionPlot.CUT_FILTER) {
                const thisPlot = this;
                densPath
                    // eslint-disable-next-line max-lines-per-function
                    .on("mouseover", function (data, dataIndex) {
                    if (thisPlot.mouseMode !== spm.SpConst.tooltipMouse.key) {
                        return;
                    }
                    d3.select(this).attr("fill", thisPlot.fillColor(dataIndex, filtering, 3));
                    thisPlot.pdfDisplay(filtering, dataIndex, false);
                    const tooltipLocation = thisPlot.tooltipLocation();
                    d3.select(thisPlot.bindto + " .mspTooltip").remove();
                    const mspDiv = d3.select(thisPlot.bindto + " .mspDiv");
                    const tooltip = mspDiv.append("div")
                        .attr("class", "mspTooltip")
                        .style("display", "block")
                        .style("left", tooltipLocation[0] + "px")
                        .style("top", tooltipLocation[1] + "px");
                    tooltip.append("div")
                        .attr("class", "title")
                        .html("Density Curve");
                    tooltip.append("div")
                        .html(`<span class="pdfColumn">Marginal distribution for</span>: <span class="pdfColumnLabel">${thisPlot.column.labelText()}`);
                    if (data.violinCatDescription) {
                        const category = (data.violinCatDescription.column.categories)
                            ? data.violinCatDescription.column.categories[data.violinCatDescription.catIndex]
                            : "undefined";
                        const axisName = thisPlot.orientation === DistributionPlot.HOR ? "y axis" : "x axis";
                        tooltip.append("div")
                            .html(`conditional on ${data.violinCatDescription.column.labelText()} = ${category} (${axisName} )`);
                    }
                    if (filtering & spm.RegressionPlot.SUB_FILTER && thisPlot.column !== thisPlot.zColumn) {
                        if (thisPlot.zColumn) {
                            const category = (thisPlot.zColumn.categories)
                                ? thisPlot.zColumn.categories[dataIndex]
                                : "undefined";
                            const coloringHtml = `<span class='swatch' style='background:${thisPlot.fillColor(dataIndex, filtering)}'>&nbsp;</span>`;
                            if (data.violinCatDescription) {
                                tooltip.append("div")
                                    .html(`and on ${thisPlot.zColumn.labelText()} = ${DistributionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
                            }
                            else {
                                tooltip.append("div")
                                    .html(`conditional on ${thisPlot.zColumn.labelText()} = ${DistributionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
                            }
                        }
                        else {
                            tooltip.append("div").html("conditional on undefined = undefined");
                        }
                    }
                    if (filtering & spm.RegressionPlot.CUT_FILTER) {
                        const filteredUncutData = data.filterData(thisPlot.spData.sampleData);
                        const filteredCutData = data.filterData(thisPlot.spData.cutData());
                        tooltip.append("div")
                            .html(`${filteredCutData.length} sample points (filtered points: ${filteredUncutData.length - filteredCutData.length})`);
                    }
                })
                    .on("mouseout", function (_data, dataIndex) {
                    if (thisPlot.mouseMode === spm.SpConst.tooltipMouse.key) {
                        d3.select(this).attr("fill", thisPlot.fillColor(dataIndex, filtering));
                        thisPlot.pdfDisplay(filtering, dataIndex, true);
                        d3.select(thisPlot.bindto + " .mspTooltip").style("display", "none");
                    }
                });
            }
            else {
                densPath.style("pointer-events", "none");
            }
        }
        tooltipLocation() {
            const mspDivNode = d3.select(this.bindto + " .mspDiv").node();
            const parentBounds = (mspDivNode === null) ? null : mspDivNode.getBoundingClientRect();
            const xParent = (parentBounds === null) ? 0 : parentBounds.x;
            const plotGroup = d3.select(this.bindto + " .mspGroup").node();
            const elementBounds = (plotGroup === null) ? null : plotGroup.getBoundingClientRect();
            const xRect = (elementBounds === null) ? 0 : elementBounds.x;
            const wRect = (elementBounds === null) ? 0 : elementBounds.width;
            return [xRect - xParent + wRect + 5, this.yPlot];
        }
        pdfDisplay(filtering, dataIndex, display) {
            const mspGroup = d3.select(this.bindto + " .mspGroup");
            const cutClass = filtering & DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
            const pdfGroup = mspGroup.selectAll(`${cutClass} .pdfGroup`);
            const pdfPath = pdfGroup.selectAll("path")
                .filter(function (_data, dataIndex2) {
                return dataIndex2 !== dataIndex;
            });
            const subPdfGroup = mspGroup.selectAll(`${cutClass} .subPdfGroup`);
            const subPdfPath = subPdfGroup.selectAll("path")
                .filter(function (_data, dataIndex2) {
                return dataIndex2 !== dataIndex;
            });
            if (display) {
                pdfPath.style("display", null);
                subPdfPath.style("display", null);
            }
            else {
                pdfPath.style("display", "none");
                subPdfPath.style("display", "none");
            }
        }
        // eslint-disable-next-line max-lines-per-function
        updateHorHistogram(distribGroup, filtering) {
            const thisPlot = this;
            const dataList = filtering & DistributionPlot.SUB_FILTER ? this.subDistribList : [this.mainDistrib];
            const cutClass = filtering & DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
            const histoClass = filtering & DistributionPlot.SUB_FILTER ? ".subHistoGroup" : ".histoGroup";
            const plotGroup = distribGroup.select(`.hor ${cutClass} ${histoClass}`);
            if (this.useHistogramRep() && this.plotVisibility(DistributionPlot.HISTO_REP, filtering)) {
                plotGroup.style("display", null);
            }
            else {
                plotGroup.style("display", "none");
                return;
            }
            const binsGroup = plotGroup.selectAll("g").data(dataList)
                .join(enter => enter.append("g"), update => update, exit => exit.remove());
            let baseLineList = [];
            // eslint-disable-next-line max-lines-per-function
            binsGroup.each(function (data, dataIndex) {
                let bins = filtering & DistributionPlot.CUT_FILTER ? data.cutBins : data.uncutBins;
                if (!bins) {
                    console.error("DistribtionPlot.update called but no histogram computed");
                    bins = [];
                }
                if (dataIndex === 0) {
                    baseLineList = bins.map(_bin => 0);
                }
                const histoScale = filtering & DistributionPlot.CUT_FILTER ? data.cutHistoScale : data.uncutHistoScale;
                const binsSelection = d3.select(this);
                binsSelection.selectAll("rect").remove();
                const binRect = binsSelection.selectAll("rect")
                    .data(bins).enter()
                    .append("rect")
                    .attr("fill", thisPlot.fillColor(dataIndex, filtering))
                    .attr("x", function (bin) {
                    if (typeof bin.x0 === "undefined") {
                        console.error("bin.x0 is undefined");
                        return NaN;
                    }
                    const x = thisPlot.valuesScale(bin.x0);
                    return DistributionPlot.undef2Nan(x);
                })
                    .attr("y", function (bin, binIndex) {
                    const y = histoScale(bin.length + baseLineList[binIndex]);
                    return DistributionPlot.undef2Nan(y);
                })
                    .attr("width", function (bin) {
                    if (typeof bin.x0 === "undefined" || typeof bin.x1 === "undefined") {
                        console.error("bin.x0 or bin.x1 are undefined");
                        return NaN;
                    }
                    const x0 = DistributionPlot.undef2Nan(thisPlot.valuesScale(bin.x0));
                    const x1 = DistributionPlot.undef2Nan(thisPlot.valuesScale(bin.x1));
                    return x1 - x0;
                })
                    .attr("height", function (bin) {
                    const y0 = DistributionPlot.undef2Nan(histoScale(0));
                    const y1 = DistributionPlot.undef2Nan(histoScale(bin.length));
                    return y0 - y1;
                });
                thisPlot.histoMouse(binRect, filtering, data, dataIndex);
                bins.forEach((bin, binIndex) => {
                    baseLineList[binIndex] += bin.length;
                });
            });
        }
        // eslint-disable-next-line max-lines-per-function
        updateVerHistogram(distribGroup, filtering) {
            const thisPlot = this;
            const dataList = filtering & DistributionPlot.SUB_FILTER ? this.subDistribList : [this.mainDistrib];
            const cutClass = filtering & DistributionPlot.CUT_FILTER ? ".cut" : ".uncut";
            const histoClass = filtering & DistributionPlot.SUB_FILTER ? ".subHistoGroup" : ".histoGroup";
            const plotGroup = distribGroup.select(`.ver ${cutClass} ${histoClass}`);
            if (this.useHistogramRep() && this.plotVisibility(DistributionPlot.HISTO_REP, filtering)) {
                plotGroup.style("display", null);
            }
            else {
                plotGroup.style("display", "none");
                return;
            }
            const binsGroup = plotGroup.selectAll("g").data(dataList)
                .join(enter => enter.append("g"), update => update, exit => exit.remove());
            let baseLineList = [];
            // eslint-disable-next-line max-lines-per-function
            binsGroup.each(function (data, dataIndex) {
                let bins = filtering & DistributionPlot.CUT_FILTER ? data.cutBins : data.uncutBins;
                if (!bins) {
                    console.error("DistribtionPlot.update called but no histogram computed");
                    bins = [];
                }
                if (dataIndex === 0) {
                    baseLineList = bins.map(_bin => 0);
                }
                const histoScale = filtering & DistributionPlot.CUT_FILTER ? data.cutHistoScale : data.uncutHistoScale;
                const binsSelection = d3.select(this);
                binsSelection.selectAll("rect").remove();
                const binRect = binsSelection.selectAll("rect")
                    .data(bins).enter()
                    .append("rect")
                    .attr("fill", thisPlot.fillColor(dataIndex, filtering))
                    .attr("y", function (bin) {
                    if (typeof bin.x0 === "undefined" || typeof bin.x1 === "undefined") {
                        console.error("bin.x0 or bin.x1 are undefined");
                        return NaN;
                    }
                    const y0 = DistributionPlot.undef2Nan(thisPlot.valuesScale(bin.x0));
                    const y1 = DistributionPlot.undef2Nan(thisPlot.valuesScale(bin.x1));
                    return Math.min(y0, y1);
                })
                    .attr("x", function (_bin, binIndex) {
                    const x = histoScale(0 + baseLineList[binIndex]);
                    return DistributionPlot.undef2Nan(x);
                })
                    .attr("height", function (bin) {
                    if (typeof bin.x0 === "undefined" || typeof bin.x1 === "undefined") {
                        console.error("bin.x0 or bin.x1 are undefined");
                        return NaN;
                    }
                    const y1 = DistributionPlot.undef2Nan(thisPlot.valuesScale(bin.x1));
                    const y0 = DistributionPlot.undef2Nan(thisPlot.valuesScale(bin.x0));
                    return Math.abs(y1 - y0);
                })
                    .attr("width", function (bin) {
                    const x0 = DistributionPlot.undef2Nan(histoScale(0));
                    const x1 = DistributionPlot.undef2Nan(histoScale(bin.length));
                    return x1 - x0;
                });
                thisPlot.histoMouse(binRect, filtering, data, dataIndex);
                bins.forEach((bin, binIndex) => {
                    baseLineList[binIndex] += bin.length;
                });
            });
        }
        static undef2Nan(value) {
            return typeof value === "undefined" ? NaN : value;
        }
        // eslint-disable-next-line max-lines-per-function
        histoMouse(binRect, filtering, data, dataIndex) {
            if (filtering & DistributionPlot.CUT_FILTER) {
                const thisPlot = this;
                binRect
                    // eslint-disable-next-line max-lines-per-function
                    .on("mouseover", function (bin, binIndex) {
                    if (thisPlot.mouseMode !== spm.SpConst.tooltipMouse.key) {
                        return;
                    }
                    d3.select(this).attr("fill", thisPlot.fillColor(dataIndex, filtering, 3));
                    const x0 = (typeof bin.x0 === "undefined") ? "undefined" : spm.ExpFormat.format(bin.x0);
                    const x1 = (typeof bin.x1 === "undefined") ? "undefined" : spm.ExpFormat.format(bin.x1);
                    const tooltipLocation = thisPlot.tooltipLocation();
                    d3.select(thisPlot.bindto + " .mspTooltip").remove();
                    const mspDiv = d3.select(thisPlot.bindto + " .mspDiv");
                    const tooltip = mspDiv.append("div")
                        .attr("class", "mspTooltip")
                        .style("display", "block")
                        .style("left", tooltipLocation[0] + "px")
                        .style("top", tooltipLocation[1] + "px");
                    tooltip.append("div")
                        .attr("class", "title")
                        .html("Histogram");
                    if (thisPlot.column.categories) {
                        if (typeof bin.x0 !== "undefined" && typeof bin.x1 !== "undefined") {
                            const cat = thisPlot.column.categories[(bin.x0 + bin.x1) / 2];
                            tooltip.append("div")
                                .html(`<span class="binColumn">bin for</span>: <span class="binColumnLabel">${thisPlot.column.labelText()}</span> = <span class="domainValue">${cat}</span>`);
                        }
                    }
                    else {
                        tooltip.append("div")
                            .html(`<span class="binColumn">bin for</span>: <span class="binColumnValue">${thisPlot.column.labelText()}</span> ∈ <span class="domainValue">[${x0}, ${x1}[</span> `);
                    }
                    if (data.violinCatDescription) {
                        const category = (data.violinCatDescription.column.categories)
                            ? data.violinCatDescription.column.categories[data.violinCatDescription.catIndex]
                            : "undefined";
                        const axisName = thisPlot.orientation === DistributionPlot.HOR ? "y axis" : "x axis";
                        tooltip.append("div")
                            .html(`where ${data.violinCatDescription.column.labelText()} = ${category} (${axisName})`);
                    }
                    if (filtering & spm.RegressionPlot.SUB_FILTER && thisPlot.column !== thisPlot.zColumn) {
                        if (thisPlot.zColumn) {
                            const category = (thisPlot.zColumn.categories)
                                ? thisPlot.zColumn.categories[dataIndex]
                                : "undefined";
                            const coloringHtml = `<span class='swatch' style='background:${thisPlot.fillColor(dataIndex, filtering)}'>&nbsp;</span>`;
                            if (data.violinCatDescription) {
                                tooltip.append("div")
                                    .html(`and ${thisPlot.zColumn.labelText()} is: ${DistributionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
                            }
                            else {
                                tooltip.append("div")
                                    .html(`where ${thisPlot.zColumn.labelText()} is: ${DistributionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
                            }
                        }
                        else {
                            tooltip.append("div").html("where zaxis undefined = undefined");
                        }
                    }
                    const uncutBins = data.uncutBins;
                    const cutInfo = uncutBins
                        ? `(${uncutBins[binIndex].length - bin.length} filtered points)`
                        : "";
                    tooltip.append("div")
                        .html(`<span class="binLength">=> <span class="binLengthValue">${bin.length}</span> sample points</span> ${cutInfo}`);
                })
                    .on("mouseout", function () {
                    if (thisPlot.mouseMode === spm.SpConst.tooltipMouse.key) {
                        d3.select(this).attr("fill", thisPlot.fillColor(dataIndex, filtering));
                        d3.select(thisPlot.bindto + " .mspTooltip").style("display", "none");
                    }
                });
            }
            else {
                binRect.style("pointer-events", "none");
            }
        }
        static brIze(category) {
            const splittedCategory = category.toString().split(", ");
            if (splittedCategory.length > 1) {
                return "<br>" + splittedCategory.join(",<br>") + "<br>";
            }
            return splittedCategory[0];
        }
        fillColor(dataIndex, filtering, k) {
            if (!(filtering & spm.RegressionPlot.CUT_FILTER)) {
                return this.style.plotProperties.watermarkColor;
            }
            const color = (filtering & spm.RegressionPlot.SUB_FILTER)
                ? this.subColorScale(dataIndex)
                : this.style.plotProperties.noCatColor;
            const d3Color = d3.color(color);
            return d3Color ? d3Color.darker(k).toString() : color;
        }
        computePlot(zColumn, violinCatDescription) {
            this.zColumn = zColumn;
            this.mainDistrib.computePlot(violinCatDescription);
            if (!zColumn || !zColumn.categories) {
                this.subDistribList = [];
            }
            else {
                this.subDistribList = zColumn.categories.map((_cat, i) => {
                    const zCatDescription = { column: zColumn, catIndex: i };
                    return new spm.DistributionData(this, zCatDescription);
                });
            }
            this.subDistribList.forEach(data => {
                data.computePlot(violinCatDescription);
                // Adjust domain between sub and main histogram
                if (data.plot.useHistogramRep()) {
                    data.cutHistoScale.domain(this.mainDistrib.cutHistoScale.domain());
                    data.uncutHistoScale.domain(this.mainDistrib.uncutHistoScale.domain());
                }
            });
            return this;
        }
        distribScaleRange(range) {
            this.mainDistrib.distribScaleRange(range);
            this.subDistribList.forEach(data => data.distribScaleRange(range));
            return this;
        }
        valuesScaleRange(range) {
            this.valuesScale.range(range);
            return this;
        }
        static equalsDomain(domain1, domain2) {
            return domain1[0] === domain2[0] && domain1[1] === domain2[1];
        }
    }
    DistributionPlot.HOR = "Horizontal";
    DistributionPlot.VER = "Vertical";
    DistributionPlot.NO_FILTER = 0;
    DistributionPlot.CUT_FILTER = 1;
    DistributionPlot.SUB_FILTER = 1 << 1;
    DistributionPlot.DENS_REP = 1;
    DistributionPlot.HISTO_REP = 1 << 1;
    spm.DistributionPlot = DistributionPlot;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class ExpFormat {
        static sToExp(siValue) {
            const siStr = /[yzafpnµmkMGTPEZY]/.exec(siValue);
            if (siStr !== null) {
                return siValue.replace(siStr[0], ExpFormat.NONBREAKING_SPACE + "E" + ExpFormat.EXP_FORMATS[siStr[0]]);
            }
            return siValue;
        }
        static format(value) {
            if (value > 1e3 || value < -1e3 || (value < 1e-3 && value > -1e-3)) {
                return ExpFormat.sToExp(ExpFormat.f2s(value));
            }
            return ExpFormat.f3f(value);
        }
    }
    ExpFormat.NONBREAKING_SPACE = String.fromCharCode(0xA0);
    ExpFormat.EXP_FORMATS = {
        "y": "-24",
        "z": "-21",
        "a": "-18",
        "f": "-15",
        "p": "-12",
        "n": "-9",
        "µ": "-6",
        "m": "-3",
        "k": "3",
        "M": "6",
        "G": "9",
        "T": "12",
        "P": "15",
        "E": "18",
        "Z": "21",
        "Y": "24"
    };
    ExpFormat.f2s = d3.format(".2~s");
    ExpFormat.f3f = d3.format(".3~r");
    spm.ExpFormat = ExpFormat;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class MultiBrush {
        constructor(plot) {
            // Keep the actual d3-brush functions and their IDs in a list
            this.brushDefList = [];
            this.plot = plot;
            // Add new empty BrushDef
            this.addNewBrushDef();
            this.applyDataJoin();
        }
        static multiBrushClass(plotIndex) {
            return "multibrush_plot" + plotIndex;
        }
        static brushClass(plotIndex, brushDef) {
            return "brush" + brushDef.id + "_plot" + plotIndex;
        }
        brushClass(brushDef) {
            return MultiBrush.brushClass(this.plot.index, brushDef);
        }
        addNewBrushDef(initialXYCutoff) {
            const thisMB = this;
            const tlCorner = [
                this.plot.xScale.range()[0],
                this.plot.yScale.range()[1]
            ];
            const brCorner = [
                this.plot.xScale.range()[1],
                this.plot.yScale.range()[0]
            ];
            const brush = d3.brush()
                .handleSize(4)
                // Set brushable area
                .extent([tlCorner, brCorner])
                // When the brush moves (such as on mousemove), update cutoffs of scatter plots
                .on("brush", function () { thisMB.updatePlotCutoffs(false); })
                // At the end of a brush gesture (such as on mouseup), update cutoffs of scatter plots and 'brushDefList'
                .on("end", function () {
                thisMB.updatePlotCutoffs(true);
                thisMB.updateBrushDefList();
            });
            const newBrushDef = {
                id: this.brushDefList.length,
                brush: brush,
                initialXYCutoff: initialXYCutoff
            };
            this.brushDefList.push(newBrushDef);
            return newBrushDef;
        }
        // eslint-disable-next-line max-lines-per-function
        applyDataJoin() {
            const thisMB = this;
            const brushGroup = d3.select(thisMB.plot.bindto + " ." + MultiBrush.multiBrushClass(this.plot.index))
                .selectAll(".brush")
                .data(this.brushDefList, brushDef => brushDef.id.toString());
            // Set up a new BrushBehavior for each entering Brush
            brushGroup.enter().insert("g", ".brush")
                .attr("class", function (brushDef) {
                return ["brush", thisMB.brushClass(brushDef)].join(" ");
            })
                .each(function (brushDef, brushIndex) {
                d3.select(this).call(brushDef.brush);
                // if entering Brush has an initialCutoff, modify BrushBehavior selection
                if (brushDef.initialXYCutoff) {
                    if (brushDef.initialXYCutoff[0] && brushDef.initialXYCutoff[1]) {
                        const tlCorner = [
                            thisMB.plot.xScale(brushDef.initialXYCutoff[0][0]),
                            thisMB.plot.yScale(brushDef.initialXYCutoff[1][1])
                        ];
                        const brCorner = [
                            thisMB.plot.xScale(brushDef.initialXYCutoff[0][1]),
                            thisMB.plot.yScale(brushDef.initialXYCutoff[1][0])
                        ];
                        d3.select(this).call(d3.brush().move, [tlCorner, brCorner]);
                    }
                    else {
                        console.error(`Plot (${thisMB.plot.xColumn.dim}, ${thisMB.plot.yColumn.dim}), brush ${brushIndex}, unexpected initialXYCutoff: ${brushDef.initialXYCutoff}`);
                    }
                }
            });
            brushGroup.each(function (brushDef) {
                d3.select(this)
                    .selectAll(".overlay")
                    .style("pointer-events", function () {
                    return (brushDef.id === thisMB.brushDefList.length - 1
                        && brushDef.brush !== undefined)
                        ? "all"
                        : "none";
                })
                    .on("click", function () {
                    thisMB.removeBrushes();
                });
            });
            brushGroup.exit().remove();
        }
        removeBrushes() {
            const brushSelections = [];
            this.plot.setContCutoff(brushSelections, true);
            // Remove all brushes
            this.brushDefList = [];
            this.applyDataJoin();
            // Add new empty BrushDef
            this.addNewBrushDef();
            this.applyDataJoin();
        }
        initFrom(xyCutoffs) {
            const thisMB = this;
            // Remove all Brushes
            thisMB.brushDefList = [];
            thisMB.applyDataJoin();
            if (xyCutoffs !== null) {
                // Add a new BrushDef for each given cutoffs
                xyCutoffs.forEach((xyCutoff) => {
                    thisMB.addNewBrushDef(xyCutoff);
                    thisMB.applyDataJoin();
                });
            }
            // Add new empty BrushDef
            thisMB.addNewBrushDef();
            thisMB.applyDataJoin();
        }
        updatePlotCutoffs(end) {
            const thisMB = this;
            const brushSelections = [];
            this.brushDefList.forEach(brushDef => {
                const brushGroup = d3.select(thisMB.plot.bindto + " ." + this.brushClass(brushDef));
                const brushSelection = d3.brushSelection(brushGroup.node());
                if (brushSelection !== null) {
                    brushSelections.push(brushSelection);
                }
            });
            this.plot.setContCutoff(brushSelections, end);
        }
        updateBrushDefList() {
            this.updatePlotCutoffs(true);
            // If our latest brush has a selection, that means we need to add a new empty BrushDef
            const lastBrushDef = this.brushDefList[this.brushDefList.length - 1];
            const lastBrushGroup = d3.select(this.plot.bindto + " ." + this.brushClass(lastBrushDef));
            const lastBrushSelection = d3.brushSelection(lastBrushGroup.node());
            if (lastBrushSelection && lastBrushSelection[0] !== lastBrushSelection[1]) {
                this.addNewBrushDef();
            }
            // Always draw brushes
            this.applyDataJoin();
        }
    }
    spm.MultiBrush = MultiBrush;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class MultipleScatterPlot {
        constructor(id, width, height) {
            this.width = 900;
            this.height = 750;
            this.scatterPlotList = [];
            this.mouseMode = spm.SpConst.tooltipMouse.key;
            this.legendVisibility = true;
            this.distribVisibility = true;
            this.distribType = 
            // DistributionPlot.DENS_REP;
            // DistributionPlot.DENS_REP |
            spm.DistributionPlot.HISTO_REP;
            this.regressionType = 0;
            // RegressionPlot.LOESS_REP;
            // RegressionPlot.LOESS_REP |
            // RegressionPlot.LINEAR_REP;
            this.continuousCsId = spm.SpConst.CONTINUOUS_CS_IDS[0];
            this.categoricalCsId = spm.SpConst.CATEGORIAL_CS_IDS[0];
            this.dispatch = d3.dispatch(MultipleScatterPlot.PLOT_EVENT);
            this.bindto = "#" + id;
            this.style = new spm.Style(this.bindto);
            this.setSize(width, height);
            this.visibilityPad = new spm.VisibilityPad(this);
            this.selectionPad = new spm.SelectionPad(this);
        }
        resize(width, height) {
            this.setSize(width, height);
            d3.select(this.bindto + " .MultiPlot svg")
                .attr("width", this.width + MultipleScatterPlot.margin.l + MultipleScatterPlot.margin.r)
                .attr("height", this.height + MultipleScatterPlot.margin.b + MultipleScatterPlot.margin.t);
            this.removePlots();
            this.updatePlots(spm.ScatterPlot.INIT);
        }
        setSize(width, height) {
            this.width = width - MultipleScatterPlot.margin.l - MultipleScatterPlot.margin.r;
            this.height = height - MultipleScatterPlot.margin.b - MultipleScatterPlot.margin.t;
        }
        removePlots() {
            this.scatterPlotList.forEach(plot => {
                plot.removePlot();
            });
        }
        id() {
            return this.bindto.substring(1);
        }
        generate(config) {
            if (d3.select(this.bindto).empty()) {
                throw new Error("'bindto' dom element not found:" + this.bindto);
            }
            this.style.initWith(config.cssRules, config.plotProperties);
            this.scatterPlotList.splice(0, this.scatterPlotList.length);
            this.initData(config);
            this.buildMainDomElements(config);
            this.visibilityPad.generate(config.visibilityPadId, config.visiblePlots);
            this.selectionPad.generate(config.selectionPadId).update();
            this.appendPlotSvg();
            this.appendDistribRepSelect();
            this.appendContCsSelect();
            this.appendCatCsSelect();
            this.appendXAxisSelector();
            this.appendYAxisSelector();
            this.appendZAxisSelector();
            this.initLegendVisibilityCB();
            this.initZAxisUsedCB();
            this.appendMouseModeSelect();
            this.initRegressionCB();
            this.spData.on(spm.SpData.HL_POINT_EVENT, MultipleScatterPlot.prototype.hlPoint.bind(this));
            this.updatePlots(spm.ScatterPlot.INIT);
            this.selectionPad.sendSelectionEvent();
            return this;
        }
        on(type, callback) {
            // @ts-ignore
            this.dispatch.on(type, callback);
        }
        hlPoint(hlEvent) {
            if (hlEvent.rowIndex === null) {
                new spm.PointsPlot(hlEvent.scatterPlot).mouseout();
            }
            else {
                new spm.PointsPlot(hlEvent.scatterPlot).mouseover(this.spData.sampleData[hlEvent.rowIndex], hlEvent.rowIndex, this.scatterPlotList);
            }
        }
        // eslint-disable-next-line max-lines-per-function
        initData(config) {
            if (!config.continuousCS) {
                this.continuousCsId = spm.SpConst.CONTINUOUS_CS_IDS[0];
            }
            else if (spm.SpConst.CONTINUOUS_CS_IDS.includes(config.continuousCS)) {
                this.continuousCsId = config.continuousCS;
            }
            else {
                console.error("Unknown continuous color scale: " + config.continuousCS);
            }
            if (!config.categoricalCS) {
                this.categoricalCsId = spm.SpConst.CATEGORIAL_CS_IDS[0];
            }
            else if (spm.SpConst.CATEGORIAL_CS_IDS.includes(config.categoricalCS)) {
                this.categoricalCsId = config.categoricalCS;
            }
            else {
                console.error("Unknown categorical color scale: " + config.categoricalCS);
            }
            if (!config.distribType) {
                this.distribType = 2;
            }
            else if ([0, 1, 2, 3].includes(config.distribType)) {
                this.distribType = config.distribType;
            }
            else {
                console.error("Unknown distribType: " + config.distribType);
            }
            if (!config.regressionType) {
                this.regressionType = 0;
            }
            else if ([0, 1, 2, 3].includes(config.regressionType)) {
                this.regressionType = config.regressionType;
            }
            else {
                console.error("Unknown regressionType: " + config.regressionType);
            }
            this.legendVisibility = typeof config.legendVisibility === "boolean" ? config.legendVisibility : true;
            this.spData = new spm.SpData(config);
            this.spData.updateCutRowsMask();
            this.spData.on(spm.SpData.ROW_FILTER_EVENT, MultipleScatterPlot.prototype.rowFilterChange.bind(this));
            for (let j = 0; j < MultipleScatterPlot.grid.nRow; j++) {
                for (let i = 0; i < MultipleScatterPlot.grid.nCol; i++) {
                    this.pushNewSP(i, j);
                }
            }
            this.initAxes(config);
        }
        initAxes(config) {
            if (this.checkAxesConfig(config.xAxisDim, "config.xAxisDim")) {
                for (let i = 0; i < this.scatterPlotList.length; i++) {
                    if (typeof config.xAxisDim === "string") {
                        this.scatterPlotList[i].setXColumn(this.spData.columns[config.xAxisDim]);
                    }
                    if (Array.isArray(config.xAxisDim)) {
                        this.scatterPlotList[i].setXColumn(this.spData.columns[config.xAxisDim[i]]);
                    }
                }
            }
            if (this.checkAxesConfig(config.yAxisDim, "config.yAxisDim")) {
                for (let i = 0; i < this.scatterPlotList.length; i++) {
                    if (typeof config.yAxisDim === "string") {
                        this.scatterPlotList[i].setYColumn(this.spData.columns[config.yAxisDim]);
                    }
                    if (Array.isArray(config.yAxisDim)) {
                        this.scatterPlotList[i].setYColumn(this.spData.columns[config.yAxisDim[i]]);
                    }
                }
            }
            if (this.checkAxesConfig(config.zAxisDim, "config.zAxisDim")) {
                for (let i = 0; i < this.scatterPlotList.length; i++) {
                    if (typeof config.zAxisDim === "string") {
                        this.scatterPlotList[i].setZColumn(this.spData.columns[config.zAxisDim]);
                    }
                    if (Array.isArray(config.zAxisDim)) {
                        const zAxisDim = config.zAxisDim[i];
                        this.scatterPlotList[i].setZColumn(zAxisDim ? this.spData.columns[zAxisDim] : null);
                    }
                }
            }
        }
        checkAxesConfig(configDim, configLabel) {
            if (typeof configDim === "string" &&
                typeof this.spData.columns[configDim] === "undefined") {
                console.error(`Unknown '${configLabel}':${configDim}`);
                return false;
            }
            if (Array.isArray(configDim)) {
                if (configDim.length !== 9) {
                    console.error(`'${configLabel}' is an array but its length is not 9`);
                    return false;
                }
                for (let i = 0; i < 9; i++) {
                    const axisDim = configDim[i];
                    if (typeof axisDim === "string" &&
                        typeof this.spData.columns[axisDim] === "undefined") {
                        console.error(`Unknown '${configLabel}[${i}]':${axisDim}`);
                        return false;
                    }
                }
            }
            return true;
        }
        rowFilterChange() {
            this.spData.updateCutRowsMask();
            this.scatterPlotList.forEach(plot => {
                const plotSelection = plot.plotSelection();
                plot.drawRegressionPlots(spm.ScatterPlot.DOMAIN, plotSelection);
                plot.drawDistribPlots(spm.ScatterPlot.DOMAIN, plotSelection);
                plot.drawVerViolinPlots(spm.ScatterPlot.DOMAIN, plotSelection);
                plot.drawHorViolinPlots(spm.ScatterPlot.DOMAIN, plotSelection);
                if (plotSelection.size() !== 0) {
                    plot.drawCanvas(false);
                }
            });
        }
        pushNewSP(i, j) {
            const scatterPlot = new spm.ScatterPlot(this.spData, {
                bindto: this.bindto,
                index: i + MultipleScatterPlot.grid.nCol * j,
                row: j,
                col: i,
                regressionType: this.regressionType,
                mouseMode: this.mouseMode,
                continuousCsId: this.continuousCsId,
                categoricalCsId: this.categoricalCsId,
                distribVisibility: this.distribVisibility,
                distribType: this.distribType,
                corrPlotType: "unused",
                corrPlotCsId: "unused",
                axisVisibility: {
                    xTitle: true,
                    xValues: true,
                    yTitle: true,
                    yValues: true
                },
                style: this.style
            });
            this.scatterPlotList.push(scatterPlot);
        }
        // eslint-disable-next-line max-lines-per-function
        buildMainDomElements(config) {
            const controlWidgets = config.controlWidgets ? config.controlWidgets : false;
            d3.select(this.bindto + " .mspDiv").remove();
            const mspDiv = d3.select(this.bindto).append("div")
                .attr("class", "mspDiv")
                .classed("withWidgets", controlWidgets)
                .classed("withoutWidgets", !controlWidgets);
            const controlDiv = mspDiv.append("div").attr("class", "controlDiv");
            const optionalPlotsDiv = controlDiv.append("div")
                .attr("class", "optionalPlotsDiv");
            optionalPlotsDiv.append("div")
                .attr("class", "distribRepDiv")
                .html("Distribution Representation: <span class=\"distribRepSelect\"></span>");
            optionalPlotsDiv.append("div")
                .attr("class", "linearRegrDiv")
                .html(`<input type="checkbox" id="${this.id()}_linearRegr" name="linearRegr"> <label for="${this.id()}_linearRegr">Linear Regression</label>`);
            optionalPlotsDiv.append("div")
                .attr("class", "loessDiv")
                .html(`<input type="checkbox" id="${this.id()}_loess" name="loess"> <label for="${this.id()}_loess">Local Polynomial Regression</label>`);
            optionalPlotsDiv.append("div")
                .attr("class", "contCsDiv")
                .html("Continuous Color Scale: <span class=\"contCsSelect\"></span>");
            optionalPlotsDiv.append("div")
                .html("Categorical Color Scale: <span class=\"catCsSelect\"></span>");
            const visibilityDiv = controlDiv.append("div")
                .attr("class", "visibilityDiv");
            visibilityDiv.append("div")
                .attr("class", "visibilityLabel")
                .text("Toggle plots to display");
            visibilityDiv.append("div")
                .attr("class", "visibilityPad");
            const selectionDiv = controlDiv.append("div")
                .attr("class", "selectionDiv");
            selectionDiv.append("div")
                .attr("class", "selectionLabel")
                .text("Select plots to customize");
            selectionDiv.append("div")
                .attr("class", "selectionPad");
            const customDiv = controlDiv.append("div")
                .attr("class", "customDiv");
            customDiv.append("div")
                .attr("class", "customLabel")
                .text("Customize selected plots");
            customDiv.append("div")
                .attr("class", "xParamDiv")
                .html("X Axis: <span class=\"ParamSelect XAxis\"></span>");
            customDiv.append("div")
                .attr("class", "yParamDiv")
                .html("Y Axis: <span class=\"ParamSelect YAxis\"></span>");
            customDiv.append("div")
                .attr("class", "zAxisUsedDiv")
                .html(`<input type="checkbox" id="${this.id()}_zAxisUsed" name="zAxisUsed" checked> <label for="${this.id()}_zAxisUsed">Use Z Axis</label>`);
            customDiv.append("div")
                .attr("class", "zParamDiv")
                .html("Z Axis: <span class=\"ParamSelect ZAxis\"></span>");
            const csDiv = controlDiv.append("div")
                .attr("class", "csDiv");
            csDiv.append("div")
                .attr("class", "legendVisibilityDiv")
                .html(`<input type="checkbox" id="${this.id()}_legendVisibility" name="legendVisibility" checked> <label for="${this.id()}_legendVisibility">Display Legend</label>`);
            csDiv.append("div")
                .attr("class", "mouseModeDiv")
                .html("Mouse mode: <span class=\"mouseModeSelect\"></span>");
            mspDiv.append("div").attr("class", "MultiPlot");
        }
        appendPlotSvg() {
            const mspSvg = d3.select(this.bindto + " .MultiPlot")
                .append("svg")
                .attr("width", this.width + MultipleScatterPlot.margin.l + MultipleScatterPlot.margin.r)
                .attr("height", this.height + MultipleScatterPlot.margin.b + MultipleScatterPlot.margin.t);
            mspSvg.append("g")
                .attr("transform", "translate(" + MultipleScatterPlot.margin.l + "," + MultipleScatterPlot.margin.t + ")")
                .attr("class", "mspGroup");
            this.appendSvgDefs();
        }
        appendSvgDefs() {
            const svg = d3.select(this.bindto + " svg");
            const defs = svg.append("defs");
            defs.append("clipPath")
                .attr("id", "tile-clip")
                .append("rect");
            defs.append("clipPath")
                .attr("id", "x-clip")
                .append("rect");
            defs.append("clipPath")
                .attr("id", "y-clip")
                .append("rect");
        }
        // eslint-disable-next-line max-lines-per-function
        updatePlots(updateType) {
            const thisMPlot = this;
            const mspGroup = d3.select(this.bindto + " .MultiPlot .mspGroup");
            if (updateType & (spm.ScatterPlot.INIT | spm.ScatterPlot.RANGE)) {
                const tileWidth = (this.width / this.visibilityPad.nCol)
                    - spm.ScatterPlot.margin.l - spm.ScatterPlot.margin.r
                    - (this.legendVisibility ? spm.ScatterPlot.cslTotalWidth : 0);
                const tileHeight = (this.height / thisMPlot.visibilityPad.nRow)
                    - spm.ScatterPlot.margin.t - spm.ScatterPlot.margin.b;
                this.scatterPlotList.forEach(plot => {
                    plot.height = tileHeight;
                    plot.width = tileWidth;
                });
                d3.select(this.bindto + " #tile-clip > rect")
                    .attr("x", tileWidth * this.distribRatio())
                    .attr("y", spm.ScatterPlot.padding.t)
                    .attr("width", tileWidth * (1 - this.distribRatio()) - spm.ScatterPlot.padding.r)
                    .attr("height", tileHeight * (1 - this.distribRatio()) - spm.ScatterPlot.padding.t);
                d3.select(this.bindto + " #x-clip > rect")
                    .attr("x", tileWidth * this.distribRatio())
                    .attr("y", tileHeight * (1 - this.distribRatio()))
                    .attr("width", tileWidth * (1 - this.distribRatio()) - spm.ScatterPlot.padding.r)
                    .attr("height", tileHeight * this.distribRatio());
                d3.select(this.bindto + " #y-clip > rect")
                    .attr("y", spm.ScatterPlot.padding.t)
                    .attr("width", tileWidth * this.distribRatio())
                    .attr("height", tileHeight * (1 - this.distribRatio()) - spm.ScatterPlot.padding.t);
            }
            if (updateType & spm.ScatterPlot.INIT) {
                const scatterPlot = mspGroup.selectAll(".scatterPlot")
                    .data(this.visibilityPad.visibleScatterPlots())
                    .enter().append("g")
                    .attr("class", "scatterPlot")
                    .attr("transform", function (plot) {
                    return `translate(${thisMPlot.xScatterPlot(plot)}, ${thisMPlot.yScatterPlot(plot)})`;
                });
                scatterPlot.append("g")
                    .attr("class", "axesGroup");
                scatterPlot.append("g")
                    .attr("class", "spArea")
                    .attr("clip-path", "url(#tile-clip)");
                scatterPlot.append("g")
                    .attr("class", "distribGroup");
            }
            mspGroup.selectAll(".scatterPlot").each(function (plot) {
                plot.draw(updateType);
            });
            mspGroup.selectAll(".cslGroup").style("display", this.legendVisibility ? "block" : "none");
            if (updateType & spm.ScatterPlot.INIT) {
                this.fixBrush();
                this.style.applyCssRules();
            }
        }
        xScatterPlot(plot) {
            const vcell = this.visibilityPad.vcell(plot);
            return vcell.vcol * this.width / this.visibilityPad.nCol + spm.ScatterPlot.margin.l;
        }
        yScatterPlot(plot) {
            const vcell = this.visibilityPad.vcell(plot);
            return vcell.vrow * this.height / this.visibilityPad.nRow + spm.ScatterPlot.margin.t;
        }
        distribRatio() {
            return this.distribVisibility
                ? spm.ScatterPlot.DISTRIB_RATIO
                : 0;
        }
        //************************************
        //********** Axes Selectors **********
        //************************************
        appendXAxisSelector() {
            const thisMPlot = this;
            const plot = [...this.selectionPad.selectedPlots][0];
            d3.select(this.bindto + " .ParamSelect.XAxis")
                .append("select")
                .on("change", function () {
                thisMPlot.setXAxis(thisMPlot.spData.dimensions[this.selectedIndex]);
            })
                .selectAll("option")
                .data(this.spData.dimensions)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            const paramIndex = this.spData.dimensions.indexOf(plot.xColumn.dim);
            d3.select(this.bindto + " .ParamSelect.XAxis > select")
                .property("selectedIndex", paramIndex);
        }
        appendYAxisSelector() {
            const thisMPlot = this;
            const plot = [...this.selectionPad.selectedPlots][0];
            d3.select(this.bindto + " .ParamSelect.YAxis")
                .append("select")
                .on("change", function () {
                thisMPlot.setYAxis(thisMPlot.spData.dimensions[this.selectedIndex]);
            })
                .selectAll("option")
                .data(this.spData.dimensions)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            const paramIndex = this.spData.dimensions.indexOf(plot.yColumn.dim);
            d3.select(this.bindto + " .ParamSelect.YAxis > select")
                .property("selectedIndex", paramIndex);
        }
        appendZAxisSelector() {
            const thisMPlot = this;
            const plot = [...this.selectionPad.selectedPlots][0];
            d3.select(this.bindto + " .ParamSelect.ZAxis")
                .append("select")
                .on("change", function () {
                thisMPlot.setZAxis(thisMPlot.spData.dimensions[this.selectedIndex]);
            })
                .selectAll("option")
                .data(this.spData.dimensions)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            if (plot.zColumn !== null) {
                const paramIndex = this.spData.dimensions.indexOf(plot.zColumn.dim);
                d3.select(this.bindto + " .ParamSelect.ZAxis > select")
                    .property("selectedIndex", paramIndex);
            }
        }
        //********************************************************************
        //********** About "LegendVisibility/ZAxisUsed" check boxes **********
        //********************************************************************
        initLegendVisibilityCB() {
            const thisMPlot = this;
            d3.select(`#${this.id()}_legendVisibility`)
                .property("checked", this.legendVisibility)
                .on("change", function () {
                thisMPlot.legendVisibility = d3.select(this).property("checked");
                // 'range' is to update when the visibility of color scale legend is changed
                thisMPlot.updatePlots(spm.ScatterPlot.RANGE);
            });
        }
        initZAxisUsedCB() {
            const plot = [...this.selectionPad.selectedPlots][0];
            d3.select(`#${this.id()}_zAxisUsed`)
                .property("checked", plot.zColumn !== null)
                .on("change", MultipleScatterPlot.prototype.updateZAxisFromGui.bind(this));
        }
        updateZAxisFromGui() {
            if (d3.select(`#${this.id()}_zAxisUsed`).property("checked")) {
                const zAxisSelectNode = d3.select(this.bindto + " .ParamSelect.ZAxis>select").node();
                if (zAxisSelectNode) {
                    this.setZAxis(this.spData.dimensions[zAxisSelectNode.selectedIndex]);
                }
            }
            else {
                this.setZAxis(null);
            }
        }
        changeLegendVisibility(visible) {
            this.legendVisibility = visible;
            // 'range' is to update when the visibility of color scale legend is changed
            this.updatePlots(spm.ScatterPlot.RANGE);
        }
        //******************************************************
        //********** "Tooltip/Filter/Zoom" select box **********
        //******************************************************
        appendMouseModeSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .mouseModeSelect").append("select")
                .on("change", MultipleScatterPlot.prototype.mouseModeChange.bind(thisMPlot))
                .selectAll("option")
                .data(spm.SpConst.mouseModeList)
                .enter().append("option")
                .text(function (d) { return d.label; })
                .attr("value", function (d) { return d.key; });
            this.mouseModeChange();
        }
        mouseModeChange() {
            const mouseModeSelect = d3.select(this.bindto + " .mouseModeSelect > select").node();
            if (mouseModeSelect) {
                this.changeMouseMode(mouseModeSelect.options[mouseModeSelect.selectedIndex].value);
            }
        }
        changeMouseMode(mouseMode) {
            this.mouseMode = mouseMode;
            this.scatterPlotList.forEach(plot => {
                plot.changeMouseMode(mouseMode);
            });
            const modeIndex = spm.SpConst.mouseModeList.findIndex(mode => mode.key === mouseMode);
            d3.select(this.bindto + " .mouseModeSelect > select")
                .property("selectedIndex", modeIndex);
            this.fixBrush();
        }
        fixBrush() {
            this.scatterPlotList.forEach(plot => {
                plot.fixBrush();
            });
        }
        //***************************************************************
        //********** About "distribRep/Regression" check boxes **********
        //***************************************************************
        appendDistribRepSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .distribRepSelect").append("select")
                .on("change", function () {
                const rep = spm.SpConst.distribRepList[this.selectedIndex];
                thisMPlot.setDistribType(rep.key === spm.SpConst.histogramRep.key ? spm.DistributionPlot.HISTO_REP : spm.DistributionPlot.DENS_REP);
            })
                .selectAll("option")
                .data(spm.SpConst.distribRepList)
                .enter().append("option")
                .text(function (d) { return d.label; })
                .attr("value", function (d) { return d.key; });
            const histoRep = (this.distribType & spm.DistributionPlot.HISTO_REP) ? spm.SpConst.histogramRep.key : spm.SpConst.densityRep.key;
            const repIndex = spm.SpConst.distribRepList.findIndex(distribRep => distribRep.key === histoRep);
            d3.select(this.bindto + " .distribRepSelect > select")
                .property("selectedIndex", repIndex);
        }
        initRegressionCB() {
            const thisMPlot = this;
            d3.select(`#${this.id()}_linearRegr`)
                .property("checked", (this.regressionType & spm.RegressionPlot.LINEAR_REP) !== 0)
                .on("change", function () {
                if (d3.select(this).property("checked")) {
                    thisMPlot.setRegressionType(thisMPlot.regressionType | spm.RegressionPlot.LINEAR_REP);
                }
                else {
                    thisMPlot.setRegressionType(thisMPlot.regressionType ^ spm.RegressionPlot.LINEAR_REP);
                }
            });
            d3.select(`#${this.id()}_loess`)
                .property("checked", (this.regressionType & spm.RegressionPlot.LOESS_REP) !== 0)
                .on("change", function () {
                if (d3.select(this).property("checked")) {
                    thisMPlot.setRegressionType(thisMPlot.regressionType | spm.RegressionPlot.LOESS_REP);
                }
                else {
                    thisMPlot.setRegressionType(thisMPlot.regressionType ^ spm.RegressionPlot.LOESS_REP);
                }
            });
        }
        appendContCsSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .contCsSelect").append("select")
                .on("change", function () {
                const contCsKey = spm.SpConst.CONTINUOUS_CS_IDS[this.selectedIndex];
                thisMPlot.setContinuousColorScale(contCsKey);
            })
                .selectAll("option")
                .data(spm.SpConst.CONTINUOUS_CS_IDS)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            const contCsIndex = spm.SpConst.CONTINUOUS_CS_IDS.indexOf(this.continuousCsId);
            d3.select(this.bindto + " .contCsSelect > select")
                .property("selectedIndex", contCsIndex);
        }
        appendCatCsSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .catCsSelect").append("select")
                .on("change", function () {
                const catCsKey = spm.SpConst.CATEGORIAL_CS_IDS[this.selectedIndex];
                thisMPlot.setCategoricalColorScale(catCsKey);
            })
                .selectAll("option")
                .data(spm.SpConst.CATEGORIAL_CS_IDS)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            const catCsIndex = spm.SpConst.CATEGORIAL_CS_IDS.indexOf(this.categoricalCsId);
            d3.select(this.bindto + " .catCsSelect > select")
                .property("selectedIndex", catCsIndex);
        }
        //**************************************************
        //********** API (called by R htmlwidget) **********
        //**************************************************
        setXAxis(dim) {
            this.selectionPad.setXAxis(dim);
        }
        setYAxis(dim) {
            this.selectionPad.setYAxis(dim);
        }
        setZAxis(dim) {
            this.selectionPad.setZAxis(dim);
        }
        setDistribType(distribType) {
            if (distribType & (spm.DistributionPlot.HISTO_REP | spm.DistributionPlot.DENS_REP)) {
                this.distribType = distribType;
                this.scatterPlotList.forEach(plot => {
                    plot.distribRepChange(distribType);
                });
            }
            else {
                console.error("Invalid distribution type code: " + distribType);
            }
        }
        setRegressionType(regressionType) {
            if (regressionType === 0 || regressionType & (spm.RegressionPlot.LINEAR_REP | spm.RegressionPlot.LOESS_REP)) {
                this.regressionType = regressionType;
                this.scatterPlotList.forEach(plot => {
                    plot.regressionRepChange(regressionType);
                });
            }
            else {
                console.error("Invalid regression type code: " + regressionType);
            }
        }
        setContinuousColorScale(continuousCsId) {
            if (spm.SpConst.CONTINUOUS_CS_IDS.includes(continuousCsId)) {
                this.continuousCsId = continuousCsId;
                this.scatterPlotList.forEach(plot => {
                    plot.continuousCsId = continuousCsId;
                });
                this.updatePlots(spm.ScatterPlot.PALETTE);
            }
            else {
                console.error("Unknown continuous color scale: " + continuousCsId);
            }
        }
        setCategoricalColorScale(categoricalCsId) {
            if (spm.SpConst.CATEGORIAL_CS_IDS.includes(categoricalCsId)) {
                this.categoricalCsId = categoricalCsId;
                this.scatterPlotList.forEach(plot => {
                    plot.categoricalCsId = categoricalCsId;
                });
                this.updatePlots(spm.ScatterPlot.PALETTE);
            }
            else {
                console.error("Unknown categorical color scale: " + categoricalCsId);
            }
        }
        setCutoffs(spCutoffsList) {
            this.spData.setCutoffs(spCutoffsList);
            this.fixBrush();
        }
        getPlotConfig() {
            const allDimensions = d3.keys(this.spData.sampleData[0]);
            return {
                data: [],
                rowLabels: this.spData.rowLabels,
                categorical: allDimensions.map(dim => this.spData.columns[dim]
                    ? this.spData.columns[dim].categories
                    : null),
                inputColumns: allDimensions.map(dim => this.spData.columns[dim] && this.spData.columns[dim].ioType === spm.Column.INPUT),
                keptColumns: allDimensions.map(dim => this.spData.dimensions.includes(dim)),
                cutoffs: this.spData.getXYCutoffs(),
                xAxisDim: this.scatterPlotList.map(sp => sp.xColumn.dim),
                yAxisDim: this.scatterPlotList.map(sp => sp.yColumn.dim),
                zAxisDim: this.scatterPlotList.map(sp => sp.zColumn ? sp.zColumn.dim : null),
                visiblePlots: this.scatterPlotList.map(sp => this.visibilityPad.visible(sp)),
                distribType: this.distribType,
                regressionType: this.regressionType,
                columnLabels: allDimensions.map(dim => this.spData.columns[dim]
                    ? this.spData.columns[dim].label
                    : dim),
                continuousCS: this.continuousCsId,
                categoricalCS: this.categoricalCsId,
                legendVisibility: this.legendVisibility,
                visibilityPadId: "",
                selectionPadId: "",
                controlWidgets: d3.select(this.bindto + " .mspDiv").classed("withWidgets"),
                cssRules: this.style.cssRules,
                plotProperties: this.style.plotProperties
            };
        }
    }
    MultipleScatterPlot.margin = { t: 15, r: 10, b: 0, l: 0 };
    MultipleScatterPlot.axesClasses = ["XAxis", "YAxis", "ZAxis"];
    MultipleScatterPlot.grid = { nCol: 3, nRow: 3 };
    MultipleScatterPlot.PLOT_EVENT = "plotEvent";
    MultipleScatterPlot.SELECTION_EVENT = "selectionChange";
    spm.MultipleScatterPlot = MultipleScatterPlot;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class PointsPlot {
        constructor(scatterPlot) {
            this.scatterPlot = scatterPlot;
        }
        // eslint-disable-next-line max-lines-per-function
        mouseover(row, i, scatterPlotList) {
            if (this.scatterPlot.mouseMode !== spm.SpConst.tooltipMouse.key) {
                return;
            }
            const tooltipTitle = this.scatterPlot.spData.rowLabels
                ? this.scatterPlot.spData.rowLabels[i]
                : "Point " + (i + 1);
            const tooltipLocation = this.tooltipLocation();
            d3.select(this.scatterPlot.bindto + " .mspTooltip").remove();
            const mspDiv = d3.select(this.scatterPlot.bindto + " .mspDiv");
            const tooltip = mspDiv.append("div")
                .attr("class", "mspTooltip")
                .style("display", "block")
                .style("left", tooltipLocation[0] + "px")
                .style("top", tooltipLocation[1] + "px");
            tooltip.append("div")
                .attr("class", "pointIndex title")
                .html(tooltipTitle);
            const xDimSet = new Set(scatterPlotList.map(sp => sp.xColumn.dim));
            const yDimSet = new Set(scatterPlotList.map(sp => sp.yColumn.dim));
            const dimsToPrint = this.scatterPlot.spData.dimensions.filter(dim => xDimSet.has(dim) || yDimSet.has(dim));
            if (this.scatterPlot.zColumn !== null && this.scatterPlot.zColumn.categories && !dimsToPrint.includes(this.scatterPlot.zColumn.dim)) {
                dimsToPrint.push(this.scatterPlot.zColumn.dim);
            }
            dimsToPrint.forEach(dim => {
                const column = this.scatterPlot.spData.columns[dim];
                if (this.scatterPlot.zColumn !== null && this.scatterPlot.zColumn.categories && dim === this.scatterPlot.zColumn.dim) {
                    const catIndex = row[this.scatterPlot.zColumn.dim];
                    const category = this.scatterPlot.zColumn.categories[catIndex];
                    const coloringHtml = `<span class='swatch' style='background:${this.scatterPlot.pointColor(row)}'>&nbsp;</span>`;
                    const axisName = (dim === this.scatterPlot.xColumn.dim || dim === this.scatterPlot.yColumn.dim)
                        ? "axisName"
                        : "zAxisName";
                    tooltip.append("div")
                        .html(`<span class="${axisName}">${this.scatterPlot.zColumn.labelText()}</span> ${coloringHtml}&nbsp;: ${PointsPlot.brIze(category)}`);
                }
                else if (dim === this.scatterPlot.xColumn.dim) {
                    tooltip.append("div")
                        .html(`<span class="axisName">${column.labelText()}</span>: <span class="xValue">${column.formatedRowValue(row)}</span>`);
                }
                else if (dim === this.scatterPlot.yColumn.dim) {
                    tooltip.append("div")
                        .html(`<span class="axisName">${column.labelText()}</span>: <span class="yValue">${column.formatedRowValue(row)}</span>`);
                }
                else {
                    tooltip.append("div")
                        .html(`${column.labelText()}: ${column.formatedRowValue(row)}`);
                }
            });
        }
        static brIze(category) {
            const splittedCategory = category.toString().split(", ");
            if (splittedCategory.length > 1) {
                return "<br>" + splittedCategory.join(",<br>") + "<br>";
            }
            return splittedCategory[0];
        }
        tooltipLocation() {
            const mspDivNode = d3.select(this.scatterPlot.bindto + " .mspDiv").node();
            const parentBounds = (mspDivNode === null) ? null : mspDivNode.getBoundingClientRect();
            const xParent = (parentBounds === null) ? 0 : parentBounds.x;
            const plotGroup = d3.select(this.scatterPlot.bindto + " .mspGroup").node();
            const elementBounds = (plotGroup === null) ? null : plotGroup.getBoundingClientRect();
            const xRect = (elementBounds === null) ? 0 : elementBounds.x;
            const wRect = (elementBounds === null) ? 0 : elementBounds.width;
            return [xRect - xParent + wRect + 5, this.scatterPlot.yPlot];
        }
        mouseout() {
            if (this.scatterPlot.mouseMode !== spm.SpConst.tooltipMouse.key) {
                return;
            }
            d3.select(this.scatterPlot.bindto + " .mspTooltip").style("display", "none");
        }
        addAlpha(color) {
            const d3Color = d3.color(color);
            if (d3Color) {
                d3Color.opacity = this.scatterPlot.style.plotProperties.point.alpha;
                return d3Color.toString();
            }
            return color;
        }
        drawCanvas(picking) {
            const canvasSelector = this.scatterPlot.canvasSelector(picking);
            const canvasNode = d3.select(canvasSelector).node();
            if (!canvasNode) {
                console.error("canvasNode is null for:", canvasSelector);
                return;
            }
            const context2d = canvasNode.getContext("2d");
            if (!context2d) {
                console.error("context2d is null");
                return;
            }
            const thisPlot = this;
            const xScaleRange = this.scatterPlot.xScale.range();
            const yScaleRange = this.scatterPlot.yScale.range();
            const xPlot = -xScaleRange[0];
            const yPlot = -yScaleRange[1];
            // First, draw points which are not selected by brushes
            this.scatterPlot.spData.sampleData
                .forEach(function (row, i) {
                if (!thisPlot.scatterPlot.spData.cutRows[i]) {
                    if (picking) {
                        thisPlot.drawPickingPoint(row, i, xPlot, yPlot, context2d);
                    }
                    else {
                        thisPlot.drawPoint(row, i, xPlot, yPlot, context2d, true);
                    }
                }
            });
            // Second, draw points which are selected by brushes
            this.scatterPlot.spData.sampleData
                .forEach(function (row, i) {
                if (thisPlot.scatterPlot.spData.cutRows[i]) {
                    if (picking) {
                        thisPlot.drawPickingPoint(row, i, xPlot, yPlot, context2d);
                    }
                    else {
                        thisPlot.drawPoint(row, i, xPlot, yPlot, context2d, false);
                    }
                }
            });
        }
        drawPoint(row, rowIndex, xPlot, yPlot, context2d, useWatermark) {
            const cx = this.scatterPlot.cx(row, rowIndex) + xPlot;
            const cy = this.scatterPlot.cy(row, rowIndex) + yPlot;
            const r = this.scatterPlot.style.plotProperties.point.radius;
            context2d.beginPath();
            context2d.arc(cx, cy, r, 0, 2 * Math.PI, false);
            context2d.fillStyle = useWatermark
                ? this.scatterPlot.style.plotProperties.watermarkColor
                : this.addAlpha(this.scatterPlot.pointColor(row));
            context2d.fill();
        }
        drawPickingPoint(row, rowIndex, xPlot, yPlot, context2d) {
            const size = 2 * this.scatterPlot.style.plotProperties.point.radius;
            const x = Math.round(this.scatterPlot.cx(row, rowIndex) + xPlot - this.scatterPlot.style.plotProperties.point.radius);
            const y = Math.round(this.scatterPlot.cy(row, rowIndex) + yPlot - this.scatterPlot.style.plotProperties.point.radius);
            context2d.beginPath();
            context2d.rect(x, y, size, size);
            context2d.fillStyle = PointsPlot.pickingColor(rowIndex);
            context2d.fill();
        }
        static pickingColor(dataIndex) {
            const hex = "00000" + dataIndex.toString(16);
            const pickingColor = "#" + hex.substring(hex.length - 6);
            return pickingColor;
        }
    }
    spm.PointsPlot = PointsPlot;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class RegressionData {
        constructor(regressionPlot, zFilter) {
            this.uncutLinearRegression = null;
            this.cutLinearRegression = null;
            this.uncutLoessRegression = null;
            this.cutLoessRegression = null;
            this.plot = regressionPlot;
            this.zFilter = zFilter;
        }
        computePlot() {
            const xDim = this.plot.scatterPlot.xColumn.dim;
            const yDim = this.plot.scatterPlot.yColumn.dim;
            // Compute uncut plot
            const filteredUncutData = this.filterData(this.plot.scatterPlot.spData.sampleData);
            if (this.plot.useLinearRep()) {
                // @ts-ignore
                this.uncutLinearRegression = d3.regressionLinear()
                    .x((d) => d[xDim])
                    .y((d) => d[yDim])(filteredUncutData);
            }
            if (this.plot.useLoessRep()) {
                // @ts-ignore
                this.uncutLoessRegression = d3.regressionLoess()
                    .x((d) => d[xDim])
                    .y((d) => d[yDim])
                    .bandwidth(RegressionData.bandwidth)(filteredUncutData);
            }
            // Compute cut plot
            const filteredCutData = this.filterData(this.plot.scatterPlot.spData.cutData());
            if (this.plot.useLinearRep()) {
                // @ts-ignore
                this.cutLinearRegression = d3.regressionLinear()
                    .x((d) => d[xDim])
                    .y((d) => d[yDim])(filteredCutData);
            }
            if (this.plot.useLoessRep()) {
                // @ts-ignore
                this.cutLoessRegression = d3.regressionLoess()
                    .x((d) => d[xDim])
                    .y((d) => d[yDim])
                    .bandwidth(RegressionData.bandwidth)(filteredCutData);
            }
            return this;
        }
        filterData(data) {
            return this.zFilter ? data.filter(this.zFilter) : data;
        }
    }
    RegressionData.bandwidth = 0.75;
    spm.RegressionData = RegressionData;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class RegressionPlot {
        constructor(scatterPlot) {
            this.scatterPlot = scatterPlot;
            this.mainRegr = new spm.RegressionData(this);
            this.subRegrList = [];
            this.subColorScale = spm.SpConst.CATEGORIAL_CS[this.scatterPlot.categoricalCsId];
        }
        generate(plotSelection) {
            const regrPlot = plotSelection.append("g").attr("class", "regressionPlots");
            const uncut = regrPlot.append("g").attr("class", "uncut");
            const cut = regrPlot.append("g").attr("class", "cut");
            // About linear regression
            cut.append("g").attr("class", "linearGroup");
            cut.append("g").attr("class", "subLinearGroup");
            uncut.append("g").attr("class", "linearGroup");
            uncut.append("g").attr("class", "subLinearGroup");
            // About loess regression
            cut.append("g").attr("class", "loessGroup");
            cut.append("g").attr("class", "subLoessGroup");
            uncut.append("g").attr("class", "loessGroup");
            uncut.append("g").attr("class", "subLoessGroup");
            return this;
        }
        useLinearRep() {
            return (this.scatterPlot.regressionType & RegressionPlot.LINEAR_REP) !== 0
                && this.scatterPlot.xColumn.categories === null
                && this.scatterPlot.yColumn.categories === null;
        }
        useLoessRep() {
            return (this.scatterPlot.regressionType & RegressionPlot.LOESS_REP) !== 0
                && this.scatterPlot.xColumn.categories === null
                && this.scatterPlot.yColumn.categories === null;
        }
        colorScale(colorScale) {
            this.subColorScale = colorScale;
            return this;
        }
        update(_updateType, regrGroup) {
            this.updateLinearPlot(regrGroup, RegressionPlot.SUB_FILTER);
            this.updateLinearPlot(regrGroup, RegressionPlot.SUB_FILTER | RegressionPlot.CUT_FILTER);
            this.updateLinearPlot(regrGroup, RegressionPlot.CUT_FILTER);
            this.updateLinearPlot(regrGroup, RegressionPlot.NO_FILTER);
            this.updateLoessPlot(regrGroup, RegressionPlot.SUB_FILTER);
            this.updateLoessPlot(regrGroup, RegressionPlot.SUB_FILTER | RegressionPlot.CUT_FILTER);
            this.updateLoessPlot(regrGroup, RegressionPlot.CUT_FILTER);
            this.updateLoessPlot(regrGroup, RegressionPlot.NO_FILTER);
        }
        plotVisibility(filtering) {
            // if plot is about data coming from cutoff
            if (filtering & RegressionPlot.CUT_FILTER) {
                // if plot is about data coming from sub data
                if (filtering & RegressionPlot.SUB_FILTER) {
                    // plot is visible if sub data are available
                    return this.subRegrList.length !== 0;
                }
                else {
                    return true;
                }
            }
            // if plot is not about data coming from cutoff
            else {
                return false;
            }
        }
        updateLinearPlot(regrGroup, filtering) {
            const thisPlot = this;
            const dataList = filtering & RegressionPlot.SUB_FILTER ? this.subRegrList : [this.mainRegr];
            const cutClass = filtering & RegressionPlot.CUT_FILTER ? ".cut" : ".uncut";
            const linearClass = filtering & RegressionPlot.SUB_FILTER ? ".subLinearGroup" : ".linearGroup";
            const plotGroup = regrGroup.select(`${cutClass} ${linearClass}`);
            if (this.useLinearRep() && this.plotVisibility(filtering)) {
                plotGroup.style("display", null);
            }
            else {
                plotGroup.style("display", "none");
                return;
            }
            plotGroup.selectAll("line").data(dataList)
                .join(enter => enter.append("line"), update => update, exit => exit.remove())
                .attr("stroke-width", this.scatterPlot.style.plotProperties.regression.strokeWidth)
                .attr("stroke", (_data, i) => this.strokeColor(i, filtering))
                .attr("fill", "none")
                .attr("x1", data => thisPlot.linearRegressionX1(data, filtering))
                .attr("x2", data => thisPlot.linearRegressionX2(data, filtering))
                .attr("y1", data => thisPlot.linearRegressionY1(data, filtering))
                .attr("y2", data => thisPlot.linearRegressionY2(data, filtering))
                .on("mouseover", function (regressionData, i) {
                thisPlot.mouseoverLinear(this, regressionData, i, filtering);
            })
                .on("mouseout", function (_data, i) {
                if (thisPlot.scatterPlot.mouseMode === spm.SpConst.tooltipMouse.key) {
                    d3.select(thisPlot.scatterPlot.bindto + " .mspTooltip").style("display", "none");
                    d3.select(this).attr("stroke", () => thisPlot.strokeColor(i, filtering));
                }
            });
        }
        strokeColor(catIndex, filtering, k) {
            if (!(filtering & RegressionPlot.CUT_FILTER)) {
                return null;
            }
            const color = (filtering & RegressionPlot.SUB_FILTER)
                ? this.subColorScale(catIndex)
                : this.scatterPlot.style.plotProperties.noCatColor;
            const d3Color = d3.color(color);
            return d3Color ? d3Color.darker(k).toString() : color;
        }
        linearRegressionX1(data, filtering) {
            const linearRegression = RegressionPlot.linearRegression(data, filtering);
            return this.xScale(linearRegression[0][0]);
        }
        linearRegressionX2(data, filtering) {
            const linearRegression = RegressionPlot.linearRegression(data, filtering);
            return this.xScale(linearRegression[1][0]);
        }
        linearRegressionY1(data, filtering) {
            const linearRegression = RegressionPlot.linearRegression(data, filtering);
            return this.yScale(linearRegression[0][1]);
        }
        linearRegressionY2(data, filtering) {
            const linearRegression = RegressionPlot.linearRegression(data, filtering);
            return this.yScale(linearRegression[1][1]);
        }
        mouseoverLinear(line, data, i, filtering) {
            if (this.scatterPlot.mouseMode !== spm.SpConst.tooltipMouse.key) {
                return;
            }
            d3.select(line).attr("stroke", () => this.strokeColor(i, filtering, 2));
            const rSquared = (data.cutLinearRegression)
                ? spm.ExpFormat.format(data.cutLinearRegression.rSquared)
                : "Not computed";
            const tooltipLocation = this.tooltipLocation();
            d3.select(this.scatterPlot.bindto + " .mspTooltip").remove();
            const mspDiv = d3.select(this.scatterPlot.bindto + " .mspDiv");
            const tooltip = mspDiv.append("div")
                .attr("class", "mspTooltip")
                .style("display", "block")
                .style("left", tooltipLocation[0] + "px")
                .style("top", tooltipLocation[1] + "px");
            tooltip.append("div")
                .attr("class", "title")
                .html("Linear Regression");
            if (filtering & RegressionPlot.SUB_FILTER && this.scatterPlot.zColumn !== null) {
                const coloringHtml = `<span class='swatch' style='background:${this.strokeColor(i, filtering)}'>&nbsp;</span>`;
                const category = this.scatterPlot.zColumn.categories
                    ? this.scatterPlot.zColumn.categories[i]
                    : "undefined";
                tooltip.append("div")
                    .html(`where ${this.scatterPlot.zColumn.labelText()} is: ${RegressionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
            }
            if (filtering & RegressionPlot.CUT_FILTER) {
                const filteredUncutData = data.filterData(this.scatterPlot.spData.sampleData);
                const filteredCutData = data.filterData(this.scatterPlot.spData.cutData());
                tooltip.append("div")
                    .html(`${filteredCutData.length} sample points (${filteredUncutData.length - filteredCutData.length} filtered points)`);
            }
            tooltip.append("div")
                .html(`<span class="r2">=> r2</span>: <span class="r2Value">${rSquared}</span>`);
        }
        static brIze(category) {
            const splittedCategory = category.toString().split(", ");
            if (splittedCategory.length > 1) {
                return "<br>" + splittedCategory.join(",<br>") + "<br>";
            }
            return splittedCategory[0];
        }
        tooltipLocation() {
            const mspDivNode = d3.select(this.scatterPlot.bindto + " .mspDiv").node();
            const parentBounds = (mspDivNode === null) ? null : mspDivNode.getBoundingClientRect();
            const xParent = (parentBounds === null) ? 0 : parentBounds.x;
            const plotGroup = d3.select(this.scatterPlot.bindto + " .mspGroup").node();
            const elementBounds = (plotGroup === null) ? null : plotGroup.getBoundingClientRect();
            const xRect = (elementBounds === null) ? 0 : elementBounds.x;
            const wRect = (elementBounds === null) ? 0 : elementBounds.width;
            return [xRect - xParent + wRect + 5, this.scatterPlot.yPlot];
        }
        updateLoessPlot(regrGroup, filtering) {
            const thisPlot = this;
            const dataList = filtering & RegressionPlot.SUB_FILTER ? this.subRegrList : [this.mainRegr];
            const cutClass = filtering & RegressionPlot.CUT_FILTER ? ".cut" : ".uncut";
            const loessClass = filtering & RegressionPlot.SUB_FILTER ? ".subLoessGroup" : ".loessGroup";
            const plotGroup = regrGroup.select(`${cutClass} ${loessClass}`);
            if (this.useLoessRep() && this.plotVisibility(filtering)) {
                plotGroup.style("display", null);
            }
            else {
                plotGroup.style("display", "none");
                return;
            }
            plotGroup.selectAll("path").data(dataList)
                .join(enter => enter.append("path"), update => update, exit => exit.remove())
                .attr("stroke-width", this.scatterPlot.style.plotProperties.regression.strokeWidth)
                .attr("stroke", (_data, i) => this.strokeColor(i, filtering))
                .attr("fill", "none")
                .attr("d", function (data) {
                const loessRegression = RegressionPlot.loessRegression(data, filtering);
                const lineGenerator = d3.line()
                    .curve(d3.curveBasis)
                    .x(d => thisPlot.xScale(d[0]))
                    .y(d => thisPlot.yScale(d[1]));
                return lineGenerator(loessRegression);
            })
                .on("mouseover", function (regressionData, i) {
                thisPlot.mouseoverLoess(this, regressionData, i, filtering);
            })
                .on("mouseout", function (_data, i) {
                if (thisPlot.scatterPlot.mouseMode === spm.SpConst.tooltipMouse.key) {
                    d3.select(thisPlot.scatterPlot.bindto + " .mspTooltip").style("display", "none");
                    d3.select(this).attr("stroke", () => thisPlot.strokeColor(i, filtering));
                }
            });
        }
        xScale(value) {
            const scaled = this.scatterPlot.xScale(value);
            return typeof scaled === "undefined" ? NaN : scaled;
        }
        yScale(value) {
            const scaled = this.scatterPlot.yScale(value);
            return typeof scaled === "undefined" ? NaN : scaled;
        }
        mouseoverLoess(path, data, i, filtering) {
            if (this.scatterPlot.mouseMode !== spm.SpConst.tooltipMouse.key) {
                return;
            }
            d3.select(path).attr("stroke", () => this.strokeColor(i, filtering, 2));
            const tooltipLocation = this.tooltipLocation();
            d3.select(this.scatterPlot.bindto + " .mspTooltip").remove();
            const mspDiv = d3.select(this.scatterPlot.bindto + " .mspDiv");
            const tooltip = mspDiv.append("div")
                .attr("class", "mspTooltip")
                .style("display", "block")
                .style("left", tooltipLocation[0] + "px")
                .style("top", tooltipLocation[1] + "px");
            tooltip.append("div")
                .attr("class", "title")
                .html("Local Polynomial Regression");
            if (filtering & RegressionPlot.SUB_FILTER && this.scatterPlot.zColumn !== null) {
                const category = this.scatterPlot.zColumn.categories
                    ? this.scatterPlot.zColumn.categories[i]
                    : "undefined";
                const coloringHtml = `<span class='swatch' style='background:${this.strokeColor(i, filtering)}'>&nbsp;</span>`;
                tooltip.append("div")
                    .html(`where ${this.scatterPlot.zColumn.labelText()} is: ${RegressionPlot.brIze(category)} (color ${coloringHtml}&nbsp;)`);
            }
            if (filtering & RegressionPlot.CUT_FILTER) {
                const filteredUncutData = data.filterData(this.scatterPlot.spData.sampleData);
                const filteredCutData = data.filterData(this.scatterPlot.spData.cutData());
                tooltip.append("div")
                    .html(`${filteredCutData.length} sample points (${filteredUncutData.length - filteredCutData.length} filtered points)`);
            }
        }
        static linearRegression(data, filtering) {
            let linearRegression = filtering & RegressionPlot.CUT_FILTER ? data.cutLinearRegression : data.uncutLinearRegression;
            if (!linearRegression) {
                console.error("RegressionPlot.update called but no linear regression computed");
                linearRegression = [[0, 0]];
                linearRegression.rSquared = 0;
            }
            return linearRegression;
        }
        static loessRegression(data, filtering) {
            let loessRegression = filtering & RegressionPlot.CUT_FILTER ? data.cutLoessRegression : data.uncutLoessRegression;
            if (!loessRegression) {
                console.error("RegressionPlot.update called but no loess regression computed");
                loessRegression = [[0, 0]];
            }
            return loessRegression;
        }
        computePlot(zColumn) {
            this.mainRegr.computePlot();
            if (!zColumn || !zColumn.categories) {
                this.subRegrList = [];
            }
            else {
                this.subRegrList = zColumn.categories.map((_cat, i) => {
                    return new spm.RegressionData(this, RegressionPlot.catFilter(zColumn, i));
                });
            }
            this.subRegrList.forEach(data => {
                data.computePlot();
            });
            return this;
        }
        static catFilter(column, catIndex) {
            return function (row) {
                return row[column.dim] === catIndex;
            };
        }
    }
    RegressionPlot.NO_FILTER = 0;
    RegressionPlot.CUT_FILTER = 1;
    RegressionPlot.SUB_FILTER = 1 << 1;
    RegressionPlot.LINEAR_REP = 1;
    RegressionPlot.LOESS_REP = 1 << 1;
    spm.RegressionPlot = RegressionPlot;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class RowFilter {
        constructor(xDim, yDim) {
            this.xyCutoffs = null;
            this.xDim = xDim;
            this.yDim = yDim;
        }
        isKeptRow(row) {
            return this.isKeptValue(row[this.xDim], row[this.yDim]);
        }
        isKeptValue(xValue, yValue) {
            if (this.xyCutoffs !== null) {
                let active = false;
                this.xyCutoffs.forEach(function (xyCutoff) {
                    const xActive = xyCutoff[0] === null || xyCutoff[0][0] <= xValue && xValue <= xyCutoff[0][1];
                    const yActive = xyCutoff[1] === null || xyCutoff[1][0] <= yValue && yValue <= xyCutoff[1][1];
                    active = active || (xActive && yActive);
                });
                return active;
            }
            return true;
        }
    }
    spm.RowFilter = RowFilter;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class ScatterPlot {
        constructor(spData, config) {
            this.xPlot = 0;
            this.yPlot = 0;
            this.width = 0;
            this.height = 0;
            this.xScale = d3.scaleLinear();
            this.axisVisibility = { xTitle: true, xValues: true, yTitle: true, yValues: true };
            this.scatterXAxis = d3.axisBottom(this.xScale)
                .tickFormat(ScatterPlot.prototype.formatXValue.bind(this));
            this.yScale = d3.scaleLinear();
            this.scatterYAxis = d3.axisLeft(this.yScale)
                .tickFormat(ScatterPlot.prototype.formatYValue.bind(this));
            this.zScale = d3.scaleLinear();
            this.continuousCslAxis = d3.axisRight(this.zScale)
                .tickFormat(ScatterPlot.prototype.formatZValue.bind(this));
            this.zoomBrush = null;
            this.dblClickTimeout = null;
            this.regressionPlot = null;
            this.xDistribPlot = null;
            this.yDistribPlot = null;
            this.horViolinPlots = [];
            this.verViolinPlots = [];
            this.pickingReady = false;
            this.spData = spData;
            this.style = config.style;
            this.bindto = config.bindto;
            this.index = config.index;
            this.xColumn = spData.columns[spData.dimensions[0]];
            this.yColumn = spData.columns[spData.dimensions[0]];
            this.zColumn = null;
            this.row = config.row;
            this.col = config.col;
            this.regressionType = config.regressionType;
            this.mouseMode = config.mouseMode;
            this.continuousCsId = config.continuousCsId;
            this.categoricalCsId = config.categoricalCsId;
            this.distribVisibility = config.distribVisibility;
            this.distribType = config.distribType;
            this.axisVisibility = config.axisVisibility;
            this.multiBrush = null;
            this.contColorScale = d3.scaleSequential(spm.SpConst.CONTINUOUS_CS[this.continuousCsId]);
            this.catColorScale = spm.SpConst.CATEGORIAL_CS[this.categoricalCsId];
        }
        removePlot() {
            this.plotSelection().remove();
            d3.select(this.canvasSelector(true)).remove();
            d3.select(this.canvasSelector(false)).remove();
            this.spData.on(spm.SpData.HL_POINT_EVENT + "." + this.index, null);
        }
        setXColumn(column) {
            this.xColumn = column;
            this.xDistribPlot = null;
            this.horViolinPlots = [];
            this.verViolinPlots = [];
        }
        setYColumn(column) {
            this.yColumn = column;
            this.yDistribPlot = null;
            this.horViolinPlots = [];
            this.verViolinPlots = [];
        }
        setZColumn(column) {
            this.zColumn = column;
        }
        formatXValue(value) {
            return this.xColumn.formatedValue(value);
        }
        formatYValue(value) {
            return this.yColumn.formatedValue(value);
        }
        formatZValue(value) {
            return this.zColumn === null ? "No Z axis" : this.zColumn.formatedValue(value);
        }
        // eslint-disable-next-line max-lines-per-function
        draw(updateType) {
            const plotSelection = this.plotSelection();
            this.updateScales();
            if (updateType & ScatterPlot.INIT) {
                // Add Numbering to the plot
                const x = (this.width * this.distribRatio() + this.width - ScatterPlot.padding.r) / 2;
                const y = (this.height * (1 - this.distribRatio()) + ScatterPlot.padding.t) / 2;
                plotSelection.append("text")
                    .attr("class", "scatterNumber")
                    .attr("x", x)
                    .attr("y", y)
                    .attr("text-anchor", "middle")
                    .attr("dominant-baseline", "middle")
                    .text(this.index + 1);
            }
            this.drawXAxis(updateType, plotSelection);
            this.drawYAxis(updateType, plotSelection);
            this.drawJitterZones(updateType, plotSelection);
            if (updateType & ScatterPlot.INIT) {
                // Add a rect to listen mouse events for canvas 
                const xScaleRange = this.xScale.range();
                const yScaleRange = this.yScale.range();
                const spRect = plotSelection.select(".spArea")
                    .append("rect")
                    .attr("class", "spRect")
                    .attr("x", xScaleRange[0])
                    .attr("y", yScaleRange[1])
                    .attr("width", xScaleRange[1] - xScaleRange[0])
                    .attr("height", yScaleRange[0] - yScaleRange[1])
                    .attr("fill", "none")
                    .attr("pointer-events", "fill")
                    .on("mousemove", function (plot) {
                    plot.spData.dispatch.call(spm.SpData.HL_GRAPH_EVENT, undefined, plot);
                    plot.canvasMousemove(d3.mouse(this));
                })
                    .on("mouseout", function (plot) {
                    plot.canvasMouseout();
                    const coord = d3.mouse(this);
                    if (coord[0] < xScaleRange[0] || coord[0] > xScaleRange[1]
                        || coord[1] < yScaleRange[1] || coord[0] > yScaleRange[0]) {
                        plot.spData.dispatch.call(spm.SpData.HL_GRAPH_EVENT, undefined, null);
                    }
                });
                // Compute 'xPlot' and 'yPlot' (useful for canvas)
                const mspDivNode = d3.select(this.bindto + " .mspDiv").node();
                const parentBounds = (mspDivNode === null) ? null : mspDivNode.getBoundingClientRect();
                const xParent = (parentBounds === null) ? 0 : parentBounds.x;
                const yParent = (parentBounds === null) ? 0 : parentBounds.y;
                const spRectNode = spRect.node();
                const spRectBounds = (spRectNode === null) ? null : spRectNode.getBoundingClientRect();
                const xSpRect = (spRectBounds === null) ? 0 : spRectBounds.x;
                const ySpRect = (spRectBounds === null) ? 0 : spRectBounds.y;
                this.xPlot = xSpRect - xParent;
                this.yPlot = ySpRect - yParent;
                // React to 'HL_POINT_EVENT' to highlight the point which is hovered by mouse
                const thisPlot = this;
                this.spData.on(spm.SpData.HL_POINT_EVENT + "." + this.index, function (hlEvent) {
                    if (thisPlot.plotSelection().size() !== 0) {
                        thisPlot.hlPoint(thisPlot.plotSelection(), hlEvent);
                    }
                });
                // Add 'foreignObject' to use a 'drawing' canvas inside svg
                plotSelection.select(".spArea")
                    .append("foreignObject")
                    .style("pointer-events", "none")
                    .attr("class", "canvasGroup" + this.index + " drawing")
                    .attr("x", xScaleRange[0])
                    .attr("y", yScaleRange[1])
                    .attr("width", xScaleRange[1] - xScaleRange[0])
                    .attr("height", yScaleRange[0] - yScaleRange[1])
                    .append("xhtml:div");
                // Add 'foreignObject' to use a 'picking' canvas inside svg
                plotSelection.select(".spArea")
                    .append("foreignObject")
                    .style("pointer-events", "none")
                    .attr("class", "canvasGroup" + this.index + " picking")
                    .attr("x", xScaleRange[0])
                    .attr("y", yScaleRange[1])
                    .attr("width", xScaleRange[1] - xScaleRange[0])
                    .attr("height", yScaleRange[0] - yScaleRange[1])
                    .append("xhtml:div");
            }
            this.drawRegressionPlots(updateType, plotSelection);
            this.drawDistribPlots(updateType, plotSelection);
            this.drawVerViolinPlots(updateType, plotSelection);
            this.drawHorViolinPlots(updateType, plotSelection);
            this.drawCsl(updateType, plotSelection);
            this.drawBrush(updateType, plotSelection);
            this.drawCanvas(false);
        }
        hlPoint(plotSelection, hlEvent) {
            const i = hlEvent.rowIndex;
            const cx = i === null ? 0 : this.cx(this.spData.sampleData[i], i);
            const cy = i === null ? 0 : this.cy(this.spData.sampleData[i], i);
            const color = i === null ? "black" : this.pointColor(this.spData.sampleData[i]);
            const spArea = plotSelection.select(".spArea");
            spArea.selectAll(".hlPoint").data(["hlPoint"])
                .join(enter => enter.append("circle")
                .attr("class", "hlPoint")
                .style("pointer-events", "none"), update => update, exit => exit.remove())
                .attr("fill", color)
                .attr("cx", cx)
                .attr("cy", cy)
                .attr("r", 2 * this.style.plotProperties.point.radius)
                .attr("display", i === null ? "none" : "block");
        }
        cx(row, i) {
            const cx = (this.xColumn.categories === null)
                ? this.xScale(row[this.xColumn.dim])
                : this.xScale(row[this.xColumn.dim] + this.spData.jitterXValues[i]);
            return typeof cx === "undefined" ? NaN : cx;
        }
        cy(row, i) {
            const cy = (this.yColumn.categories === null)
                ? this.yScale(row[this.yColumn.dim])
                : this.yScale(row[this.yColumn.dim] + this.spData.jitterYValues[i]);
            return typeof cy === "undefined" ? NaN : cy;
        }
        pointColor(row) {
            if (this.zColumn !== null) {
                const pointColor = (this.zColumn.categories === null)
                    ? this.contColorScale(row[this.zColumn.dim])
                    : this.catColorScale(row[this.zColumn.dim]);
                if (typeof pointColor !== "undefined") {
                    return pointColor;
                }
            }
            return this.style.plotProperties.noCatColor;
        }
        hlGraph(highlight) {
            const plotSelection = this.plotSelection();
            plotSelection.select(".spRect")
                .classed("hlGraph", highlight);
        }
        plotSelection(plotSelection) {
            if (plotSelection) {
                return plotSelection;
            }
            const thisPlot = this;
            const mspGroup = d3.select(this.bindto + " .mspGroup");
            return mspGroup.selectAll(".scatterPlot")
                .filter(function (plot) {
                return plot.row === thisPlot.row && plot.col === thisPlot.col;
            });
        }
        drawRegressionPlots(updateType, plotSelection) {
            const spArea = plotSelection.select(".spArea");
            if (updateType & ScatterPlot.INIT || !this.regressionPlot) {
                this.regressionPlot = new spm.RegressionPlot(this);
                this.regressionPlot.generate(spArea);
            }
            if (updateType & (ScatterPlot.INIT | ScatterPlot.SHAPE | ScatterPlot.RANGE | ScatterPlot.DOMAIN | ScatterPlot.Z_AXIS)) {
                this.regressionPlot.computePlot(this.zColumn);
            }
            if (updateType & (ScatterPlot.INIT | ScatterPlot.PALETTE)) {
                this.regressionPlot.colorScale(this.catColorScale);
            }
            this.regressionPlot.update(updateType, spArea);
        }
        // eslint-disable-next-line max-lines-per-function
        drawDistribPlots(updateType, plotSelection) {
            if (!this.distribVisibility) {
                return;
            }
            const distribGroup = plotSelection.select(".distribGroup");
            // Horizontal Distrib Plot
            if (updateType & ScatterPlot.INIT || !this.xDistribPlot) {
                this.xDistribPlot = new spm.DistributionPlot(this.spData, this.xColumn, {
                    bindto: this.bindto,
                    orientation: spm.DistributionPlot.HOR,
                    mouseMode: this.mouseMode,
                    categoricalCsId: this.categoricalCsId,
                    distribType: this.distribType,
                    style: this.style
                }, this.index, this.xPlot, this.yPlot);
                this.xDistribPlot.generate(distribGroup, "#x-clip");
            }
            if (updateType & (ScatterPlot.INIT | ScatterPlot.SHAPE | ScatterPlot.RANGE | ScatterPlot.DOMAIN | ScatterPlot.Z_AXIS)) {
                const xDistribPlotRange = [this.height, this.height * (1 - this.distribRatio() * 0.8)];
                this.xDistribPlot
                    .valuesScaleRange(this.xScale.range())
                    .computePlot(this.zColumn)
                    .distribScaleRange(xDistribPlotRange);
            }
            if (updateType & (ScatterPlot.INIT | ScatterPlot.PALETTE)) {
                this.xDistribPlot.colorScale(this.catColorScale);
            }
            this.xDistribPlot.update(updateType, distribGroup);
            // Vertical Distrib Plot
            if (updateType & ScatterPlot.INIT || !this.yDistribPlot) {
                this.yDistribPlot = new spm.DistributionPlot(this.spData, this.yColumn, {
                    bindto: this.bindto,
                    orientation: spm.DistributionPlot.VER,
                    mouseMode: this.mouseMode,
                    categoricalCsId: this.categoricalCsId,
                    distribType: this.distribType,
                    style: this.style
                }, this.index, this.xPlot, this.yPlot);
                this.yDistribPlot.generate(distribGroup, "#y-clip");
            }
            if (updateType & (ScatterPlot.INIT | ScatterPlot.SHAPE | ScatterPlot.RANGE | ScatterPlot.DOMAIN | ScatterPlot.Z_AXIS)) {
                const yDistribPlotRange = [0, this.width * this.distribRatio() * 0.8];
                this.yDistribPlot
                    .valuesScaleRange(this.yScale.range())
                    .computePlot(this.zColumn)
                    .distribScaleRange(yDistribPlotRange);
            }
            if (updateType & (ScatterPlot.INIT | ScatterPlot.PALETTE)) {
                this.yDistribPlot.colorScale(this.catColorScale);
            }
            this.yDistribPlot.update(updateType, distribGroup);
        }
        distribRatio() {
            return this.distribVisibility
                ? ScatterPlot.DISTRIB_RATIO
                : 0;
        }
        drawVerViolinPlots(updateType, plotSelection) {
            const thisPlot = this;
            const spArea = plotSelection.select(".spArea");
            const xCategories = this.xColumn.categories ? this.xColumn.categories : [];
            if (updateType & ScatterPlot.INIT || this.verViolinPlots.length === 0) {
                this.verViolinPlots = xCategories.map(_cat => new spm.DistributionPlot(this.spData, this.yColumn, {
                    bindto: this.bindto,
                    orientation: spm.DistributionPlot.VER,
                    mouseMode: this.mouseMode,
                    categoricalCsId: this.categoricalCsId,
                    distribType: this.distribType,
                    style: this.style
                }, this.index, this.xPlot, this.yPlot));
            }
            spArea.selectAll(".ver.violinGroup").data(this.verViolinPlots)
                .join(enter => enter.append("g")
                .attr("class", "ver violinGroup")
                .each(function (violinPlot) {
                violinPlot.generate(d3.select(this));
            }), update => update, exit => exit.remove())
                .each(function (violinPlot, i) {
                if (updateType & (ScatterPlot.INIT | ScatterPlot.SHAPE | ScatterPlot.RANGE | ScatterPlot.DOMAIN | ScatterPlot.Z_AXIS)) {
                    const violinRange = ScatterPlot.verViolinRange(thisPlot.xScale, i, xCategories.length);
                    violinPlot
                        .valuesScaleRange(thisPlot.yScale.range())
                        .computePlot(thisPlot.zColumn, { column: thisPlot.xColumn, catIndex: i })
                        .distribScaleRange(violinRange);
                }
                if (updateType & (ScatterPlot.INIT | ScatterPlot.PALETTE | ScatterPlot.Z_AXIS)) {
                    violinPlot.colorScale(thisPlot.catColorScale);
                }
                violinPlot.update(updateType, d3.select(this));
            });
        }
        drawHorViolinPlots(updateType, plotSelection) {
            const thisPlot = this;
            const spArea = plotSelection.select(".spArea");
            const yCategories = this.yColumn.categories ? this.yColumn.categories : [];
            if (updateType & ScatterPlot.INIT || this.horViolinPlots.length === 0) {
                this.horViolinPlots = yCategories.map(_cat => new spm.DistributionPlot(this.spData, this.xColumn, {
                    bindto: this.bindto,
                    orientation: spm.DistributionPlot.HOR,
                    mouseMode: this.mouseMode,
                    categoricalCsId: this.categoricalCsId,
                    distribType: this.distribType,
                    style: this.style
                }, this.index, this.xPlot, this.yPlot));
            }
            spArea.selectAll(".hor.violinGroup").data(this.horViolinPlots)
                .join(enter => enter.append("g")
                .attr("class", "hor violinGroup")
                .each(function (violinPlot) {
                violinPlot.generate(d3.select(this));
            }), update => update, exit => exit.remove())
                .each(function (violinPlot, i) {
                if (updateType & (ScatterPlot.INIT | ScatterPlot.SHAPE | ScatterPlot.RANGE | ScatterPlot.DOMAIN | ScatterPlot.Z_AXIS)) {
                    const violinRange = ScatterPlot.horViolinRange(thisPlot.yScale, i, yCategories.length);
                    violinPlot
                        .valuesScaleRange(thisPlot.xScale.range())
                        .computePlot(thisPlot.zColumn, { column: thisPlot.yColumn, catIndex: i })
                        .distribScaleRange(violinRange);
                }
                if (updateType & (ScatterPlot.INIT | ScatterPlot.PALETTE | ScatterPlot.Z_AXIS)) {
                    violinPlot.colorScale(thisPlot.catColorScale);
                }
                violinPlot.update(updateType, d3.select(this));
            });
        }
        updateScales() {
            this.updateXScale();
            this.updateYScale();
            this.updateZScale();
        }
        updateXScale() {
            this.xScale
                .range([this.width * this.distribRatio(), this.width - ScatterPlot.padding.r])
                .domain(this.xColumn.domain());
            if (this.xColumn.categories === null) {
                this.xScale.nice();
            }
            const xTickSize = -this.height + ScatterPlot.padding.t;
            this.scatterXAxis.scale(this.xScale)
                .ticks(this.xColumn.axisTicks())
                .tickSize(xTickSize);
        }
        updateYScale() {
            this.yScale
                .range([this.height * (1 - this.distribRatio()), ScatterPlot.padding.t])
                .domain(this.yColumn.domain());
            if (this.yColumn.categories === null) {
                this.yScale.nice();
            }
            const yTickSize = -this.width + ScatterPlot.padding.r;
            this.scatterYAxis.scale(this.yScale)
                .ticks(this.yColumn.axisTicks())
                .tickSize(yTickSize);
        }
        updateZScale() {
            const zColumn = this.zColumn;
            if (zColumn === null) {
                return;
            }
            this.zScale
                .range([this.height * (1 - this.distribRatio()) - ScatterPlot.padding.t, 0])
                .domain(zColumn.domain());
            if (zColumn.categories === null) {
                this.zScale.nice();
                const [zMin, zMax] = zColumn.domain();
                this.contColorScale = d3.scaleSequential(spm.SpConst.CONTINUOUS_CS[this.continuousCsId])
                    .domain([zMin, zMax]);
                this.continuousCslAxis.scale(this.zScale)
                    .ticks(zColumn.axisTicks());
            }
            else {
                const zMax = zColumn.domain()[1];
                this.catColorScale = spm.SpConst.CATEGORIAL_CS[this.categoricalCsId]
                    .domain(d3.range(zMax));
            }
        }
        drawCsl(updateType, plotSelection) {
            if (updateType & ScatterPlot.INIT) {
                plotSelection.append("g")
                    .attr("class", "cslGroup");
            }
            else {
                plotSelection.select(".cslGroup > g").remove();
            }
            plotSelection.select(".cslGroup").style("visibility", this.zColumn === null ? "hidden" : "visible");
            const zColumn = this.zColumn;
            if (zColumn === null) {
                return;
            }
            if (updateType & (ScatterPlot.INIT | ScatterPlot.PALETTE | ScatterPlot.Z_AXIS | ScatterPlot.RANGE | ScatterPlot.DOMAIN)) {
                if (zColumn.categories === null) {
                    this.drawContinuousCsl(plotSelection);
                }
                else {
                    this.drawCategoricalCsl(plotSelection);
                }
                // Set Color Scale Legend position
                plotSelection.select(".cslGroup").attr("transform", "translate(" + (this.width + ScatterPlot.cslLeft) + ", " + ScatterPlot.padding.t + ")");
                // Set title of Color Scale Legend
                ScatterPlot.tspanColumnTitleBT(plotSelection.select(".cslTitle"), zColumn);
            }
        }
        static tspanColumnTitleBT(textSelection, column) {
            const labels = column.label.split("<br>");
            textSelection.text(labels[labels.length - 1]);
            for (let i = 1; i < labels.length; i++) {
                textSelection.append("tspan")
                    .attr("x", 0)
                    .attr("dy", "-1em")
                    .text(labels[labels.length - 1 - i]);
            }
        }
        static tspanColumnTitleTB(textSelection, column) {
            const labels = column.label.split("<br>");
            textSelection.text(labels[0]);
            for (let i = 1; i < labels.length; i++) {
                textSelection.append("tspan")
                    .attr("x", 0)
                    .attr("dy", "1em")
                    .text(labels[i]);
            }
        }
        drawContinuousCsl(plotSelection) {
            const thisPlot = this;
            const cslGroup = plotSelection.select(".cslGroup");
            const continuousCslGroup = cslGroup.append("g").attr("class", "continuous");
            const colorScaleBars = continuousCslGroup.append("g").attr("class", "colorScaleBars");
            const csHeight = thisPlot.zScale.range()[0] - thisPlot.zScale.range()[1];
            colorScaleBars.selectAll(".colorScaleBar")
                .data(d3.range(csHeight))
                .enter().append("rect")
                .attr("class", "colorScaleBar")
                .attr("y", function (_d, i) {
                return i;
            })
                .attr("width", ScatterPlot.cslWidth)
                .attr("height", 1)
                .style("fill", function (pixel) {
                const fill = thisPlot.contColorScale(thisPlot.zScale.invert(pixel));
                return typeof fill === "undefined" ? "black" : fill;
            });
            continuousCslGroup.append("rect")
                .attr("class", "continuousCslRect")
                .attr("width", ScatterPlot.cslWidth)
                .attr("height", csHeight);
            continuousCslGroup.append("g")
                .attr("class", "continuousCslAxis")
                .attr("transform", "translate(" + ScatterPlot.cslWidth + ", 0)")
                .call(this.continuousCslAxis);
            continuousCslGroup.append("text")
                .attr("class", "cslTitle")
                .attr("x", 0)
                .attr("y", -7);
        }
        drawCategoricalCsl(plotSelection) {
            const zColumn = this.zColumn;
            if (zColumn === null || !zColumn.categories) {
                console.error("'drawCategoricalCsl' called, but Z column is not categorial");
                return;
            }
            const thisPlot = this;
            const cslGroup = plotSelection.select(".cslGroup");
            const catGroupHeight = 15;
            const yCatGroup = 0.5 * (this.height - zColumn.categories.length * catGroupHeight);
            const categorialCslGroup = cslGroup.append("g")
                .attr("transform", `translate(0,${yCatGroup < 0 ? 0 : yCatGroup})`);
            categorialCslGroup.append("text")
                .attr("class", "cslTitle")
                .attr("x", 0)
                .attr("y", -7);
            const categoryGroup = categorialCslGroup.selectAll(".categoryGroup")
                .data(zColumn.categories)
                .enter().append("g")
                .attr("class", "categoryGroup")
                .attr("transform", (_cat, i) => `translate(0,${i * catGroupHeight})`);
            categoryGroup.append("rect")
                .attr("width", 10)
                .attr("height", 10)
                .style("fill", function (_cat, i) { return thisPlot.catColorScale(i); });
            categoryGroup.append("text")
                .attr("x", 15)
                .attr("y", 5)
                .attr("dy", "0.35em")
                .text(function (cat) { return cat; });
        }
        drawXAxis(updateType, plotSelection) {
            if (updateType & (ScatterPlot.INIT | ScatterPlot.RANGE | ScatterPlot.DOMAIN | ScatterPlot.SHAPE)) {
                const axesGroup = plotSelection.select(".axesGroup");
                if (updateType & ScatterPlot.INIT) {
                    axesGroup.append("g")
                        .attr("class", "x axis")
                        .attr("transform", "translate(0," + this.height + ")");
                    if (this.axisVisibility.xTitle) {
                        const x = (this.width * this.distribRatio() + this.width - ScatterPlot.padding.r) / 2;
                        const y = this.height + ScatterPlot.margin.b - 5;
                        axesGroup.append("text")
                            .attr("class", "x scatterlabel")
                            .attr("x", x)
                            .attr("y", y)
                            .attr("text-anchor", "middle")
                            .attr("dominant-baseline", "middle");
                    }
                }
                axesGroup.select(".x.axis").call(this.scatterXAxis)
                    .selectAll(".tick text")
                    .attr("transform", "rotate(45)")
                    .style("text-anchor", "start")
                    .attr("display", this.axisVisibility.xValues ? "block" : "none");
                if (this.axisVisibility.xTitle) {
                    axesGroup.select(".x.scatterlabel")
                        .text(this.xColumn.labelText())
                        .classed("input", this.xColumn.isInput())
                        .classed("output", this.xColumn.isOutput());
                }
            }
        }
        drawYAxis(updateType, plotSelection) {
            if (updateType & (ScatterPlot.INIT | ScatterPlot.RANGE | ScatterPlot.DOMAIN | ScatterPlot.SHAPE)) {
                const axesGroup = plotSelection.select(".axesGroup");
                if (updateType & ScatterPlot.INIT) {
                    axesGroup.append("g")
                        .attr("class", "y axis");
                    if (this.axisVisibility.yTitle) {
                        const x = -ScatterPlot.margin.l * 0.7;
                        const y = (this.height * (1 - this.distribRatio()) + ScatterPlot.padding.t) / 2;
                        axesGroup.append("text")
                            .attr("class", "y scatterlabel")
                            .attr("transform", "translate(" + x + "," + y + ")rotate(270)")
                            .attr("dominant-baseline", "baseline")
                            .attr("text-anchor", "middle");
                    }
                }
                axesGroup.select(".y.axis").call(this.scatterYAxis)
                    .selectAll(".tick text")
                    .attr("display", this.axisVisibility.yValues ? "block" : "none");
                if (this.axisVisibility.yTitle) {
                    axesGroup.select(".y.scatterlabel")
                        .text(this.yColumn.labelText())
                        .classed("input", this.yColumn.isInput())
                        .classed("output", this.yColumn.isOutput());
                }
            }
        }
        // eslint-disable-next-line max-lines-per-function
        drawJitterZones(_updateType, plotSelection) {
            const thisPlot = this;
            const spArea = plotSelection.select(".spArea");
            const xZoneRange = this.xColumn.categories
                ? this.xColumn.categories
                    .map((_cat, i) => [thisPlot.xScale(i - 0.5 / spm.SpConst.CAT_RATIO), thisPlot.xScale(i + 0.5 / spm.SpConst.CAT_RATIO)]
                    .map(ScatterPlot.undef2Nan))
                : [thisPlot.xScale.range()];
            const yZoneRange = this.yColumn.categories
                ? this.yColumn.categories
                    .map((_cat, i) => [thisPlot.yScale(i - 0.5 / spm.SpConst.CAT_RATIO), thisPlot.yScale(i + 0.5 / spm.SpConst.CAT_RATIO)]
                    .map(ScatterPlot.undef2Nan))
                : [thisPlot.yScale.range()];
            const jitterZonesIndexes = [];
            for (let i = 0; i < xZoneRange.length; i++) {
                for (let j = 0; j < yZoneRange.length; j++) {
                    jitterZonesIndexes.push([i, j]);
                }
            }
            spArea.selectAll(".jitterZone").data(d3.range(jitterZonesIndexes.length))
                .join(enter => enter.append("rect")
                .attr("class", "jitterZone"), update => update, exit => exit.remove())
                .attr("x", function (zoneIndex) {
                const index = jitterZonesIndexes[zoneIndex][0];
                return Math.min(xZoneRange[index][0], xZoneRange[index][1]);
            })
                .attr("y", function (zoneIndex) {
                const index = jitterZonesIndexes[zoneIndex][1];
                return Math.min(yZoneRange[index][0], yZoneRange[index][1]);
            })
                .attr("width", function (zoneIndex) {
                const index = jitterZonesIndexes[zoneIndex][0];
                return Math.abs(xZoneRange[index][1] - xZoneRange[index][0]);
            })
                .attr("height", function (zoneIndex) {
                const index = jitterZonesIndexes[zoneIndex][1];
                return Math.abs(yZoneRange[index][1] - yZoneRange[index][0]);
            });
        }
        setContCutoff(brushSelections, _brushEnd) {
            const xyCutoffs = brushSelections
                .map(brushSelection => [
                [brushSelection[0][0], brushSelection[1][0]].map(this.xScale.invert).sort((a, b) => a - b),
                [brushSelection[0][1], brushSelection[1][1]].map(this.yScale.invert).sort((a, b) => a - b)
            ]);
            if (xyCutoffs === null || xyCutoffs.length === 0) {
                this.spData.setXYCutoffs(this.xColumn.dim, this.yColumn.dim, null);
            }
            else {
                this.spData.setXYCutoffs(this.xColumn.dim, this.yColumn.dim, xyCutoffs);
            }
            this.spData.rowFilterChange();
            // this.sendCutoffEvent(dim, brushEnd);
        }
        fixBrush() {
            const plotSelection = this.plotSelection();
            const multiBrushGroup = d3.select(this.bindto + " ." + spm.MultiBrush.multiBrushClass(this.index));
            if (this.mouseMode === spm.SpConst.filterMouse.key) {
                multiBrushGroup.selectAll(".selection").style("display", null);
                multiBrushGroup.selectAll(".handle").style("display", null);
                multiBrushGroup.selectAll(".overlay").style("pointer-events", "all");
            }
            else {
                multiBrushGroup.selectAll(".selection").style("display", "none");
                multiBrushGroup.selectAll(".handle").style("display", "none");
                multiBrushGroup.selectAll(".overlay").style("pointer-events", "auto");
            }
            const zoomBrushGroup = d3.select(this.bindto + " ." + this.zoomBrushClass());
            if (this.mouseMode === spm.SpConst.zoomMouse.key) {
                zoomBrushGroup.selectAll(".selection").style("display", null);
                zoomBrushGroup.selectAll(".handle").style("display", null);
                zoomBrushGroup.selectAll(".overlay").style("pointer-events", "all");
            }
            else {
                zoomBrushGroup.selectAll(".selection").style("display", "none");
                zoomBrushGroup.selectAll(".handle").style("display", "none");
                zoomBrushGroup.selectAll(".overlay").style("pointer-events", "auto");
            }
            if (this.mouseMode === spm.SpConst.filterMouse.key) {
                this.drawBrush(ScatterPlot.RANGE, plotSelection);
            }
        }
        drawBrush(updateType, plotSelection) {
            const thisPlot = this;
            if (updateType & ScatterPlot.INIT) {
                plotSelection.select(".spArea").append("g")
                    .attr("class", function () {
                    return spm.MultiBrush.multiBrushClass(thisPlot.index);
                });
                plotSelection.select(".spArea").append("g")
                    .attr("class", function () {
                    return thisPlot.zoomBrushClass();
                });
                this.initZoomBrush();
            }
            if (thisPlot.mouseMode === spm.SpConst.filterMouse.key) {
                if (this.multiBrush === null ||
                    d3.select(this.bindto).select("." + spm.MultiBrush.multiBrushClass(this.index))
                        .selectAll(".brush")
                        .size() === 0) {
                    this.multiBrush = new spm.MultiBrush(this);
                }
                this.multiBrush.initFrom(this.spData.getRowFilter(this.xColumn.dim, this.yColumn.dim).xyCutoffs);
            }
        }
        initZoomBrush() {
            const thisPlot = this;
            this.zoomBrush = d3.brush()
                // At the end of a brush gesture (such as on mouseup), apply zoom action if 'mouseMode' is 'zoom'
                .on("end", () => {
                const brushZone = d3.event.selection;
                thisPlot.applyZoom(brushZone);
            });
            // Set brushable area
            const xExtent = [
                thisPlot.xScale.range()[0],
                thisPlot.yScale.range()[1]
            ];
            const yExtent = [
                thisPlot.xScale.range()[1],
                thisPlot.yScale.range()[0]
            ];
            this.zoomBrush.extent([xExtent, yExtent]);
            // Create the SVG elements necessary to display the brush selection and to receive input events for interaction
            d3.select(this.bindto + " ." + this.zoomBrushClass()).call(this.zoomBrush);
        }
        // eslint-disable-next-line max-lines-per-function
        applyZoom(brushZone) {
            if (!this.zoomBrush) {
                return;
            }
            const thisPlot = this;
            // zoom on selected zone, or unzoom when a double-click is detected
            if (!brushZone && !this.dblClickTimeout) {
                this.dblClickTimeout = setTimeout(function () {
                    thisPlot.dblClickTimeout = null;
                }, spm.SpConst.dblClickDelay);
                return;
            }
            const plotSelection = this.plotSelection();
            if (brushZone) {
                this.xScale.domain([brushZone[0][0], brushZone[1][0]].map(this.xScale.invert));
                this.yScale.domain([brushZone[1][1], brushZone[0][1]].map(this.yScale.invert));
                // Remove the brush selection
                d3.select(this.bindto + " ." + this.zoomBrushClass()).call(this.zoomBrush.clear);
            }
            else {
                this.xScale.domain(this.xColumn.domain());
                this.yScale.domain(this.yColumn.domain());
            }
            if (this.xColumn.categories === null) {
                this.xScale.nice();
            }
            if (this.yColumn.categories === null) {
                this.yScale.nice();
            }
            // Zoom for axes
            this.drawXAxis(ScatterPlot.DOMAIN, plotSelection);
            this.drawYAxis(ScatterPlot.DOMAIN, plotSelection);
            // Zoom for jitter zone
            this.drawJitterZones(ScatterPlot.DOMAIN, plotSelection);
            // Zoom for brush
            this.drawBrush(ScatterPlot.DOMAIN, plotSelection);
            // Zoom for regression plot
            if (this.regressionPlot) {
                this.regressionPlot.update(ScatterPlot.DOMAIN, plotSelection);
            }
            this.drawCanvas(false);
            // Zoom for distribution plots
            if (this.xDistribPlot && this.distribVisibility) {
                this.xDistribPlot.valuesScale.domain(this.xScale.domain());
                this.xDistribPlot.update(ScatterPlot.DOMAIN, plotSelection.select(".distribGroup"));
            }
            if (this.yDistribPlot && this.distribVisibility) {
                this.yDistribPlot.valuesScale.domain(this.yScale.domain());
                this.yDistribPlot.update(ScatterPlot.DOMAIN, plotSelection.select(".distribGroup"));
            }
            // Vertical violin plots
            if (this.xColumn.categories) {
                const xCategories = this.xColumn.categories;
                const fullXScale = d3.scaleLinear()
                    .range(this.xScale.range())
                    .domain(this.xColumn.domain());
                plotSelection.selectAll(".ver.violinGroup").each(function (violinPlot, i) {
                    const violinRange = ScatterPlot.verViolinRange(fullXScale, i, xCategories.length);
                    if (brushZone) {
                        const range = violinRange.map(fullXScale.invert).map(thisPlot.xScale).map(ScatterPlot.undef2Nan);
                        violinPlot.distribScaleRange(range);
                    }
                    else {
                        violinPlot.distribScaleRange(violinRange);
                    }
                    violinPlot.valuesScale.domain(thisPlot.yScale.domain());
                    violinPlot.update(ScatterPlot.DOMAIN, d3.select(this));
                });
            }
            // Horizontal violin plots
            if (this.yColumn.categories) {
                const yCategories = this.yColumn.categories;
                const fullYScale = d3.scaleLinear()
                    .range(this.yScale.range())
                    .domain(this.yColumn.domain());
                plotSelection.selectAll(".hor.violinGroup").each(function (violinPlot, i) {
                    const violinRange = ScatterPlot.horViolinRange(fullYScale, i, yCategories.length);
                    if (brushZone) {
                        const range = violinRange.map(fullYScale.invert).map(thisPlot.yScale).map(ScatterPlot.undef2Nan);
                        violinPlot.distribScaleRange(range);
                    }
                    else {
                        violinPlot.distribScaleRange(violinRange);
                    }
                    violinPlot.valuesScale.domain(thisPlot.xScale.domain());
                    violinPlot.update(ScatterPlot.DOMAIN, d3.select(this));
                });
            }
        }
        zoomBrushClass() {
            return "zoombrush_plot" + this.index;
        }
        distribRepChange(newType) {
            this.distribType = newType;
            if (this.xDistribPlot) {
                this.xDistribPlot.distribType = newType;
            }
            if (this.yDistribPlot) {
                this.yDistribPlot.distribType = newType;
            }
            this.verViolinPlots.forEach(function (violinPlot) { violinPlot.distribType = newType; });
            this.horViolinPlots.forEach(function (violinPlot) { violinPlot.distribType = newType; });
            const plotSelection = this.plotSelection();
            this.drawDistribPlots(ScatterPlot.SHAPE, plotSelection);
            this.drawVerViolinPlots(ScatterPlot.SHAPE, plotSelection);
            this.drawHorViolinPlots(ScatterPlot.SHAPE, plotSelection);
        }
        regressionRepChange(newType) {
            this.regressionType = newType;
            const plotSelection = this.plotSelection();
            this.drawRegressionPlots(ScatterPlot.SHAPE, plotSelection);
        }
        static undef2Nan(value) {
            return typeof value === "undefined" ? NaN : value;
        }
        static verViolinRange(scale, catIndex, catCount) {
            const min = ScatterPlot.undef2Nan(scale(catIndex + 0.5 / spm.SpConst.CAT_RATIO));
            const rangeLength = Math.abs(scale.range()[1] - scale.range()[0]);
            return [min, min + rangeLength / catCount / 3];
        }
        static horViolinRange(scale, catIndex, catCount) {
            const max = ScatterPlot.undef2Nan(scale(catIndex + 0.5 / spm.SpConst.CAT_RATIO));
            const rangeLength = Math.abs(scale.range()[1] - scale.range()[0]);
            return [max, max - rangeLength / catCount / 3];
        }
        changeMouseMode(mouseMode) {
            this.mouseMode = mouseMode;
            if (this.xDistribPlot) {
                this.xDistribPlot.mouseMode = mouseMode;
            }
            if (this.yDistribPlot) {
                this.yDistribPlot.mouseMode = mouseMode;
            }
            this.verViolinPlots.forEach(function (violinPlot) { violinPlot.mouseMode = mouseMode; });
            this.horViolinPlots.forEach(function (violinPlot) { violinPlot.mouseMode = mouseMode; });
        }
        canvasSelector(picking) {
            return this.bindto + " .canvas" + this.index + (picking ? ".picking" : ".drawing");
        }
        drawCanvas(picking) {
            this.pickingReady = picking;
            const canvasSelector = this.canvasSelector(picking);
            let canvas = d3.select(canvasSelector);
            if (canvas.empty()) {
                canvas = d3.select(this.bindto + " .MultiPlot .canvasGroup" + this.index + (picking ? ".picking" : ".drawing") + " div").append("xhtml:canvas")
                    .attr("class", "canvas" + this.index + (picking ? " picking" : " drawing"));
                if (picking) {
                    canvas.style("display", "none");
                }
            }
            const xScaleRange = this.xScale.range();
            const yScaleRange = this.yScale.range();
            canvas
                .attr("width", xScaleRange[1] - xScaleRange[0])
                .attr("height", yScaleRange[0] - yScaleRange[1]);
            const canvasNode = canvas.node();
            if (!canvasNode) {
                console.error("canvasNode is null");
                return;
            }
            const context2d = canvasNode.getContext("2d");
            if (!context2d) {
                console.error("context2d is null");
                return;
            }
            context2d.clearRect(0, 0, this.width, this.height);
            new spm.PointsPlot(this).drawCanvas(picking);
        }
        canvasMousemove(coords) {
            if (!this.pickingReady) {
                this.drawCanvas(true);
            }
            const canvasSelector = this.canvasSelector(true);
            const canvasNode = d3.select(canvasSelector).node();
            if (!canvasNode) {
                console.error("canvasNode is null for:", canvasSelector);
                return;
            }
            const context2d = canvasNode.getContext("2d");
            if (!context2d) {
                console.error("context2d is null");
                return;
            }
            const xScaleRange = this.xScale.range();
            const yScaleRange = this.yScale.range();
            const xPlot = -xScaleRange[0];
            const yPlot = -yScaleRange[1];
            const color = context2d.getImageData(xPlot + coords[0], yPlot + coords[1], 1, 1).data;
            const code = (color[2] + (color[1] << 8) + (color[0] << 16));
            if (color[3] === 0) {
                this.canvasMouseout();
            }
            else if (code < this.spData.sampleData.length) {
                this.spData.dispatchHlPointEvent({ rowIndex: code, scatterPlot: this });
            }
        }
        canvasMouseout() {
            this.spData.dispatchHlPointEvent({ rowIndex: null, scatterPlot: this });
        }
    }
    ScatterPlot.padding = { r: 10, t: 10 };
    ScatterPlot.DISTRIB_RATIO = 0.15;
    ScatterPlot.margin = { l: 60, r: 10, b: 50, t: 5 };
    ScatterPlot.cslRight = 30;
    ScatterPlot.cslLeft = 10;
    ScatterPlot.cslWidth = 20;
    ScatterPlot.cslTotalWidth = ScatterPlot.cslRight + ScatterPlot.cslLeft + ScatterPlot.cslWidth;
    ScatterPlot.INIT = 1;
    ScatterPlot.SHAPE = 1 << 1;
    ScatterPlot.PALETTE = 1 << 2;
    ScatterPlot.Z_AXIS = 1 << 3;
    ScatterPlot.RANGE = 1 << 4;
    ScatterPlot.DOMAIN = 1 << 5;
    spm.ScatterPlot = ScatterPlot;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class ScatterPlotMatrix {
        constructor(id, width, height) {
            this.zColumn = null;
            this.width = 900;
            this.height = 750;
            this.mouseMode = spm.SpConst.tooltipMouse.key;
            this.rotateTitle = false;
            this.distribType = 
            // DistributionPlot.DENS_REP;
            // DistributionPlot.DENS_REP |
            spm.DistributionPlot.HISTO_REP;
            this.regressionType = 0;
            // RegressionPlot.LOESS_REP;
            // RegressionPlot.LOESS_REP |
            // RegressionPlot.LINEAR_REP;
            this.corrPlotType = 
            // CorrPlot.EMPTY;
            // CorrPlot.TEXT;
            // CorrPlot.ABS_TEXT_REP;
            spm.CorrPlot.CIRCLES_REP;
            this.continuousCsId = spm.SpConst.CONTINUOUS_CS_IDS[0];
            this.categoricalCsId = spm.SpConst.CATEGORIAL_CS_IDS[0];
            this.corrPlotCsId = spm.SpConst.CONTINUOUS_CS_IDS[0];
            this.dispatch = d3.dispatch(ScatterPlotMatrix.PLOT_EVENT);
            this.scatterPlotList = [];
            this.diagPlotList = [];
            this.corrPlotList = [];
            this.brushSlidersLinked = true;
            this.defaultVisibleDimCount = 0;
            this.visibleDimCount = 0;
            this.xStartingDimIndex = 0;
            this.yStartingDimIndex = 0;
            this.bindto = "#" + id;
            this.style = new spm.Style(this.bindto);
            this.setSize(width, height);
        }
        resize(width, height) {
            this.setSize(width, height);
            d3.select(this.bindto + " .MultiPlot svg")
                .attr("width", this.width + ScatterPlotMatrix.margin.l + ScatterPlotMatrix.margin.r)
                .attr("height", this.height + ScatterPlotMatrix.margin.b + ScatterPlotMatrix.margin.t);
            this.removePlots();
            this.updatePlots(spm.ScatterPlot.INIT);
            this.xBrushSlider.update();
            this.yBrushSlider.update();
        }
        setSize(width, height) {
            const wSize = width - (ScatterPlotMatrix.margin.l + ScatterPlotMatrix.margin.r);
            const hSize = height - (ScatterPlotMatrix.margin.t + ScatterPlotMatrix.margin.b);
            this.width = Math.max(200, Math.min(wSize, hSize));
            this.height = this.width;
        }
        removePlots() {
            this.scatterPlotList.forEach(plot => {
                plot.removePlot();
            });
            d3.selectAll(this.bindto + " .diagPlot").remove();
            d3.selectAll(this.bindto + " .corrPlot").remove();
        }
        id() {
            return this.bindto.substring(1);
        }
        initSlidersPosition(config) {
            if (config.slidersPosition) {
                if (typeof config.slidersPosition.dimCount !== "number"
                    || config.slidersPosition.dimCount > ScatterPlotMatrix.MAX_VISIBLE_DIMS) {
                    config.slidersPosition.dimCount = ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.dimCount;
                }
                if (typeof config.slidersPosition.xStartingDimIndex !== "number") {
                    config.slidersPosition.xStartingDimIndex = ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.xStartingDimIndex;
                }
                if (typeof config.slidersPosition.yStartingDimIndex !== "number") {
                    config.slidersPosition.yStartingDimIndex = ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION.yStartingDimIndex;
                }
            }
            else {
                config.slidersPosition = ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION;
            }
            this.defaultVisibleDimCount = config.slidersPosition.dimCount;
            if (this.spData.dimensions.length < this.defaultVisibleDimCount) {
                this.visibleDimCount = this.spData.dimensions.length;
            }
            else {
                this.visibleDimCount = this.defaultVisibleDimCount;
            }
            if (config.slidersPosition.xStartingDimIndex > this.spData.dimensions.length - this.visibleDimCount) {
                this.xStartingDimIndex = this.spData.dimensions.length - this.visibleDimCount;
            }
            else {
                this.xStartingDimIndex = config.slidersPosition.xStartingDimIndex;
            }
            if (config.slidersPosition.yStartingDimIndex > this.spData.dimensions.length - this.visibleDimCount) {
                this.yStartingDimIndex = this.spData.dimensions.length - this.visibleDimCount;
            }
            else {
                this.yStartingDimIndex = config.slidersPosition.yStartingDimIndex;
            }
            this.brushSlidersLinked = this.xStartingDimIndex === this.yStartingDimIndex;
        }
        generate(config) {
            if (d3.select(this.bindto).empty()) {
                throw new Error("'bindto' dom element not found:" + this.bindto);
            }
            this.zColumn = null;
            this.style.initWith(config.cssRules, config.plotProperties);
            this.initData(config);
            this.initSlidersPosition(config);
            this.initTilePlots();
            this.setZAxis(config.zAxisDim, true);
            this.buildMainDomElements(config);
            this.appendPlotSvg();
            this.appendDistribRepSelect();
            this.appendContCsSelect();
            this.appendCatCsSelect();
            this.appendCorrTypeSelect();
            this.appendCorrCsSelect();
            this.appendZAxisSelector();
            this.initZAxisUsedCB();
            this.appendMouseModeSelect();
            this.initRegressionCB();
            this.spData.on(spm.SpData.HL_GRAPH_EVENT, ScatterPlotMatrix.prototype.hlGraph.bind(this));
            this.spData.on(spm.SpData.HL_POINT_EVENT, ScatterPlotMatrix.prototype.hlPoint.bind(this));
            this.updatePlots(spm.ScatterPlot.INIT);
            this.xBrushSlider.update();
            this.yBrushSlider.update();
            return this;
        }
        hlGraph(targetPlot) {
            this.scatterPlotList.forEach(plot => {
                plot.hlGraph(false);
            });
            this.diagPlotList.forEach(plot => {
                plot.hlGraph(false);
            });
            this.corrPlotList.forEach(plot => {
                plot.hlGraph(false);
            });
            if (targetPlot !== null) {
                const thisMPlot = this;
                const targetPlotXDimIndex = thisMPlot.xStartingDimIndex + targetPlot.col;
                const targetPlotYDimIndex = thisMPlot.yStartingDimIndex + targetPlot.row;
                [
                    [targetPlotXDimIndex, targetPlotXDimIndex],
                    [targetPlotXDimIndex, targetPlotYDimIndex],
                    [targetPlotYDimIndex, targetPlotYDimIndex],
                    [targetPlotYDimIndex, targetPlotXDimIndex]
                ].forEach(function (plotCoord) {
                    thisMPlot.scatterPlotList.forEach(plot => {
                        if (thisMPlot.xStartingDimIndex + plot.col === plotCoord[0]
                            && thisMPlot.yStartingDimIndex + plot.row === plotCoord[1]) {
                            plot.hlGraph(true);
                        }
                    });
                    thisMPlot.diagPlotList.forEach(plot => {
                        if (thisMPlot.xStartingDimIndex + plot.col === plotCoord[0]
                            && thisMPlot.yStartingDimIndex + plot.row === plotCoord[1]) {
                            plot.hlGraph(true);
                        }
                    });
                    thisMPlot.corrPlotList.forEach(plot => {
                        if (thisMPlot.xStartingDimIndex + plot.col === plotCoord[0]
                            && thisMPlot.yStartingDimIndex + plot.row === plotCoord[1]) {
                            plot.hlGraph(true);
                        }
                    });
                });
            }
        }
        hlPoint(hlEvent) {
            if (hlEvent.rowIndex === null) {
                new spm.PointsPlot(hlEvent.scatterPlot).mouseout();
            }
            else {
                new spm.PointsPlot(hlEvent.scatterPlot).mouseover(this.spData.sampleData[hlEvent.rowIndex], hlEvent.rowIndex, this.scatterPlotList);
            }
        }
        on(type, callback) {
            // @ts-ignore
            this.dispatch.on(type, callback);
        }
        // eslint-disable-next-line max-lines-per-function
        initData(config) {
            if (!config.continuousCS) {
                this.continuousCsId = spm.SpConst.CONTINUOUS_CS_IDS[0];
            }
            else if (spm.SpConst.CONTINUOUS_CS_IDS.includes(config.continuousCS)) {
                this.continuousCsId = config.continuousCS;
            }
            else {
                console.error("Unknown continuous color scale: " + config.continuousCS);
            }
            if (!config.categoricalCS) {
                this.categoricalCsId = spm.SpConst.CATEGORIAL_CS_IDS[0];
            }
            else if (spm.SpConst.CATEGORIAL_CS_IDS.includes(config.categoricalCS)) {
                this.categoricalCsId = config.categoricalCS;
            }
            else {
                console.error("Unknown categorical color scale: " + config.categoricalCS);
            }
            if (!config.distribType) {
                this.distribType = 2;
            }
            else if ([0, 1, 2, 3].includes(config.distribType)) {
                this.distribType = config.distribType;
            }
            else {
                console.error("Unknown distribType: " + config.distribType);
            }
            if (!config.regressionType) {
                this.regressionType = 0;
            }
            else if ([0, 1, 2, 3].includes(config.regressionType)) {
                this.regressionType = config.regressionType;
            }
            else {
                console.error("Unknown regressionType: " + config.regressionType);
            }
            if (!config.corrPlotType) {
                this.corrPlotType = spm.CorrPlot.CIRCLES_REP;
            }
            else if ([spm.CorrPlot.EMPTY_REP, spm.CorrPlot.CIRCLES_REP, spm.CorrPlot.TEXT_REP, spm.CorrPlot.ABS_TEXT_REP].includes(config.corrPlotType)) {
                this.corrPlotType = config.corrPlotType;
            }
            else {
                console.error("Unknown correlation plot type: " + config.corrPlotType);
            }
            if (this.corrPlotType === spm.CorrPlot.TEXT_REP) {
                this.corrPlotCsId = spm.SpConst.CONTINUOUS_CS_IDS[21]; // RdBu
            }
            else if (this.corrPlotType === spm.CorrPlot.ABS_TEXT_REP) {
                this.corrPlotCsId = spm.SpConst.CONTINUOUS_CS_IDS[8]; // Blues
            }
            if (config.corrPlotCS) {
                if (spm.SpConst.CONTINUOUS_CS_IDS.includes(config.corrPlotCS)) {
                    this.corrPlotCsId = config.corrPlotCS;
                }
                else {
                    console.error("Unknown correlation color scale: " + config.corrPlotCS);
                }
            }
            this.rotateTitle = config.rotateTitle ? config.rotateTitle : false;
            this.spData = new spm.SpData(config);
            this.spData.updateCutRowsMask();
            this.spData.on(spm.SpData.ROW_FILTER_EVENT, ScatterPlotMatrix.prototype.rowFilterChange.bind(this));
        }
        adjustVisibleDimensions() {
            if (this.spData.dimensions.length < this.defaultVisibleDimCount) {
                this.visibleDimCount = this.spData.dimensions.length;
            }
            else {
                this.visibleDimCount = this.defaultVisibleDimCount;
            }
            this.xStartingDimIndex = 0;
            this.yStartingDimIndex = 0;
        }
        initTilePlots() {
            this.scatterPlotList.splice(0, this.scatterPlotList.length);
            this.diagPlotList.splice(0, this.diagPlotList.length);
            this.corrPlotList.splice(0, this.corrPlotList.length);
            for (let j = 0; j < this.visibleDimCount; j++) {
                for (let i = 0; i < this.visibleDimCount; i++) {
                    const xVisibleDimIndex = this.xStartingDimIndex + i;
                    const yVisibleDimIndex = this.yStartingDimIndex + j;
                    if (xVisibleDimIndex < yVisibleDimIndex) {
                        this.pushNewSP(i, j);
                    }
                    if (xVisibleDimIndex === yVisibleDimIndex) {
                        this.pushNewDP(i, j);
                    }
                    if (xVisibleDimIndex > yVisibleDimIndex) {
                        this.pushNewCP(i, j);
                    }
                }
            }
        }
        rowFilterChange() {
            this.spData.updateCutRowsMask();
            this.scatterPlotList.forEach(plot => {
                const plotSelection = plot.plotSelection();
                plot.drawRegressionPlots(spm.ScatterPlot.DOMAIN, plotSelection);
                plot.drawDistribPlots(spm.ScatterPlot.DOMAIN, plotSelection);
                plot.drawVerViolinPlots(spm.ScatterPlot.DOMAIN, plotSelection);
                plot.drawHorViolinPlots(spm.ScatterPlot.DOMAIN, plotSelection);
                if (plotSelection.size() !== 0) {
                    plot.drawCanvas(false);
                }
            });
            this.diagPlotList.forEach(plot => {
                const plotSelection = plot.plotSelection();
                plot.drawDistribPlots(spm.DiagPlot.DOMAIN, plotSelection);
                plot.updateYScaleDomain();
                plot.drawYAxis(spm.DiagPlot.DOMAIN, plotSelection);
            });
            this.corrPlotList.forEach(plot => {
                plot.draw(spm.ScatterPlot.DOMAIN);
            });
        }
        pushNewSP(i, j) {
            const scatterPlot = new spm.ScatterPlot(this.spData, {
                bindto: this.bindto,
                index: i + this.visibleDimCount * j,
                row: j,
                col: i,
                regressionType: this.regressionType,
                mouseMode: this.mouseMode,
                continuousCsId: this.continuousCsId,
                categoricalCsId: this.categoricalCsId,
                distribVisibility: false,
                distribType: this.distribType,
                corrPlotType: this.corrPlotType,
                corrPlotCsId: this.corrPlotCsId,
                axisVisibility: {
                    xTitle: false,
                    xValues: j === this.visibleDimCount - 1,
                    yTitle: false,
                    yValues: i === 0
                },
                style: this.style
            });
            scatterPlot.setXColumn(this.spData.columns[this.spData.dimensions[this.xStartingDimIndex + i]]);
            scatterPlot.setYColumn(this.spData.columns[this.spData.dimensions[this.yStartingDimIndex + j]]);
            scatterPlot.setZColumn(this.getZColumn());
            this.scatterPlotList.push(scatterPlot);
        }
        pushNewDP(i, j) {
            const diagPlot = new spm.DiagPlot(this.spData, {
                bindto: this.bindto,
                index: i + this.visibleDimCount * j,
                row: j,
                col: i,
                regressionType: this.regressionType,
                mouseMode: this.mouseMode,
                continuousCsId: this.continuousCsId,
                categoricalCsId: this.categoricalCsId,
                distribVisibility: false,
                distribType: this.distribType,
                corrPlotType: this.corrPlotType,
                corrPlotCsId: this.corrPlotCsId,
                axisVisibility: {
                    xTitle: false,
                    xValues: j === this.visibleDimCount - 1,
                    yTitle: false,
                    yValues: i === 0
                },
                style: this.style
            });
            diagPlot.setXColumn(this.spData.columns[this.spData.dimensions[this.xStartingDimIndex + i]]);
            diagPlot.setZColumn(this.getZColumn());
            this.diagPlotList.push(diagPlot);
        }
        pushNewCP(i, j) {
            const corrPlot = new spm.CorrPlot(this.spData, {
                bindto: this.bindto,
                index: i + this.visibleDimCount * j,
                row: j,
                col: i,
                regressionType: this.regressionType,
                mouseMode: this.mouseMode,
                continuousCsId: this.continuousCsId,
                categoricalCsId: this.categoricalCsId,
                distribVisibility: false,
                distribType: this.distribType,
                corrPlotType: this.corrPlotType,
                corrPlotCsId: this.corrPlotCsId,
                axisVisibility: {
                    xTitle: true,
                    xValues: true,
                    yTitle: true,
                    yValues: true
                },
                style: this.style
            });
            corrPlot.setXColumn(this.spData.columns[this.spData.dimensions[this.xStartingDimIndex + i]]);
            corrPlot.setYColumn(this.spData.columns[this.spData.dimensions[this.yStartingDimIndex + j]]);
            corrPlot.setZColumn(this.getZColumn());
            this.corrPlotList.push(corrPlot);
        }
        getZColumn() {
            if (this.zColumn) {
                return this.zColumn;
            }
            return null;
        }
        // eslint-disable-next-line max-lines-per-function
        buildMainDomElements(config) {
            const controlWidgets = config.controlWidgets ? config.controlWidgets : false;
            d3.select(this.bindto + " .mspDiv").remove();
            const mspDiv = d3.select(this.bindto).append("div")
                .attr("class", "mspDiv")
                .classed("withWidgets", controlWidgets)
                .classed("withoutWidgets", !controlWidgets);
            const controlDiv = mspDiv.append("div").attr("class", "controlDiv");
            const optionalPlotsDiv = controlDiv.append("div")
                .attr("class", "optionalPlotsDiv");
            optionalPlotsDiv.append("div")
                .attr("class", "distribRepDiv")
                .html("Distribution Representation: <span class=\"distribRepSelect\"></span>");
            optionalPlotsDiv.append("div")
                .attr("class", "linearRegrDiv")
                .html(`<input type="checkbox" id="${this.id()}_linearRegr" name="linearRegr"> <label for="${this.id()}_linearRegr">Linear Regression</label>`);
            optionalPlotsDiv.append("div")
                .attr("class", "loessDiv")
                .html(`<input type="checkbox" id="${this.id()}_loess" name="loess"> <label for="${this.id()}_loess">Local Polynomial Regression</label>`);
            const csDiv = controlDiv.append("div")
                .attr("class", "csDiv");
            csDiv.append("div")
                .attr("class", "zAxisUsedDiv")
                .html(`<input type="checkbox" id="${this.id()}_zAxisUsed" name="zAxisUsed" checked> <label for="${this.id()}_zAxisUsed">Use Z Axis</label> <span class="ParamSelect ZAxis"></span>`);
            csDiv.append("div")
                .attr("class", "contCsDiv")
                .html("Continuous Color Scale: <span class=\"contCsSelect\"></span>");
            csDiv.append("div")
                .html("Categorical Color Scale: <span class=\"catCsSelect\"></span>");
            const corrDiv = controlDiv.append("div")
                .attr("class", "corrDiv");
            corrDiv.append("div")
                .attr("class", "corrTypeDiv")
                .html("Correlation Plot Type: <span class=\"corrTypeSelect\"></span>");
            corrDiv.append("div")
                .attr("class", "corrCsDiv")
                .html("Correlation Color Scale: <span class=\"corrCsSelect\"></span>");
            corrDiv.append("div")
                .attr("class", "mouseModeDiv")
                .html("Mouse mode: <span class=\"mouseModeSelect\"></span>");
            mspDiv.append("div").attr("class", "MultiPlot");
        }
        appendPlotSvg() {
            const mspSvg = d3.select(this.bindto + " .MultiPlot")
                .append("svg")
                .attr("width", this.width + ScatterPlotMatrix.margin.l + ScatterPlotMatrix.margin.r)
                .attr("height", this.height + ScatterPlotMatrix.margin.b + ScatterPlotMatrix.margin.t);
            mspSvg.append("g")
                .attr("transform", `translate(${ScatterPlotMatrix.margin.l}, ${ScatterPlotMatrix.margin.t})`)
                .attr("class", "mspGroup");
            this.appendSvgDefs();
            this.appendBrushSliders();
        }
        appendSvgDefs() {
            const svg = d3.select(this.bindto + " svg");
            const defs = svg.append("defs");
            defs.append("clipPath")
                .attr("id", "tile-clip")
                .append("rect");
        }
        appendBrushSliders() {
            this.xBrushSlider = new spm.BrushSlider(this, true);
            this.yBrushSlider = new spm.BrushSlider(this, false);
        }
        updateVisibleDimensions(begin, end, xOriented) {
            if (begin !== undefined && end !== undefined && begin >= 0 && end >= 0) {
                if (xOriented || this.brushSlidersLinked) {
                    this.xStartingDimIndex = begin;
                }
                if (!xOriented || this.brushSlidersLinked) {
                    this.yStartingDimIndex = begin;
                }
                this.visibleDimCount = end - begin + 1;
                this.defaultVisibleDimCount = this.visibleDimCount;
                this.tilesNumberChanged();
            }
        }
        tilesNumberChanged() {
            this.removePlots();
            this.initTilePlots();
            this.updatePlots(spm.ScatterPlot.INIT);
        }
        // eslint-disable-next-line max-lines-per-function
        updatePlots(updateType) {
            const thisMPlot = this;
            const mspGroup = d3.select(this.bindto + " .MultiPlot .mspGroup");
            const tileWidth = (this.width / thisMPlot.visibleDimCount);
            const tileHeight = (this.height / thisMPlot.visibleDimCount);
            if (updateType & (spm.ScatterPlot.INIT | spm.ScatterPlot.RANGE)) {
                this.scatterPlotList.forEach(plot => {
                    plot.height = tileHeight;
                    plot.width = tileWidth;
                });
                this.diagPlotList.forEach(plot => {
                    plot.height = tileHeight;
                    plot.width = tileWidth;
                });
                this.corrPlotList.forEach(plot => {
                    plot.height = tileHeight;
                    plot.width = tileWidth;
                });
                d3.select(this.bindto + " #tile-clip > rect")
                    .attr("x", 0)
                    .attr("y", spm.ScatterPlot.padding.t)
                    .attr("width", tileWidth - spm.ScatterPlot.padding.r)
                    .attr("height", tileHeight - spm.ScatterPlot.padding.t);
            }
            if (updateType & spm.ScatterPlot.INIT) {
                this.initColHeaders();
                // corrPlot
                mspGroup.selectAll(".corrPlot")
                    .data(this.corrPlotList)
                    .enter().append("g")
                    .attr("class", "corrPlot")
                    .attr("transform", function (plot) {
                    return `translate(${thisMPlot.xSubPlot(plot)}, ${thisMPlot.ySubPlot(plot)})`;
                });
                // diagPlot
                const diagPlot = mspGroup.selectAll(".diagPlot")
                    .data(this.diagPlotList)
                    .enter().append("g")
                    .attr("class", "diagPlot")
                    .attr("transform", function (plot) {
                    return `translate(${thisMPlot.xSubPlot(plot)}, ${thisMPlot.ySubPlot(plot)})`;
                });
                diagPlot.append("g")
                    .attr("class", "axesGroup");
                diagPlot.append("g")
                    .attr("class", "spArea")
                    .attr("clip-path", "url(#tile-clip)");
                diagPlot.append("g")
                    .attr("class", "distribGroup");
                // scatterPlot
                const scatterPlot = mspGroup.selectAll(".scatterPlot")
                    .data(this.scatterPlotList)
                    .enter().append("g")
                    .attr("class", "scatterPlot")
                    .attr("transform", function (plot) {
                    return `translate(${thisMPlot.xSubPlot(plot)}, ${thisMPlot.ySubPlot(plot)})`;
                });
                scatterPlot.append("g")
                    .attr("class", "axesGroup");
                scatterPlot.append("g")
                    .attr("class", "spArea")
                    .attr("clip-path", "url(#tile-clip)");
                scatterPlot.append("g")
                    .attr("class", "distribGroup");
            }
            mspGroup.selectAll(".scatterPlot").each(function (plot) {
                plot.draw(updateType);
            });
            mspGroup.selectAll(".diagPlot").each(function (plot) {
                plot.draw(updateType);
            });
            mspGroup.selectAll(".corrPlot").each(function (plot) {
                plot.draw(updateType);
            });
            mspGroup.selectAll(".cslGroup").style("display", "none");
            if (updateType & spm.ScatterPlot.INIT) {
                this.fixBrush();
                this.style.applyCssRules();
            }
        }
        initColHeaders() {
            this.initTopHeaders();
            this.initRightHeaders();
            this.initBottomHeaders();
            this.initLeftHeaders();
        }
        initTopHeaders() {
            const thisMPlot = this;
            const mspGroup = d3.select(this.bindto + " .MultiPlot .mspGroup");
            const tileWidth = (this.width / thisMPlot.visibleDimCount);
            mspGroup.selectAll(".topHeader")
                .data(d3.range(this.visibleDimCount))
                .join(enter => enter.append("text")
                .attr("class", "topHeader Header")
                .on("click", ScatterPlotMatrix.prototype.clickColTitle.bind(thisMPlot)), update => update, exit => exit.remove())
                .classed("input", function (colIndex) { return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]].isInput(); })
                .classed("output", function (colIndex) { return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]].isOutput(); })
                .classed("zAxis", function (colIndex) { return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]; })
                .attr("transform", function (colIndex) {
                if (thisMPlot.rotateTitle) {
                    return `translate(${tileWidth * (colIndex + 0.3)}, -10)rotate(-10)`;
                }
                else {
                    return `translate(${tileWidth * (colIndex + 0.5)}, -10)`;
                }
            })
                .each(function (colIndex) {
                spm.ScatterPlot.tspanColumnTitleBT(d3.select(this), thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]);
            })
                // @ts-ignore
                .attr("text-anchor", thisMPlot.rotateTitle ? null : "middle");
        }
        initRightHeaders() {
            const thisMPlot = this;
            const mspGroup = d3.select(this.bindto + " .MultiPlot .mspGroup");
            const tileHeight = (this.height / thisMPlot.visibleDimCount);
            mspGroup.selectAll(".rightHeader")
                .data(d3.range(this.visibleDimCount))
                .join(enter => enter.append("text")
                .attr("class", "rightHeader Header")
                .on("click", ScatterPlotMatrix.prototype.clickRowTitle.bind(thisMPlot)), update => update, exit => exit.remove())
                .classed("input", function (rowIndex) { return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]].isInput(); })
                .classed("output", function (rowIndex) { return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]].isOutput(); })
                .classed("zAxis", function (colIndex) { return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]; })
                .attr("transform", function (rowIndex) {
                if (thisMPlot.rotateTitle) {
                    return `translate(${thisMPlot.width + 10}, ${tileHeight * (rowIndex + 0.3)})rotate(80)`;
                }
                else {
                    return `translate(${thisMPlot.width + 10}, ${tileHeight * (rowIndex + 0.5)})rotate(90)`;
                }
            })
                .each(function (rowIndex) {
                spm.ScatterPlot.tspanColumnTitleBT(d3.select(this), thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]]);
            })
                // @ts-ignore
                .attr("text-anchor", thisMPlot.rotateTitle ? null : "middle");
        }
        initBottomHeaders() {
            const thisMPlot = this;
            const mspGroup = d3.select(this.bindto + " .MultiPlot .mspGroup");
            const tileWidth = (this.width / thisMPlot.visibleDimCount);
            mspGroup.selectAll(".bottomHeader")
                .data(d3.range(this.visibleDimCount))
                .join(enter => enter.append("text")
                .attr("class", "bottomHeader Header")
                .on("click", ScatterPlotMatrix.prototype.clickColTitle.bind(thisMPlot)), update => update, exit => exit.remove())
                .classed("input", function (colIndex) { return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]].isInput(); })
                .classed("output", function (colIndex) { return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]].isOutput(); })
                .classed("zAxis", function (colIndex) { return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]; })
                .attr("transform", function (colIndex) {
                if (thisMPlot.rotateTitle) {
                    return `translate(${tileWidth * (colIndex + 0.3)}, ${thisMPlot.height + 55})rotate(-10)`;
                }
                else {
                    return `translate(${tileWidth * (colIndex + 0.5)}, ${thisMPlot.height + 45})`;
                }
            })
                .each(function (colIndex) {
                spm.ScatterPlot.tspanColumnTitleTB(d3.select(this), thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]);
            })
                // @ts-ignore
                .attr("text-anchor", thisMPlot.rotateTitle ? null : "middle");
        }
        initLeftHeaders() {
            const thisMPlot = this;
            const mspGroup = d3.select(this.bindto + " .MultiPlot .mspGroup");
            const tileHeight = (this.height / thisMPlot.visibleDimCount);
            mspGroup.selectAll(".leftHeader")
                .data(d3.range(this.visibleDimCount))
                .join(enter => enter.append("text")
                .attr("class", "leftHeader Header")
                .on("click", ScatterPlotMatrix.prototype.clickRowTitle.bind(thisMPlot)), update => update, exit => exit.remove())
                .classed("input", function (rowIndex) { return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]].isInput(); })
                .classed("output", function (rowIndex) { return thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]].isOutput(); })
                .classed("zAxis", function (colIndex) { return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]; })
                .attr("transform", function (rowIndex) {
                if (thisMPlot.rotateTitle) {
                    return `translate(-55, ${tileHeight * (rowIndex + 0.8)})rotate(260)`;
                }
                else {
                    return `translate(-45, ${tileHeight * (rowIndex + 0.5)})rotate(270)`;
                }
            })
                .each(function (rowIndex) {
                spm.ScatterPlot.tspanColumnTitleBT(d3.select(this), thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.yStartingDimIndex + rowIndex]]);
            })
                // @ts-ignore
                .attr("text-anchor", thisMPlot.rotateTitle ? null : "middle");
        }
        clickColTitle(colIndex) {
            const dim = this.spData.dimensions[this.xStartingDimIndex + colIndex];
            if (this.zColumn !== null && this.zColumn.dim === dim) {
                this.setZAxis(null);
            }
            else {
                this.setZAxis(dim);
            }
            this.updateZAxisHeaders();
        }
        clickRowTitle(rowIndex) {
            const dim = this.spData.dimensions[this.yStartingDimIndex + rowIndex];
            if (this.zColumn !== null && this.zColumn.dim === dim) {
                this.setZAxis(null);
            }
            else {
                this.setZAxis(dim);
            }
            this.updateZAxisHeaders();
        }
        updateZAxisHeaders() {
            const thisMPlot = this;
            const mspGroup = d3.select(this.bindto + " .MultiPlot .mspGroup");
            mspGroup.selectAll(".Header")
                .classed("zAxis", function (colIndex) { return thisMPlot.zColumn === thisMPlot.spData.columns[thisMPlot.spData.dimensions[thisMPlot.xStartingDimIndex + colIndex]]; });
        }
        xSubPlot(plot) {
            const tileWidth = (this.width / this.visibleDimCount);
            return plot.col * tileWidth /* + ScatterPlot.margin.l*/;
        }
        ySubPlot(plot) {
            const tileHeight = (this.height / this.visibleDimCount);
            return plot.row * tileHeight /* + ScatterPlot.margin.t*/;
        }
        //***********************************
        //********** About "ZAxis" **********
        //***********************************
        appendZAxisSelector() {
            const thisMPlot = this;
            d3.selectAll(this.bindto + " .ParamSelect").data([ScatterPlotMatrix.zAxisClass]).
                append("select")
                .on("change", function () {
                thisMPlot.updateZAxisFromGui();
            })
                .selectAll("option")
                .data(this.spData.dimensions)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            if (this.zColumn !== null) {
                const paramIndex = this.spData.dimensions.indexOf(this.zColumn.dim);
                d3.select(this.bindto + " .ParamSelect > select")
                    .property("selectedIndex", paramIndex);
            }
        }
        initZAxisUsedCB() {
            d3.select(`#${this.id()}_zAxisUsed`)
                .property("checked", this.zColumn !== null)
                .on("change", ScatterPlotMatrix.prototype.updateZAxisFromGui.bind(this));
        }
        updateZAxisFromGui() {
            if (d3.select(`#${this.id()}_zAxisUsed`).property("checked")) {
                const zAxisSelectNode = d3.select(this.bindto + " .ParamSelect.ZAxis>select").node();
                if (zAxisSelectNode) {
                    this.setZAxis(this.spData.dimensions[zAxisSelectNode.selectedIndex]);
                }
            }
            else {
                this.setZAxis(null);
            }
        }
        //******************************************************
        //********** "Tooltip/Filter/Zoom" select box **********
        //******************************************************
        appendMouseModeSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .mouseModeSelect").append("select")
                .on("change", ScatterPlotMatrix.prototype.mouseModeChange.bind(thisMPlot))
                .selectAll("option")
                .data(spm.SpConst.mouseModeList)
                .enter().append("option")
                .text(function (d) { return d.label; })
                .attr("value", function (d) { return d.key; });
            this.mouseModeChange();
        }
        mouseModeChange() {
            const mouseModeSelect = d3.select(this.bindto + " .mouseModeSelect > select").node();
            if (mouseModeSelect) {
                this.changeMouseMode(mouseModeSelect.options[mouseModeSelect.selectedIndex].value);
            }
        }
        changeMouseMode(mouseMode) {
            this.mouseMode = mouseMode;
            this.scatterPlotList.forEach(plot => {
                plot.changeMouseMode(mouseMode);
            });
            this.diagPlotList.forEach(plot => {
                plot.changeMouseMode(mouseMode);
            });
            const modeIndex = spm.SpConst.mouseModeList.findIndex(mode => mode.key === mouseMode);
            d3.select(this.bindto + " .mouseModeSelect > select")
                .property("selectedIndex", modeIndex);
            this.fixBrush();
        }
        fixBrush() {
            this.scatterPlotList.forEach(plot => {
                plot.fixBrush();
            });
            this.diagPlotList.forEach(plot => {
                plot.fixBrush();
            });
        }
        //***************************************************************
        //********** About "distribRep/Regression" check boxes **********
        //***************************************************************
        appendDistribRepSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .distribRepSelect").append("select")
                .on("change", function () {
                const rep = spm.SpConst.distribRepList[this.selectedIndex];
                thisMPlot.setDistribType(rep.key === spm.SpConst.histogramRep.key ? spm.DistributionPlot.HISTO_REP : spm.DistributionPlot.DENS_REP);
            })
                .selectAll("option")
                .data(spm.SpConst.distribRepList)
                .enter().append("option")
                .text(function (d) { return d.label; })
                .attr("value", function (d) { return d.key; });
            const histoRep = (this.distribType & spm.DistributionPlot.HISTO_REP) ? spm.SpConst.histogramRep.key : spm.SpConst.densityRep.key;
            const repIndex = spm.SpConst.distribRepList.findIndex(distribRep => distribRep.key === histoRep);
            d3.select(this.bindto + " .distribRepSelect > select")
                .property("selectedIndex", repIndex);
        }
        initRegressionCB() {
            const thisMPlot = this;
            d3.select(`#${this.id()}_linearRegr`)
                .property("checked", (this.regressionType & spm.RegressionPlot.LINEAR_REP) !== 0)
                .on("change", function () {
                if (d3.select(this).property("checked")) {
                    thisMPlot.setRegressionType(thisMPlot.regressionType | spm.RegressionPlot.LINEAR_REP);
                }
                else {
                    thisMPlot.setRegressionType(thisMPlot.regressionType ^ spm.RegressionPlot.LINEAR_REP);
                }
            });
            d3.select(`#${this.id()}_loess`)
                .property("checked", (this.regressionType & spm.RegressionPlot.LOESS_REP) !== 0)
                .on("change", function () {
                if (d3.select(this).property("checked")) {
                    thisMPlot.setRegressionType(thisMPlot.regressionType | spm.RegressionPlot.LOESS_REP);
                }
                else {
                    thisMPlot.setRegressionType(thisMPlot.regressionType ^ spm.RegressionPlot.LOESS_REP);
                }
            });
        }
        appendContCsSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .contCsSelect").append("select")
                .on("change", function () {
                const contCsKey = spm.SpConst.CONTINUOUS_CS_IDS[this.selectedIndex];
                thisMPlot.setContinuousColorScale(contCsKey);
            })
                .selectAll("option")
                .data(spm.SpConst.CONTINUOUS_CS_IDS)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            const contCsIndex = spm.SpConst.CONTINUOUS_CS_IDS.indexOf(this.continuousCsId);
            d3.select(this.bindto + " .contCsSelect > select")
                .property("selectedIndex", contCsIndex);
        }
        appendCatCsSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .catCsSelect").append("select")
                .on("change", function () {
                const catCsKey = spm.SpConst.CATEGORIAL_CS_IDS[this.selectedIndex];
                thisMPlot.setCategoricalColorScale(catCsKey);
            })
                .selectAll("option")
                .data(spm.SpConst.CATEGORIAL_CS_IDS)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            const catCsIndex = spm.SpConst.CATEGORIAL_CS_IDS.indexOf(this.categoricalCsId);
            d3.select(this.bindto + " .catCsSelect > select")
                .property("selectedIndex", catCsIndex);
        }
        appendCorrTypeSelect() {
            const thisMPlot = this;
            const corrPlotTypes = [spm.CorrPlot.EMPTY_REP, spm.CorrPlot.CIRCLES_REP, spm.CorrPlot.TEXT_REP, spm.CorrPlot.ABS_TEXT_REP];
            d3.select(this.bindto + " .corrTypeSelect").append("select")
                .on("change", function () {
                const corrPlotType = corrPlotTypes[this.selectedIndex];
                thisMPlot.setCorrPlotType(corrPlotType);
            })
                .selectAll("option")
                .data(corrPlotTypes)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            const corrTypeIndex = corrPlotTypes.indexOf(this.corrPlotType);
            d3.select(this.bindto + " .corrTypeSelect > select")
                .property("selectedIndex", corrTypeIndex);
        }
        appendCorrCsSelect() {
            const thisMPlot = this;
            d3.select(this.bindto + " .corrCsSelect").append("select")
                .on("change", function () {
                const contCsKey = spm.SpConst.CONTINUOUS_CS_IDS[this.selectedIndex];
                thisMPlot.setCorrPlotCS(contCsKey);
            })
                .selectAll("option")
                .data(spm.SpConst.CONTINUOUS_CS_IDS)
                .enter().append("option")
                .text(function (d) { return d; })
                .attr("value", function (d) { return d; });
            const corrCsIndex = spm.SpConst.CONTINUOUS_CS_IDS.indexOf(this.corrPlotCsId);
            d3.select(this.bindto + " .corrCsSelect > select")
                .property("selectedIndex", corrCsIndex);
        }
        //**************************************************
        //********** API (called by R htmlwidget) **********
        //**************************************************
        // eslint-disable-next-line max-lines-per-function
        setZAxis(dim, dontRedraw) {
            if (dim && !this.spData.columns[dim]) {
                console.error("setZAxis called with unknown dim:", dim);
                return;
            }
            const zAxisDim = this.zColumn === null
                ? null
                : this.zColumn.dim;
            if (dim === zAxisDim) {
                return;
            }
            const thisMPlot = this;
            this.zColumn = dim
                ? this.spData.columns[dim]
                : null;
            this.scatterPlotList.forEach(plot => {
                const plotDim = plot.zColumn === null
                    ? null
                    : plot.zColumn.dim;
                if (plotDim !== dim) {
                    plot.setZColumn(thisMPlot.zColumn);
                    if (!dontRedraw) {
                        plot.draw(spm.ScatterPlot.Z_AXIS);
                    }
                }
            });
            this.diagPlotList.forEach(plot => {
                const plotDim = plot.zColumn === null
                    ? null
                    : plot.zColumn.dim;
                if (plotDim !== dim) {
                    plot.setZColumn(thisMPlot.zColumn);
                    if (!dontRedraw) {
                        plot.draw(spm.ScatterPlot.Z_AXIS);
                    }
                }
            });
            this.corrPlotList.forEach(plot => {
                const plotDim = plot.zColumn === null
                    ? null
                    : plot.zColumn.dim;
                if (plotDim !== dim) {
                    plot.setZColumn(thisMPlot.zColumn);
                    if (!dontRedraw) {
                        plot.draw(spm.ScatterPlot.Z_AXIS);
                    }
                }
            });
            this.dispatch.call(ScatterPlotMatrix.PLOT_EVENT, undefined, {
                type: ScatterPlotMatrix.ZAXIS_EVENT,
                value: dim
            });
            if (d3.select(this.bindto + " .mspDiv.withWidgets").size() !== 0) {
                d3.select(`#${this.id()}_zAxisUsed`).property("checked", this.zColumn !== null);
                if (this.zColumn !== null) {
                    const zAxisSelectNode = d3.select(this.bindto + " .ParamSelect.ZAxis>select").node();
                    if (zAxisSelectNode) {
                        const selectedIndex = this.spData.dimensions.indexOf(this.zColumn.dim);
                        if (selectedIndex === -1) {
                            console.warn("Dim of Z axis not found => selectedIndex cannot be updated");
                        }
                        else {
                            zAxisSelectNode.selectedIndex = selectedIndex;
                        }
                    }
                }
            }
        }
        setDistribType(distribType) {
            if (distribType & (spm.DistributionPlot.HISTO_REP | spm.DistributionPlot.DENS_REP)) {
                this.distribType = distribType;
                this.scatterPlotList.forEach(plot => {
                    plot.distribRepChange(distribType);
                });
                this.diagPlotList.forEach(plot => {
                    plot.distribRepChange(distribType);
                });
            }
            else {
                console.error("Invalid distribution type code: " + distribType);
            }
        }
        setRegressionType(regressionType) {
            if (regressionType === 0 || regressionType & (spm.RegressionPlot.LINEAR_REP | spm.RegressionPlot.LOESS_REP)) {
                this.regressionType = regressionType;
                this.scatterPlotList.forEach(plot => {
                    plot.regressionRepChange(regressionType);
                });
            }
            else {
                console.error("Invalid regression type code: " + regressionType);
            }
        }
        setContinuousColorScale(continuousCsId) {
            if (spm.SpConst.CONTINUOUS_CS_IDS.includes(continuousCsId)) {
                this.continuousCsId = continuousCsId;
                this.scatterPlotList.forEach(plot => {
                    plot.continuousCsId = continuousCsId;
                });
                this.diagPlotList.forEach(plot => {
                    plot.continuousCsId = continuousCsId;
                });
                this.updatePlots(spm.ScatterPlot.PALETTE);
            }
            else {
                console.error("Unknown continuous color scale: " + continuousCsId);
            }
        }
        setCategoricalColorScale(categoricalCsId) {
            if (spm.SpConst.CATEGORIAL_CS_IDS.includes(categoricalCsId)) {
                this.categoricalCsId = categoricalCsId;
                this.scatterPlotList.forEach(plot => {
                    plot.categoricalCsId = categoricalCsId;
                });
                this.diagPlotList.forEach(plot => {
                    plot.categoricalCsId = categoricalCsId;
                });
                this.corrPlotList.forEach(plot => {
                    plot.categoricalCsId = categoricalCsId;
                });
                this.updatePlots(spm.ScatterPlot.PALETTE);
            }
            else {
                console.error("Unknown categorical color scale: " + categoricalCsId);
            }
        }
        setCorrPlotType(corrPlotType) {
            if ([spm.CorrPlot.EMPTY_REP, spm.CorrPlot.CIRCLES_REP, spm.CorrPlot.TEXT_REP, spm.CorrPlot.ABS_TEXT_REP].includes(corrPlotType)) {
                this.corrPlotType = corrPlotType;
                this.corrPlotList.forEach(plot => {
                    plot.repType = corrPlotType;
                    plot.draw(spm.ScatterPlot.SHAPE);
                });
            }
            else {
                console.error("Unknown correlation plot type: " + corrPlotType);
            }
        }
        setCorrPlotCS(corrPlotCsId) {
            if (spm.SpConst.CONTINUOUS_CS_IDS.includes(corrPlotCsId)) {
                this.corrPlotCsId = corrPlotCsId;
                this.corrPlotList.forEach(plot => {
                    plot.corrCsId = corrPlotCsId;
                });
                this.updatePlots(spm.ScatterPlot.PALETTE);
            }
            else {
                console.error("Unknown correlation color scale: " + corrPlotCsId);
            }
        }
        setCutoffs(spCutoffsList) {
            this.spData.setCutoffs(spCutoffsList);
            this.fixBrush();
        }
        setKeptColumns(keptColumns) {
            const thisPlot = this;
            if (Array.isArray(keptColumns)) {
                this.spData.dimensions = d3.keys(this.spData.sampleData[0]).filter((_dim, i) => keptColumns[i]);
            }
            else {
                this.spData.dimensions = d3.keys(this.spData.sampleData[0]).filter(function (dim) {
                    let toKeep = thisPlot.spData.dimensions.includes(dim);
                    if (typeof keptColumns[dim] !== "undefined") {
                        toKeep = keptColumns[dim];
                    }
                    return toKeep;
                });
            }
            this.adjustVisibleDimensions();
            this.tilesNumberChanged();
            this.xBrushSlider.update();
            this.yBrushSlider.update();
        }
        getPlotConfig() {
            const allDimensions = d3.keys(this.spData.sampleData[0]);
            const controlWidgets = d3.select(this.bindto + " .mspDiv").classed("withWidgets");
            const categorical = allDimensions.map(dim => this.spData.columns[dim]
                ? this.spData.columns[dim].categories
                : null);
            const inputColumns = allDimensions.map(dim => this.spData.columns[dim] && this.spData.columns[dim].ioType === spm.Column.INPUT);
            const keptColumns = allDimensions.map(dim => this.spData.dimensions.includes(dim));
            const zAxisDim = this.zColumn === null
                ? null
                : this.zColumn.dim;
            const columnLabels = allDimensions.map(dim => this.spData.columns[dim] && this.spData.columns[dim].label
                ? this.spData.columns[dim].label
                : dim);
            return {
                data: [],
                rowLabels: this.spData.rowLabels,
                categorical: categorical,
                inputColumns: inputColumns,
                keptColumns: keptColumns,
                cutoffs: this.spData.getXYCutoffs(),
                zAxisDim: zAxisDim,
                distribType: this.distribType,
                regressionType: this.regressionType,
                corrPlotType: this.corrPlotType,
                corrPlotCS: this.corrPlotCsId,
                rotateTitle: this.rotateTitle,
                columnLabels: columnLabels,
                continuousCS: this.continuousCsId,
                categoricalCS: this.categoricalCsId,
                controlWidgets: controlWidgets,
                cssRules: this.style.cssRules,
                plotProperties: this.style.plotProperties,
                slidersPosition: {
                    dimCount: this.visibleDimCount,
                    xStartingDimIndex: this.xStartingDimIndex,
                    yStartingDimIndex: this.yStartingDimIndex
                }
            };
        }
    }
    ScatterPlotMatrix.margin = { t: 95, r: 95, b: 95, l: 95 };
    ScatterPlotMatrix.zAxisClass = "ZAxis";
    ScatterPlotMatrix.PLOT_EVENT = "plotEvent";
    ScatterPlotMatrix.ZAXIS_EVENT = "zAxisChange";
    ScatterPlotMatrix.MAX_VISIBLE_DIMS = 16;
    ScatterPlotMatrix.DEFAULT_SLIDERS_POSITION = {
        dimCount: 8,
        xStartingDimIndex: 0,
        yStartingDimIndex: 0
    };
    spm.ScatterPlotMatrix = ScatterPlotMatrix;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class SelectionPad {
        constructor(multipleScatterPlot) {
            // Index of last selected row (useful when Shift key is used)
            this.lastSelectedPlotIndex = 0;
            this.selectedPlots = new Set();
            this.msp = multipleScatterPlot;
            this.selectionPadId = null;
        }
        ;
        generate(selectionPadId) {
            this.selectionPadId = selectionPadId;
            this.selectedPlots.add(this.msp.scatterPlotList[0]);
            return this;
        }
        update() {
            const thisPad = this;
            const selectionPad = (this.selectionPadId && !d3.select("#" + this.selectionPadId).empty())
                ? d3.select("#" + this.selectionPadId)
                : d3.select(this.msp.bindto + " .selectionPad");
            selectionPad.select("svg").remove();
            const svg = selectionPad.append("svg")
                .attr("width", SelectionPad.spSize + 2 * SelectionPad.spMargin)
                .attr("height", SelectionPad.spSize + 2 * SelectionPad.spMargin)
                .append("g")
                .attr("transform", "translate(" + SelectionPad.spMargin + "," + SelectionPad.spMargin + ")");
            const visiblePlotList = thisPad.msp.visibilityPad.visibleScatterPlots();
            svg.selectAll(".selectionCell")
                .data(visiblePlotList)
                .enter().append("rect")
                .attr("class", "selectionCell")
                .attr("x", function (plot) {
                return thisPad.msp.visibilityPad.vcell(plot).vcol * SelectionPad.spSize / thisPad.msp.visibilityPad.nCol;
            })
                .attr("y", function (plot) {
                return thisPad.msp.visibilityPad.vcell(plot).vrow * SelectionPad.spSize / thisPad.msp.visibilityPad.nRow;
            })
                .attr("width", SelectionPad.spSize / this.msp.visibilityPad.nCol)
                .attr("height", SelectionPad.spSize / this.msp.visibilityPad.nRow)
                .style("fill", function (plot) {
                return thisPad.selectedPlots.has(plot) ? "steelblue" : "#999";
            })
                .attr("fill-opacity", 0.8)
                .style("stroke", "aliceblue")
                .style("stroke-width", "1")
                .on("mouseover", function () {
                d3.select(this).attr("fill-opacity", 1);
            })
                .on("mouseout", function () {
                d3.select(this).attr("fill-opacity", 0.8);
            })
                .on("click", function (plot) {
                thisPad.click(plot);
            });
            return this;
        }
        click(plot) {
            const thisPad = this;
            let selectionPad = d3.select("#selectionPad");
            if (selectionPad.empty()) {
                selectionPad = d3.select(this.msp.bindto + " .selectionPad");
            }
            const svg = selectionPad.select("svg");
            const plotIndex = plot.index;
            if (d3.event.ctrlKey && d3.event.shiftKey && this.lastSelectedPlotIndex !== -1) {
                // @ts-ignore
                const range = plotIndex < this.lastSelectedPlotIndex
                    ? d3.range(plotIndex, this.lastSelectedPlotIndex + 1)
                    : d3.range(this.lastSelectedPlotIndex, plotIndex + 1);
                this.addSelection(range);
                this.lastSelectedPlotIndex = plotIndex;
            }
            else if (d3.event.shiftKey && this.lastSelectedPlotIndex !== -1) {
                // @ts-ignore
                const range = plotIndex < this.lastSelectedPlotIndex
                    ? d3.range(plotIndex, this.lastSelectedPlotIndex + 1)
                    : d3.range(this.lastSelectedPlotIndex, plotIndex + 1);
                this.setSelection(range);
                this.lastSelectedPlotIndex = plotIndex;
            }
            else if (d3.event.ctrlKey) {
                if (this.selectedPlots.has(plot)) {
                    this.lastSelectedPlotIndex = -1;
                    this.removeSelection([plotIndex]);
                }
                else {
                    this.lastSelectedPlotIndex = plotIndex;
                    this.addSelection([plotIndex]);
                }
            }
            else {
                this.lastSelectedPlotIndex = plotIndex;
                this.setSelection([plotIndex]);
            }
            svg.selectAll(".selectionCell")
                .style("fill", function (plot2) {
                return thisPad.selectedPlots.has(plot2) ? "steelblue" : "#999";
            });
            this.sendSelectionEvent();
        }
        addSelection(range) {
            this.rangePlots(range).forEach(plot => this.selectedPlots.add(plot));
        }
        setSelection(range) {
            this.selectedPlots.clear();
            this.rangePlots(range).forEach(plot => this.selectedPlots.add(plot));
        }
        removeSelection(range) {
            this.rangePlots(range).forEach(plot => this.selectedPlots.delete(plot));
        }
        rangePlots(range) {
            const visiblePlots = this.msp.visibilityPad.visibleScatterPlots();
            return visiblePlots.filter(plot => range.includes(plot.index));
        }
        updateAxesSelectors() {
            const plot = [...this.selectedPlots][0];
            this.updateXSelector(plot.xColumn.dim);
            this.updateYSelector(plot.yColumn.dim);
            if (plot.zColumn !== null) {
                this.updateZSelector(plot.zColumn.dim);
            }
        }
        updateXSelector(dim) {
            const xAxisSelectNode = d3.select(this.msp.bindto + " .ParamSelect.XAxis>select").node();
            if (xAxisSelectNode) {
                const selectedIndex = this.msp.spData.dimensions.indexOf(dim);
                if (selectedIndex === -1) {
                    console.error("Dim of X axis not found => selectedIndex cannot be updated");
                }
                else {
                    xAxisSelectNode.selectedIndex = selectedIndex;
                }
            }
        }
        updateYSelector(dim) {
            const yAxisSelectNode = d3.select(this.msp.bindto + " .ParamSelect.YAxis>select").node();
            if (yAxisSelectNode) {
                const selectedIndex = this.msp.spData.dimensions.indexOf(dim);
                if (selectedIndex === -1) {
                    console.error("Dim of Y axis not found => selectedIndex cannot be updated");
                }
                else {
                    yAxisSelectNode.selectedIndex = selectedIndex;
                }
            }
        }
        updateZSelector(dim) {
            const zAxisSelectNode = d3.select(this.msp.bindto + " .ParamSelect.ZAxis>select").node();
            if (zAxisSelectNode) {
                const selectedIndex = this.msp.spData.dimensions.indexOf(dim);
                if (selectedIndex === -1) {
                    const allDimensions = d3.keys(this.msp.spData.sampleData[0]);
                    if (allDimensions.indexOf(dim) === -1) {
                        console.error("Dim of Z axis not found => selectedIndex cannot be updated");
                    }
                    else {
                        console.warn("Dim of Z axis not found => selectedIndex cannot be updated");
                    }
                }
                else {
                    zAxisSelectNode.selectedIndex = selectedIndex;
                }
            }
        }
        setXAxis(dim) {
            if (!this.msp.spData.columns[dim]) {
                console.error("setXAxis called with unknown dim:", dim);
                return;
            }
            const thisPad = this;
            this.selectedPlots.forEach(plot => {
                if (plot.xColumn.dim !== dim) {
                    plot.setXColumn(thisPad.msp.spData.columns[dim]);
                    plot.draw(spm.ScatterPlot.SHAPE);
                }
            });
            const plot = [...this.selectedPlots][0];
            this.updateXSelector(plot.xColumn.dim);
        }
        setYAxis(dim) {
            if (!this.msp.spData.columns[dim]) {
                console.error("setYAxis called with unknown dim:", dim);
                return;
            }
            const thisPad = this;
            this.selectedPlots.forEach(plot => {
                if (plot.yColumn.dim !== dim) {
                    plot.setYColumn(thisPad.msp.spData.columns[dim]);
                    plot.draw(spm.ScatterPlot.SHAPE);
                }
            });
            const plot = [...this.selectedPlots][0];
            this.updateYSelector(plot.yColumn.dim);
        }
        setZAxis(dim) {
            if (dim !== null && !this.msp.spData.columns[dim]) {
                console.error("setZAxis called with unknown dim:", dim);
                return;
            }
            const zColumn = dim === null
                ? null
                : this.msp.spData.columns[dim];
            this.selectedPlots.forEach(plot => {
                const plotDim = plot.zColumn === null
                    ? null
                    : plot.zColumn.dim;
                if (plotDim !== dim) {
                    plot.setZColumn(zColumn);
                    plot.draw(spm.ScatterPlot.Z_AXIS);
                }
            });
            if (d3.select(this.msp.bindto + " .mspDiv.withWidgets").size() !== 0) {
                d3.select(`#${this.msp.id()}_zAxisUsed`).property("checked", zColumn !== null);
                if (zColumn !== null) {
                    this.updateZSelector(zColumn.dim);
                }
            }
        }
        sendSelectionEvent() {
            this.updateAxesSelectors();
            const selectionValue = [...this.selectedPlots].map(plot => {
                const zDim = plot.zColumn === null
                    ? null
                    : plot.zColumn.dim;
                return {
                    plotIndex: plot.index,
                    xDim: plot.xColumn.dim,
                    yDim: plot.yColumn.dim,
                    zDim: zDim
                };
            });
            const eventDescr = {
                type: spm.MultipleScatterPlot.SELECTION_EVENT,
                value: selectionValue
            };
            this.msp.dispatch.call(spm.MultipleScatterPlot.PLOT_EVENT, undefined, eventDescr);
        }
    }
    SelectionPad.spSize = 60;
    SelectionPad.spMargin = 1;
    spm.SelectionPad = SelectionPad;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class SpConst {
    }
    SpConst.CONTINUOUS_CS = {
        // From d3-scale.
        Viridis: d3.interpolateViridis,
        Inferno: d3.interpolateInferno,
        Magma: d3.interpolateMagma,
        Plasma: d3.interpolatePlasma,
        Warm: d3.interpolateWarm,
        Cool: d3.interpolateCool,
        Rainbow: d3.interpolateRainbow,
        CubehelixDefault: d3.interpolateCubehelixDefault,
        // From d3-scale-chromatic
        Blues: d3.interpolateBlues,
        Greens: d3.interpolateGreens,
        Greys: d3.interpolateGreys,
        Oranges: d3.interpolateOranges,
        Purples: d3.interpolatePurples,
        Reds: d3.interpolateReds,
        BuGn: d3.interpolateBuGn,
        BuPu: d3.interpolateBuPu,
        GnBu: d3.interpolateGnBu,
        OrRd: d3.interpolateOrRd,
        PuBuGn: d3.interpolatePuBuGn,
        PuBu: d3.interpolatePuBu,
        PuRd: d3.interpolatePuRd,
        RdBu: d3.interpolateRdBu,
        RdPu: d3.interpolateRdPu,
        YlGnBu: d3.interpolateYlGnBu,
        YlGn: d3.interpolateYlGn,
        YlOrBr: d3.interpolateYlOrBr,
        YlOrRd: d3.interpolateYlOrRd
    };
    SpConst.CONTINUOUS_CS_IDS = Object.keys(SpConst.CONTINUOUS_CS);
    SpConst.CATEGORIAL_CS = {
        Category10: d3.scaleOrdinal(d3.schemeCategory10),
        Accent: d3.scaleOrdinal(d3.schemeAccent),
        Dark2: d3.scaleOrdinal(d3.schemeDark2),
        Paired: d3.scaleOrdinal(d3.schemePaired),
        Set1: d3.scaleOrdinal(d3.schemeSet1)
    };
    SpConst.CATEGORIAL_CS_IDS = Object.keys(SpConst.CATEGORIAL_CS);
    SpConst.dblClickDelay = 350;
    SpConst.histogramRep = {
        key: "histogramRep",
        label: "Histogram"
    };
    SpConst.densityRep = {
        key: "densityRep",
        label: "Density Plot"
    };
    SpConst.distribRepList = [SpConst.histogramRep, SpConst.densityRep];
    SpConst.tooltipMouse = {
        key: "tooltip",
        label: "Tooltip"
    };
    SpConst.filterMouse = {
        key: "filter",
        label: "Filter"
    };
    SpConst.zoomMouse = {
        key: "zoom",
        label: "Zoom"
    };
    SpConst.mouseModeList = [SpConst.tooltipMouse, SpConst.filterMouse, SpConst.zoomMouse];
    SpConst.CAT_RATIO = 4;
    spm.SpConst = SpConst;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class SpData {
        // eslint-disable-next-line max-lines-per-function
        constructor(config) {
            this.dimensions = [];
            this.rowLabels = null;
            this.sampleData = [];
            this.cutSampleData = null;
            this.columns = {}; // Column for each dimension
            this.jitterXValues = [];
            this.jitterYValues = [];
            this.cutRows = [];
            this.dispatch = d3.dispatch(SpData.ROW_FILTER_EVENT, SpData.HL_POINT_EVENT, SpData.HL_GRAPH_EVENT);
            this.rowFilterMap = new Map();
            this.rowLabels = config.rowLabels;
            const thisData = this;
            SpData.checkData(config);
            SpData.checkCategorical(config);
            SpData.checkColumnLabels(config);
            SpData.checkInputColumns(config);
            // Init 'sampleData'
            this.sampleData = [];
            config.data.forEach(function (r) {
                const curRow = {};
                config.data.columns.forEach((dim, i) => {
                    const categories = Array.isArray(config.categorical)
                        ? config.categorical[i]
                        : null;
                    const cellValue = r[dim];
                    if (typeof cellValue === "undefined") {
                        curRow[dim] = NaN;
                    }
                    else if (categories) {
                        let catIndex = categories.indexOf(cellValue.toString());
                        if (catIndex === -1) {
                            catIndex = categories.indexOf(+cellValue);
                        }
                        curRow[dim] = (catIndex === -1) ? NaN : catIndex;
                    }
                    else {
                        curRow[dim] = +cellValue;
                    }
                });
                thisData.sampleData.push(curRow);
            });
            // Init cutoffs
            if (!this.checkCutoffs(config.cutoffs)) {
                config.cutoffs = null;
            }
            if (Array.isArray(config.cutoffs)) {
                config.cutoffs.forEach(spCutoffs => {
                    this.setXYCutoffs(spCutoffs.xDim, spCutoffs.yDim, spCutoffs.xyCutoffs);
                });
            }
            // Build 'Column' instances and init jitter values
            const allDimensions = d3.keys(this.sampleData[0]);
            const nanColumns = allDimensions.map(dim => this.sampleData.every(row => isNaN(row[dim])));
            this.dimensions = allDimensions.filter((_dim, i) => !(nanColumns[i] || (Array.isArray(config.keptColumns) && !config.keptColumns[i])));
            allDimensions.forEach((dim, i) => {
                const isInput = Array.isArray(config.inputColumns) ? config.inputColumns[i] : true;
                this.columns[dim] = new spm.Column(dim, i, this, Array.isArray(config.columnLabels) ? config.columnLabels[i] : dim, Array.isArray(config.categorical) ? config.categorical[i] : null, isInput ? spm.Column.INPUT : spm.Column.OUTPUT);
                if (this.columns[dim].categories !== null && !thisData.jitterXValues.length) {
                    thisData.jitterXValues = this.sampleData
                        .map(_row => (Math.random() - 0.5) / spm.SpConst.CAT_RATIO);
                    thisData.jitterYValues = this.sampleData
                        .map(_row => (Math.random() - 0.5) / spm.SpConst.CAT_RATIO);
                }
            });
        }
        static checkData(config) {
            if (!Array.isArray(config.data)) {
                throw new Error("given dataset is not a D3 friendly (row-oriented) data");
            }
            if (config.data.length === 0) {
                throw new Error("given dataset contains no line)");
            }
            if (typeof config.data.columns === "undefined") {
                config.data.columns = d3.keys(config.data[0]);
            }
        }
        static checkCategorical(config) {
            if (config.categorical) {
                if (Array.isArray(config.categorical)) {
                    if (config.categorical.length !== config.data.columns.length) {
                        console.error("Length of 'categorical' must be equal to the number of columns of 'data'");
                        config.categorical = null;
                    }
                }
                else {
                    console.error("'categorical' must be an array");
                    config.categorical = null;
                }
            }
        }
        static checkColumnLabels(config) {
            if (config.columnLabels) {
                if (Array.isArray(config.columnLabels)) {
                    if (config.columnLabels.length !== config.data.columns.length) {
                        console.error("Length of 'columnLabels' must be equal to the number of columns of 'data'");
                        config.columnLabels = null;
                    }
                }
                else {
                    console.error("'columnLabels' must be an array");
                    config.columnLabels = null;
                }
            }
        }
        static checkInputColumns(config) {
            if (config.inputColumns) {
                if (Array.isArray(config.inputColumns)) {
                    if (config.inputColumns.length !== config.data.columns.length) {
                        console.error("Length of 'inputColumns' must be equal to the number of columns of 'data'");
                        config.inputColumns = null;
                    }
                }
                else {
                    console.error("'inputColumns' must be an array");
                    config.inputColumns = null;
                }
            }
        }
        checkCutoffs(cutoffs) {
            if (cutoffs) {
                if (Array.isArray(cutoffs)) {
                    const allDimensions = d3.keys(this.sampleData[0]);
                    for (let spcIndex = 0; spcIndex < cutoffs.length; spcIndex++) {
                        const spCutoffs = cutoffs[spcIndex];
                        if (typeof spCutoffs.xDim === "undefined") {
                            console.error(`spCutoffs ${spcIndex} has no 'xDim' attribute`);
                            return false;
                        }
                        if (allDimensions.indexOf(spCutoffs.xDim) === -1) {
                            console.error(`spCutoffs ${spcIndex} has unknown 'xDim' attribute: ${spCutoffs.xDim}`);
                            return false;
                        }
                        if (typeof spCutoffs.yDim === "undefined") {
                            console.error(`spCutoffs ${spcIndex} has no 'yDim' attribute`);
                            return false;
                        }
                        if (allDimensions.indexOf(spCutoffs.yDim) === -1) {
                            console.error(`spCutoffs ${spcIndex} has unknown 'yDim' attribute: ${spCutoffs.yDim}`);
                            return false;
                        }
                        if (typeof spCutoffs.xyCutoffs === "undefined") {
                            console.error(`spCutoffs ${spcIndex} has no 'xyCutoffs' attribute`);
                            return false;
                        }
                        if (!SpData.checkXYCutoffs(spCutoffs.xyCutoffs, spcIndex)) {
                            return false;
                        }
                    }
                }
                else {
                    console.error("'cutoffs' must be an array");
                    return false;
                }
            }
            return true;
        }
        static checkXYCutoffs(xyCutoffs, spcIndex) {
            if (Array.isArray(xyCutoffs)) {
                for (let i = 0; i < xyCutoffs.length; i++) {
                    const xyCutoff = xyCutoffs[i];
                    if (Array.isArray(xyCutoff)) {
                        if (!SpData.checkXYCutoff(xyCutoff, spcIndex)) {
                            return false;
                        }
                    }
                    else {
                        console.error("cutoff with a non array 'xyCutoff' found");
                        return false;
                    }
                }
            }
            else {
                console.error(`spCutoffs ${spcIndex} has an invalid 'xyCutoffs' (not an array)`);
                return false;
            }
            return true;
        }
        static checkXYCutoff(xyCutoff, spcIndex) {
            if (xyCutoff.length === 2) {
                for (let xyi = 0; xyi < xyCutoff.length; xyi++) {
                    const cutoff = xyCutoff[xyi];
                    if (Array.isArray(cutoff)) {
                        if (!SpData.checkCutoff(cutoff, spcIndex, xyi)) {
                            xyCutoff[xyi] = null;
                        }
                    }
                    else if (cutoff) {
                        console.error(`spCutoffs ${spcIndex} has an invalid 'cutoff' (not an array of length 2)`);
                        return false;
                    }
                }
            }
            else {
                console.error(`spCutoffs ${spcIndex} has an invalid 'xyCutoff' (length is not 2)`);
                return false;
            }
            return true;
        }
        static checkCutoff(cutoff, spcIndex, xyi) {
            if (cutoff.length !== 2) {
                console.error(`spCutoffs ${spcIndex} has an invalid 'cutoff' (length is not 2)`);
                return false;
            }
            else if (+cutoff[0] >= +cutoff[1]) {
                console.error(`spCutoffs ${spcIndex} has an invalid 'cutoff' for ${xyi === 0 ? "X" : "Y"} (${cutoff[0]} >= ${+cutoff[1]})`);
                return false;
            }
            return true;
        }
        updateCutRowsMask() {
            const thisData = this;
            if (this.cutRows.length !== this.sampleData.length) {
                this.cutRows = new Array(this.sampleData.length);
            }
            const rowFilterList = [...this.rowFilterMap.values()].reduce((accu, xmap) => accu.concat([...xmap.values()]), []);
            this.sampleData.forEach(function (row, i) {
                const isKept = rowFilterList.every(function (rowFilter) {
                    return rowFilter.isKeptRow(row);
                });
                thisData.cutRows[i] = isKept;
            });
            this.cutSampleData = null;
        }
        cutData() {
            const thisData = this;
            if (this.cutSampleData === null) {
                this.cutSampleData = this.sampleData.filter((_row, i) => thisData.cutRows[i]);
            }
            return this.cutSampleData;
        }
        rowFilterChange() {
            this.dispatch.call(SpData.ROW_FILTER_EVENT, undefined, SpData.ROW_FILTER_EVENT);
        }
        dispatchHlPointEvent(hlPointEvent) {
            this.dispatch.call(SpData.HL_POINT_EVENT, undefined, hlPointEvent);
        }
        on(type, callback) {
            // @ts-ignore
            this.dispatch.on(type, callback);
        }
        setCutoffs(spCutoffsList) {
            if (spCutoffsList) {
                if (this.checkCutoffs(spCutoffsList)) {
                    spCutoffsList.forEach(spCutoffs => {
                        this.setXYCutoffs(spCutoffs.xDim, spCutoffs.yDim, spCutoffs.xyCutoffs);
                    });
                    this.rowFilterChange();
                }
            }
            else {
                this.rowFilterMap.clear();
                this.rowFilterChange();
            }
        }
        getRowFilter(xDim, yDim) {
            let xMap = this.rowFilterMap.get(xDim);
            if (!xMap) {
                xMap = new Map;
                this.rowFilterMap.set(xDim, xMap);
            }
            let rowFilter = xMap.get(yDim);
            if (!rowFilter) {
                rowFilter = new spm.RowFilter(xDim, yDim);
                xMap.set(yDim, rowFilter);
            }
            return rowFilter;
        }
        setXYCutoffs(xDim, yDim, xyCutoffs) {
            if (xyCutoffs) {
                const [xMin, xMax] = d3.extent(this.sampleData, function (row) { return +row[xDim]; });
                if (typeof xMin !== "undefined" && typeof xMax !== "undefined") {
                    xyCutoffs.forEach(xyCutoff => {
                        if (xyCutoff[0]) {
                            if (xyCutoff[0][0] < xMin || xyCutoff[0][0] > xMax) {
                                xyCutoff[0][0] = xMin;
                            }
                            if (xyCutoff[0][1] > xMax || xyCutoff[0][1] < xMin) {
                                xyCutoff[0][1] = xMax;
                            }
                        }
                        else {
                            xyCutoff[0] = [xMin, xMax];
                        }
                    });
                }
                else {
                    console.error("Wrong domain for ", xDim);
                }
                const [yMin, yMax] = d3.extent(this.sampleData, function (row) { return +row[yDim]; });
                if (typeof yMin !== "undefined" && typeof yMax !== "undefined") {
                    xyCutoffs.forEach(xyCutoff => {
                        if (xyCutoff[1]) {
                            if (xyCutoff[1][0] < yMin || xyCutoff[1][0] > yMax) {
                                xyCutoff[1][0] = yMin;
                            }
                            if (xyCutoff[1][1] > yMax || xyCutoff[1][1] < yMin) {
                                xyCutoff[1][1] = yMax;
                            }
                        }
                        else {
                            xyCutoff[1] = [yMin, yMax];
                        }
                    });
                }
                else {
                    console.error("Wrong domain for ", yDim);
                }
            }
            this.getRowFilter(xDim, yDim).xyCutoffs = xyCutoffs;
        }
        getXYCutoffs() {
            const rowFilterList = [...this.rowFilterMap.values()].reduce((accu, xmap) => accu.concat([...xmap.values()]), []);
            return rowFilterList
                .filter(rowFilter => rowFilter.xyCutoffs !== null)
                .map(rowFilter => {
                return {
                    xDim: rowFilter.xDim,
                    yDim: rowFilter.yDim,
                    xyCutoffs: rowFilter.xyCutoffs
                };
            });
        }
    }
    SpData.ROW_FILTER_EVENT = "rowFilterEvent";
    SpData.HL_POINT_EVENT = "hlPointEvent";
    SpData.HL_GRAPH_EVENT = "hlGraphEvent";
    spm.SpData = SpData;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class Style {
        constructor(bindto) {
            this.plotProperties = {
                /** Color used when categories coloring is not applied */
                noCatColor: "#43665e",
                /** Color used to show in background full plots, with cutoffs */
                watermarkColor: "#e4e4e4",
                point: {
                    /** Radius used to draw points as circles */
                    radius: 2,
                    /** Opacity value used for points */
                    alpha: 0.5
                },
                regression: {
                    /** Width of path stroke */
                    strokeWidth: 4
                }
            };
            this.bindto = bindto;
        }
        initWith(cssRules, plotProperties) {
            this.cssRules = cssRules;
            if (plotProperties) {
                if (typeof plotProperties.noCatColor !== "undefined") {
                    if (Style.isValidColor(plotProperties.noCatColor)) {
                        this.plotProperties.noCatColor = plotProperties.noCatColor;
                    }
                    else {
                        console.error(`plotProperties.noCatColor ${plotProperties.noCatColor} is invalid`);
                    }
                }
                if (typeof plotProperties.watermarkColor !== "undefined") {
                    if (Style.isValidColor(plotProperties.watermarkColor)) {
                        this.plotProperties.watermarkColor = plotProperties.watermarkColor;
                    }
                    else {
                        console.error(`plotProperties.watermarkColor ${plotProperties.watermarkColor} is invalid`);
                    }
                }
            }
            if (plotProperties && plotProperties.point) {
                this.initPointPlotPropertiesWith(plotProperties.point);
            }
            if (plotProperties && plotProperties.regression) {
                this.initRegressionPlotPropertiesWith(plotProperties.regression);
            }
        }
        initPointPlotPropertiesWith(pointPlotProperties) {
            if (typeof pointPlotProperties.alpha !== "undefined") {
                if (Style.isValidAlpha(pointPlotProperties.alpha)) {
                    this.plotProperties.point.alpha = +pointPlotProperties.alpha;
                }
                else {
                    console.error(`plotProperties.alpha ${pointPlotProperties.alpha} is invalid`);
                }
            }
            if (typeof pointPlotProperties.radius !== "undefined") {
                if (Style.isPositiveNumber(pointPlotProperties.radius)) {
                    this.plotProperties.point.radius = +pointPlotProperties.radius;
                }
                else {
                    console.error(`plotProperties.point.radius ${pointPlotProperties.radius} is invalid`);
                }
            }
        }
        initRegressionPlotPropertiesWith(regressionPlotProperties) {
            if (typeof regressionPlotProperties.strokeWidth !== "undefined") {
                if (Style.isPositiveNumber(regressionPlotProperties.strokeWidth)) {
                    this.plotProperties.regression.strokeWidth = +regressionPlotProperties.strokeWidth;
                }
                else {
                    console.error(`plotProperties.regression.strokeWidth ${regressionPlotProperties.strokeWidth} is invalid`);
                }
            }
        }
        static isValidColor(colorSpecifier) {
            return colorSpecifier !== null && d3.color(colorSpecifier) !== null;
        }
        static isValidAlpha(value) {
            if (value) {
                const valueAsNumber = +value;
                return valueAsNumber >= 0 && valueAsNumber <= 1 && valueAsNumber.toString(10) === value.toString(10);
            }
            return false;
        }
        static isPositiveNumber(value) {
            if (value) {
                const valueAsNumber = +value;
                return valueAsNumber >= 0 && valueAsNumber.toString(10) === value.toString(10);
            }
            return false;
        }
        applyCssRules() {
            if (this.cssRules) {
                for (const [selector, declarations] of Object.entries(this.cssRules)) {
                    const selection = d3.select(this.bindto).selectAll(selector);
                    const applyDeclaration = (declaration) => {
                        const splitted = declaration.split(":");
                        if (splitted.length === 2) {
                            selection.style(splitted[0], splitted[1]);
                        }
                        else {
                            console.error("Invalid CSS declaration:", declaration);
                        }
                    };
                    if (Array.isArray(declarations)) {
                        declarations.forEach(applyDeclaration);
                    }
                    if (typeof declarations === "string") {
                        applyDeclaration(declarations);
                    }
                }
            }
        }
    }
    spm.Style = Style;
})(spm || (spm = {}));
// eslint-disable-next-line no-unused-vars
var spm;
(function (spm) {
    class VisibilityPad {
        constructor(multipleScatterPlot) {
            this.nCol = 0;
            this.nRow = 0;
            this.hidePlots = new Set();
            this.msp = multipleScatterPlot;
        }
        ;
        // eslint-disable-next-line max-lines-per-function
        generate(visibilityPadId, visiblePlots) {
            const thisPad = this;
            this.hidePlots.clear();
            if (Array.isArray(visiblePlots)) {
                this.msp.scatterPlotList.forEach(function (sp, i) {
                    const index = i % visiblePlots.length;
                    if (!visiblePlots[index]) {
                        thisPad.hidePlots.add(sp);
                    }
                });
            }
            const visibilityPad = (visibilityPadId && !d3.select("#" + visibilityPadId).empty())
                ? d3.select("#" + visibilityPadId)
                : d3.select(this.msp.bindto + " .visibilityPad");
            visibilityPad.select("svg").remove();
            const vpSvg = visibilityPad.append("svg")
                .attr("width", VisibilityPad.vpSize + 2 * VisibilityPad.vpMargin)
                .attr("height", VisibilityPad.vpSize + 2 * VisibilityPad.vpMargin)
                .append("g")
                .attr("transform", "translate(" + VisibilityPad.vpMargin + "," + VisibilityPad.vpMargin + ")");
            vpSvg.selectAll(".visibityCell")
                .data(this.msp.scatterPlotList)
                .enter().append("rect")
                .attr("class", "visibityCell")
                .style("fill", function (plot) { return thisPad.hidePlots.has(plot) ? "#999" : "steelblue"; })
                .style("stroke", "aliceblue")
                .style("stroke-width", "1")
                .attr("fill-opacity", 0.8)
                .attr("x", function (plot) {
                return plot.col * VisibilityPad.vpSize / spm.MultipleScatterPlot.grid.nCol;
            })
                .attr("y", function (plot) {
                return plot.row * VisibilityPad.vpSize / spm.MultipleScatterPlot.grid.nRow;
            })
                .attr("height", VisibilityPad.vpSize / spm.MultipleScatterPlot.grid.nCol)
                .attr("width", VisibilityPad.vpSize / spm.MultipleScatterPlot.grid.nRow)
                .on("mouseover", function (plot) {
                d3.select(this).attr("fill-opacity", 1);
                thisPad.showScatterNumber(plot);
            })
                .on("mouseout", function () {
                d3.select(this).attr("fill-opacity", 0.8);
                d3.selectAll(thisPad.msp.bindto + " .scatterNumber").style("display", "none");
            })
                .on("click", function (plot) {
                const visible = thisPad.toogleVisibility(plot);
                d3.select(this).style("fill", visible ? "steelblue" : "#999");
                thisPad.displayedPlotsChange();
                thisPad.msp.removePlots();
                thisPad.msp.updatePlots(spm.ScatterPlot.INIT);
                thisPad.showScatterNumber(plot);
                thisPad.msp.selectionPad.update();
                thisPad.msp.fixBrush();
            });
            this.displayedPlotsChange();
            return this;
        }
        showScatterNumber(plot) {
            d3.selectAll(this.msp.bindto + " .scatterNumber").style("display", "block");
            d3.selectAll(this.msp.bindto + " .scatterPlot").select(".scatterNumber")
                .classed("greyed", function (plot2) {
                return plot.row !== plot2.row || plot.col !== plot2.col;
            });
        }
        toogleVisibility(plot) {
            if (this.hidePlots.has(plot)) {
                this.hidePlots.delete(plot);
                return true;
            }
            else {
                this.hidePlots.add(plot);
                return false;
            }
        }
        visibleScatterPlots() {
            return this.msp.scatterPlotList.filter(plot => !this.hidePlots.has(plot));
        }
        vcell(plot) {
            const visiblePlotList = this.visibleScatterPlots();
            const vi = visiblePlotList.indexOf(plot);
            return {
                vrow: Math.floor(vi / this.nCol),
                vcol: vi % this.nCol
            };
        }
        visible(plot) {
            return !this.hidePlots.has(plot);
        }
        displayedPlotsChange() {
            const visiblePlotList = this.visibleScatterPlots();
            this.nCol = Math.ceil(Math.sqrt(visiblePlotList.length));
            this.nRow = Math.ceil(visiblePlotList.length / this.nCol);
        }
    }
    VisibilityPad.vpSize = 60;
    VisibilityPad.vpMargin = 1;
    spm.VisibilityPad = VisibilityPad;
})(spm || (spm = {}));

//# sourceMappingURL=maps/spm-msp.js.map
