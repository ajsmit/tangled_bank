/* eslint-disable */
// @ts-ignore
HTMLWidgets.widget({

    name: "scatterPlotMatrix",

    type: "output",

    factory: function(el, width, height) {
        function js2RIndex(index) {
            return (index !== null) ? index + 1 : index;
        }

        function r2JsIndex(index) {
            return (index !== null) ? index - 1 : index;
        }

        // @ts-ignore
        const scatterPlotMatrix = new spm.ScatterPlotMatrix(el.id, width, height);

        return {
            renderValue: function(config) {
                // Add a reference to the widget from the HTML element
                // @ts-ignore
                document.getElementById(scatterPlotMatrix.id()).widget = this;

                // If htmlwidget is included in Shiny app, listen JavaScript messages sent from Shiny
                if (HTMLWidgets.shinyMode) {
                    ["setDistribType", "setRegressionType", "setCorrPlotType", "setCorrPlotCS", "setContinuousColorScale", "setCategoricalColorScale", "setCutoffs", "setKeptColumns", "changeMouseMode", "setZAxis", "getPlotConfig"].forEach(func => {
                        Shiny.addCustomMessageHandler("scatterPlotMatrix:" + func, function(message) {
                            var el = document.getElementById(message.id);
                            if (el) {
                                el.widget[func](message);
                            }
                        });
                    });

                    // Listen event sent by the scatterPlotMatrix
                    const eventInputId = config.eventInputId !== null ? config.eventInputId : spm.ScatterPlotMatrix.PLOT_EVENT;
                    scatterPlotMatrix.on(spm.ScatterPlotMatrix.PLOT_EVENT, function (event) {
                        // Forward 'event' to Shiny through the reactive input 'eventInputId'
                        Shiny.setInputValue(eventInputId, event, {priority: "event"});
                    });
                }

                const controlWidgets = (config.controlWidgets === null) 
                    ? !HTMLWidgets.shinyMode : 
                    config.controlWidgets;

                const slidersPosition = config.slidersPosition
                    ? {}
                    : null;
                if (slidersPosition !== null) {
                    if (typeof config.slidersPosition.dimCount === "number") {
                        slidersPosition.dimCount = config.slidersPosition.dimCount
                    }
                    if (typeof config.slidersPosition.xStartingDimIndex === "number") {
                        slidersPosition.xStartingDimIndex = r2JsIndex(config.slidersPosition.xStartingDimIndex)
                    }
                    if (typeof config.slidersPosition.yStartingDimIndex === "number") {
                        slidersPosition.yStartingDimIndex = r2JsIndex(config.slidersPosition.yStartingDimIndex)
                    }
                }

                // @ts-ignore
                scatterPlotMatrix.generate({
                    // @ts-ignore
                    data: HTMLWidgets.dataframeToD3(config.data),
                    rowLabels: config.rowLabels,
                    categorical: config.categorical,
                    inputColumns: config.inputColumns,
                    cutoffs: config.cutoffs,
                    keptColumns: config.keptColumns,
                    zAxisDim: config.zAxisDim,
                    distribType: config.distribType,
                    regressionType: config.regressionType,
                    corrPlotType: config.corrPlotType,
                    corrPlotCS: config.corrPlotCS,
                    rotateTitle : config.rotateTitle,
                    columnLabels: config.columnLabels,
                    continuousCS: config.continuousCS,
                    categoricalCS: config.categoricalCS,
                    controlWidgets: controlWidgets,
                    cssRules: config.cssRules,
                    plotProperties: config.plotProperties,
                    slidersPosition: slidersPosition
                });
            }, // End 'renderValue'

            setDistribType: function(params) {
                scatterPlotMatrix.setDistribType(params.distribType);
            },

            setRegressionType: function(params) {
                scatterPlotMatrix.setRegressionType(params.regressionType);
            },

            setCorrPlotType: function(params) {
                scatterPlotMatrix.setCorrPlotType(params.corrPlotType);
            },

            setCorrPlotCS: function(params) {
                scatterPlotMatrix.setCorrPlotCS(params.corrPlotCsId);
            },

            setContinuousColorScale: function(params) {
                scatterPlotMatrix.setContinuousColorScale(params.continuousCsId);
            },

            setCategoricalColorScale: function(params) {
                scatterPlotMatrix.setCategoricalColorScale(params.categoricalCsId);
            },

            setCutoffs: function(params) {
                scatterPlotMatrix.setCutoffs(params.cutoffs);
            },

            setKeptColumns: function(params) {
                scatterPlotMatrix.setKeptColumns(params.keptColumns);
            },

            changeMouseMode: function(params) {
                scatterPlotMatrix.changeMouseMode(params.interactionType);
            },

            setZAxis: function(params) {
                scatterPlotMatrix.setZAxis(params.dim);
            },

            getPlotConfig: function(params) {
                if (HTMLWidgets.shinyMode) {
                    const plotConfig = scatterPlotMatrix.getPlotConfig();
                    plotConfig.slidersPosition.xStartingDimIndex = js2RIndex(plotConfig.slidersPosition.xStartingDimIndex);
                    plotConfig.slidersPosition.yStartingDimIndex = js2RIndex(plotConfig.slidersPosition.yStartingDimIndex);
                    Shiny.setInputValue(params.configInputId, plotConfig, {priority: "event"});
                }
            },

            resize: function(width, height) {
                scatterPlotMatrix.resize(width, height);
            }
        };
    } // End 'factory'
});
