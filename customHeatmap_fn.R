cellTopicHeatmap.custom <-function(object, method = "Probability", colorBy = "densityClust", colVars = NULL,
    col.low = "floralwhite", col.mid = "pink", col.high = "red",
    select.cells = NULL, ncol = 29, ...){
cat("************** max colors 29 ************")
   topic.mat <- modelMatSelection(object, "cell", method)
   rownames(topic.mat) <- paste("Topic", seq(1, nrow(topic.mat)))
   colnames(topic.mat) <- object@cell.names

   object.cell.data <- object@cell.data

   cl.cells <- fastcluster::hclust.vector(t(topic.mat), method = "ward",
       metric = "euclidean")

   dd.cells <- as.dendrogram(cl.cells)
   colorPal <- grDevices::colorRampPalette(c(col.low, col.mid,
       col.high))


       for (variable in colorBy) {
           if (is.null(colVars[[variable]])) {
               #colVars[[variable]] <- setNames(distinctColorPalette(length(unique(object@cell.data[,
                # variable]))), as.vector(sort(unique(object@cell.data[,
                # variable]))))
                x = ncol-17
                colVars[[variable]] <- setNames(c(brewer.pal(9,"Set1"),brewer.pal(8,"Set2"),brewer.pal(x,"Set3")), as.vector(sort(unique(object@cell.data[,
                  variable]))))

               cellColor <- setNames(colVars[[variable]][object.cell.data[,
                 variable]], rownames(object.cell.data))
           }
       }

       annotation <- ComplexHeatmap::HeatmapAnnotation(df = object.cell.data[,
           colorBy, drop = FALSE], col = colVars, which = "column"
           #,width = unit(5, "mm")
            )

       heatmap <- ComplexHeatmap::Heatmap(data.matrix(topic.mat),
           col = colorPal(20), cluster_columns = dd.cells, name = method,
           show_column_names = FALSE, show_row_names = TRUE,
           top_annotation = annotation, heatmap_legend_param = list(legend_direction = "horizontal",
               legend_width = unit(5, "cm"), title_position = "topcenter"),
           column_title = "Topic contribution per cell", column_title_gp = gpar(fontface = "bold"))

       ComplexHeatmap::draw(heatmap, heatmap_legend_side = "bottom",
           annotation_legend_side = "right")
   }
