buildShowText <- function(item) {
  cat("buildShowText\n")

  data <- ""

  if (item@typecode == "mcrv" ||
      item@typecode == "fnrv" ||
      item@typecode == "rsrv" ||
      item@typecode == "bsrv")
    data <- paste(item@data[1],
                  item@data[2],
                  item@data[3],
                  item@data[4],
                  item@data[5],
                  item@data[6],
                  sep = "<br />")

  paste0("<ul style='text-align: left;'>",
          "<li>",
            "<strong>Description:</strong> ",
            item@title,
          "</li>",
          "<li>",
            "<strong>Type:</strong> ",
            item@type,
          "</li>",
          "<li>",
            "<strong>Data:</strong> ",
            "<br />",
            data,
          "</li>",
          "<li>",
            "<strong>Definition:</strong> ",
            item@definition,
          "</li>",
          "<li>",
            "<strong>Dependencies:</strong> ",
            item@depitem,
          "</li>",
          "<li>",
            "<strong>Unit:</strong> ",
            item@unit,
          "</li>",
          "<li>",
            "<strong>Role:</strong> ",
            item@role,
          "</li>",
          "<li>",
            "<strong>Plausimin:</strong> ",
            item@plausimin,
          "</li>",
          "<li>",
            "<strong>Plausimax:</strong> ",
            item@plausimax,
          "</li>",
          "<li>",
            "<strong>Assumptions:</strong> ",
            item@assumptions,
          "</li>",
          "<li>",
            "<strong>Reference:</strong> ",
            item@reference,
          "</li>",
        "</ul>")
}
