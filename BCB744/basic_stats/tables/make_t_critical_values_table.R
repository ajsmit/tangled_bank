alpha_two_labels <- c("0.50", "0.20", "0.10", "0.05", "0.02",
                      "0.01", "0.005", "0.002", "0.001")
alpha_one_labels <- c("0.25", "0.10", "0.05", "0.025", "0.01",
                      "0.005", "0.0025", "0.001", "0.0005")

default_df_values <- c(
  1:50,
  seq(52, 100, by = 2),
  seq(105, 150, by = 5),
  seq(160, 200, by = 10),
  seq(250, 450, by = 50),
  seq(500, 1000, by = 100),
  Inf
)

build_t_critical_table <- function(output_dir = ".", df_values = default_df_values,
                                   compile_pdf = TRUE) {
  stopifnot(length(df_values) >= 1)

  alpha_two <- as.numeric(alpha_two_labels)
  alpha_one <- as.numeric(alpha_one_labels)
  df_values <- unique(df_values)
  finite_df_values <- df_values[is.finite(df_values)]
  display_nu <- ifelse(is.infinite(df_values), "$\\infty$", as.character(df_values))

  critical_matrix <- vapply(
    alpha_two,
    FUN = function(alpha) {
      ifelse(
        is.infinite(df_values),
        stats::qnorm(1 - alpha / 2),
        stats::qt(1 - alpha / 2, df = df_values)
      )
    },
    FUN.VALUE = numeric(length(df_values))
  )

  formatted_matrix <- matrix(sprintf("%.3f", critical_matrix), nrow = length(df_values))

  if (any(is.infinite(df_values))) {
    formatted_matrix[is.infinite(df_values), ] <- sprintf(
      "%.4f",
      critical_matrix[is.infinite(df_values), , drop = FALSE]
    )
  }

  table_data <- data.frame(
    nu = display_nu,
    matrix(formatted_matrix, nrow = length(df_values)),
    check.names = FALSE
  )

  names(table_data) <- c("nu", alpha_two_labels)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  csv_path <- file.path(output_dir, "t_critical_values_extended.csv")
  tex_path <- file.path(output_dir, "t_critical_values_extended.tex")
  pdf_path <- file.path(output_dir, "t_critical_values_extended.pdf")

  utils::write.csv(table_data, csv_path, row.names = FALSE)

  row_lines <- character(length = 0)

  for (i in seq_len(nrow(table_data))) {
    row_values <- c(
      table_data$nu[i],
      unname(unlist(table_data[i, -1], use.names = FALSE))
    )

    row_lines <- c(row_lines, paste0(paste(row_values, collapse = " & "), " \\\\"))

    if (i < nrow(table_data) && i %% 5 == 0) {
      row_lines <- c(row_lines, "\\addlinespace[0.35em]")
    }
  }

  header_line_one <- paste(
    c("$\\nu$", paste0("$\\alpha(2)$: ", alpha_two_labels[1]),
      alpha_two_labels[-1]),
    collapse = " & "
  )

  header_line_two <- paste(
    c("", paste0("$\\alpha(1)$: ", alpha_one_labels[1]),
      alpha_one_labels[-1]),
    collapse = " & "
  )

  tex_lines <- c(
    "\\documentclass[10pt]{article}",
    "\\usepackage[a4paper,margin=1.6cm]{geometry}",
    "\\usepackage{array}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{caption}",
    "\\setlength{\\LTpre}{0pt}",
    "\\setlength{\\LTpost}{0pt}",
    "\\renewcommand{\\arraystretch}{1.08}",
    "\\captionsetup{labelformat=empty}",
    "\\begin{document}",
    "\\thispagestyle{empty}",
    "\\begin{center}",
    "{\\large\\bfseries TABLE B.3: Critical Values of the $t$ Distribution\\par}",
    "\\vspace{0.75em}",
    "\\small",
    "\\begin{longtable}{@{}r!{\\vrule width 0.8pt\\hspace{0.65em}}rrrrrrrrr@{}}",
    "\\specialrule{1.1pt}{0pt}{0pt}",
    paste0(header_line_one, " \\\\"),
    paste0(header_line_two, " \\\\"),
    "\\specialrule{0.9pt}{0.2em}{0.3em}",
    "\\endfirsthead",
    "\\specialrule{1.1pt}{0pt}{0pt}",
    paste0(header_line_one, " \\\\"),
    paste0(header_line_two, " \\\\"),
    "\\specialrule{0.9pt}{0.2em}{0.3em}",
    "\\endhead",
    "\\specialrule{1.1pt}{0.3em}{0pt}",
    "\\endlastfoot",
    row_lines,
    "\\end{longtable}",
    "\\end{center}",
    "\\end{document}"
  )

  writeLines(tex_lines, con = tex_path, useBytes = TRUE)

  if (isTRUE(compile_pdf)) {
    latex_bin <- Sys.which("lualatex")

    if (nzchar(latex_bin)) {
      old_dir <- getwd()
      on.exit(setwd(old_dir), add = TRUE)
      setwd(output_dir)

      for (i in 1:2) {
        result <- system2(
          latex_bin,
          args = c(
            "-interaction=nonstopmode",
            "-halt-on-error",
            basename(tex_path)
          ),
          stdout = TRUE,
          stderr = TRUE
        )

        status <- attr(result, "status")

        if (is.null(status)) {
          status <- 0L
        }

        if (!identical(status, 0L)) {
          stop("lualatex failed while compiling ", basename(tex_path), call. = FALSE)
        }
      }

      unlink(c(
        sub("\\.tex$", ".aux", basename(tex_path)),
        sub("\\.tex$", ".log", basename(tex_path))
      ))
    }
  }

  invisible(
    list(
      table_data = table_data,
      df_values = df_values,
      finite_df_values = finite_df_values,
      alpha_two = alpha_two,
      alpha_one = alpha_one,
      csv_path = csv_path,
      tex_path = tex_path,
      pdf_path = pdf_path
    )
  )
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  script_dir <- if (length(file_arg) == 1) {
    dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/"))
  } else {
    "."
  }

  build_t_critical_table(
    output_dir = script_dir,
    df_values = default_df_values,
    compile_pdf = TRUE
  )
}
