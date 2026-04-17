alpha_two_labels <- c("0.50", "0.20", "0.10", "0.05", "0.02",
                      "0.01", "0.005", "0.002", "0.001")
alpha_one <- c(0.25, 0.10, 0.05, 0.025, 0.01,
               0.005, 0.0025, 0.001, 0.0005)
alpha_one_labels <- c("0.25", "0.10", "0.05", "0.025", "0.01",
                      "0.005", "0.0025", "0.001", "0.0005")

numerator_df_values <- c(
  1:20,
  22, 24, 26, 28, 30,
  40, 50, 60, 70, 80, 90, 100,
  120, 140, 200, Inf
)
denominator_df_values <- c(
  1:30,
  35, 40, 45, 50, 60, 70, 80, 90, 100, 120,
  140, 160, 180, 200, 300, 500, Inf
)

format_f_value <- function(x) {
  if (x >= 100) {
    return(paste0(format(round(x), scientific = FALSE, trim = TRUE), "."))
  }

  if (x >= 10) {
    return(formatC(x, format = "f", digits = 1))
  }

  formatC(x, format = "f", digits = 2)
}

compute_f_critical <- function(df1, df2, alpha) {
  if (is.infinite(df1) && is.infinite(df2)) {
    return(1)
  }

  if (is.infinite(df1)) {
    return(df2 / stats::qchisq(alpha, df = df2))
  }

  if (is.infinite(df2)) {
    return(stats::qchisq(1 - alpha, df = df1) / df1)
  }

  stats::qf(1 - alpha, df1 = df1, df2 = df2)
}

build_f_critical_table <- function(output_dir = ".",
                                   numerator_df_values = numerator_df_values,
                                   denominator_df_values = denominator_df_values,
                                   compile_pdf = TRUE) {
  stopifnot(length(numerator_df_values) >= 1, length(denominator_df_values) >= 1)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  csv_path <- file.path(output_dir, "f_critical_values_v1_1_to_6.csv")
  tex_path <- file.path(output_dir, "f_critical_values_v1_1_to_6.tex")
  pdf_path <- file.path(output_dir, "f_critical_values_v1_1_to_6.pdf")

  raw_rows <- list()
  tex_lines <- c(
    "\\documentclass[10pt]{article}",
    "\\usepackage[a4paper,margin=1.5cm]{geometry}",
    "\\usepackage{array}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{caption}",
    "\\setlength{\\LTpre}{0pt}",
    "\\setlength{\\LTpost}{0pt}",
    "\\renewcommand{\\arraystretch}{1.06}",
    "\\captionsetup{labelformat=empty}",
    "\\begin{document}"
  )

  header_line_one <- paste(
    c("$v_2$ =", paste0("$\\alpha(2)$: ", alpha_two_labels[1]), alpha_two_labels[-1]),
    collapse = " & "
  )
  header_line_two <- paste(
    c("Denom. DF", paste0("$\\alpha(1)$: ", alpha_one_labels[1]), alpha_one_labels[-1]),
    collapse = " & "
  )

  for (idx in seq_along(numerator_df_values)) {
    df1 <- numerator_df_values[idx]
    is_first <- idx == 1
    title_text <- if (is_first) {
      "TABLE B.4: Critical Values of the $F$ Distribution"
    } else {
      "TABLE B.4 (cont.): Critical Values of the $F$ Distribution"
    }

    tex_lines <- c(
      tex_lines,
      if (!is_first) "\\newpage" else NULL,
      "\\thispagestyle{empty}",
      "\\begin{center}",
      paste0("{\\large\\bfseries ", title_text, "\\par}"),
      "\\end{center}",
      paste0("{\\large\\bfseries $v_1$ = Numerator DF = ", df1, "\\par}"),
      "\\vspace{0.5em}",
      "\\small",
      "\\begin{longtable}{@{}r!{\\vrule width 0.8pt\\hspace{0.55em}}rrrrrrrrr@{}}",
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
      "\\endlastfoot"
    )

    for (i in seq_along(denominator_df_values)) {
      df2 <- denominator_df_values[i]
      row_values <- vapply(alpha_one, function(alpha) {
        format_f_value(compute_f_critical(df1 = df1, df2 = df2, alpha = alpha))
      }, character(1))

      raw_rows[[length(raw_rows) + 1]] <- data.frame(
        v1 = df1,
        v2 = if (is.infinite(df2)) "Inf" else as.character(df2),
        t(row_values),
        check.names = FALSE
      )

      display_df2 <- if (is.infinite(df2)) "$\\infty$" else as.character(df2)
      tex_lines <- c(
        tex_lines,
        paste0(paste(c(display_df2, row_values), collapse = " & "), " \\\\")
      )

      if (i < length(denominator_df_values) && (i %% 5 == 0 || denominator_df_values[i] %in% c(30, 60, 120, 300))) {
        tex_lines <- c(tex_lines, "\\addlinespace[0.32em]")
      }
    }

    tex_lines <- c(tex_lines, "\\end{longtable}")
  }

  tex_lines <- c(tex_lines, "\\end{document}")

  csv_data <- do.call(rbind, raw_rows)
  names(csv_data) <- c("v1", "v2", alpha_one_labels)

  utils::write.csv(csv_data, csv_path, row.names = FALSE)
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
      csv_path = csv_path,
      tex_path = tex_path,
      pdf_path = pdf_path,
      table_data = csv_data
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

  build_f_critical_table(
    output_dir = script_dir,
    numerator_df_values = numerator_df_values,
    denominator_df_values = denominator_df_values,
    compile_pdf = TRUE
  )
}
