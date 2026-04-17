row_bases <- seq(0, 3.8, by = 0.1)
col_digits <- 0:9

build_normal_tail_table <- function(output_dir = ".", row_bases = row_bases,
                                    col_digits = col_digits, compile_pdf = TRUE) {
  stopifnot(length(row_bases) >= 1, length(col_digits) >= 1)

  z_matrix <- outer(row_bases, col_digits / 100, "+")
  tail_matrix <- stats::pnorm(z_matrix, lower.tail = FALSE)

  table_data <- data.frame(
    Z_left = sprintf("%.1f", row_bases),
    matrix(sprintf("%.4f", tail_matrix), nrow = length(row_bases)),
    Z_right = sprintf("%.1f", row_bases),
    check.names = FALSE
  )

  names(table_data) <- c("Z_left", as.character(col_digits), "Z_right")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  csv_path <- file.path(output_dir, "normal_curve_tail_proportions.csv")
  tex_path <- file.path(output_dir, "normal_curve_tail_proportions.tex")
  pdf_path <- file.path(output_dir, "normal_curve_tail_proportions.pdf")

  utils::write.csv(table_data, csv_path, row.names = FALSE)

  row_lines <- character(length = 0)

  for (i in seq_len(nrow(table_data))) {
    row_values <- c(
      table_data$Z_left[i],
      unname(unlist(table_data[i, 2:11], use.names = FALSE)),
      table_data$Z_right[i]
    )

    row_lines <- c(row_lines, paste0(paste(row_values, collapse = " & "), " \\\\"))

    if (i < nrow(table_data) && i %% 5 == 0) {
      row_lines <- c(row_lines, "\\addlinespace[0.30em]")
    }
  }

  tex_lines <- c(
    "\\documentclass[10pt]{article}",
    "\\usepackage[a4paper,margin=1.4cm]{geometry}",
    "\\usepackage{array}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{caption}",
    "\\usepackage{amsmath}",
    "\\setlength{\\LTpre}{0pt}",
    "\\setlength{\\LTpost}{0pt}",
    "\\renewcommand{\\arraystretch}{1.06}",
    "\\captionsetup{labelformat=empty}",
    "\\begin{document}",
    "\\thispagestyle{empty}",
    "\\begin{center}",
    "{\\large\\bfseries TABLE B.2: Proportions of the Normal Curve (One-Tailed)\\par}",
    "\\vspace{0.35em}",
    "{\\small\\bfseries This table gives the proportion of the normal curve in the right-hand tail that lies at\\par}",
    "{\\small\\bfseries or beyond (i.e., is at least as extreme as) a given normal deviate; for example,\\par}",
    "{\\small\\bfseries $Z = |(X_i - \\mu)|/\\sigma$ or $Z = |(\\bar{X} - \\mu)|/\\sigma_{\\bar{X}}$. For example, the proportion of a normal\\par}",
    "{\\small\\bfseries distribution for which $Z \\geq 1.51$ is 0.0655.\\par}",
    "\\vspace{0.75em}",
    "\\small",
    "\\begin{longtable}{@{}r!{\\vrule width 0.8pt\\hspace{0.55em}}rrrrrrrrrr!{\\hspace{0.55em}\\vrule width 0.8pt}r@{}}",
    "\\specialrule{1.1pt}{0pt}{0pt}",
    "Z & 0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & Z \\\\",
    "\\specialrule{0.9pt}{0.2em}{0.3em}",
    "\\endfirsthead",
    "\\specialrule{1.1pt}{0pt}{0pt}",
    "Z & 0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & Z \\\\",
    "\\specialrule{0.9pt}{0.2em}{0.3em}",
    "\\endhead",
    "\\specialrule{1.1pt}{0.3em}{0pt}",
    "\\endlastfoot",
    row_lines,
    "\\end{longtable}",
    "\\vspace{0.8em}",
    "\\parbox{0.97\\linewidth}{\\small Table B.2 was prepared directly from the standard normal distribution using R. Probabilities for values of $Z$ between those shown in this table may be obtained by interpolation. Critical values of $Z$ are related to the $t$ table by $Z_{\\alpha(2)} = t_{\\alpha(2),\\infty}$.}",
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

  build_normal_tail_table(
    output_dir = script_dir,
    row_bases = row_bases,
    col_digits = col_digits,
    compile_pdf = TRUE
  )
}
