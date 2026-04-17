alpha_labels <- c("0.50", "0.20", "0.10", "0.05", "0.025", "0.01", "0.005", "0.001")
alpha_values <- c(0.50, 0.20, 0.10, 0.05, 0.025, 0.01, 0.005, 0.001)

k_values <- c(2:20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 50, 60, 70, 80, 90, 100)
nu_values <- c(1:20, 24, 30, 40, 60, 120, Inf)

compute_q_matrix <- function(alpha_values, k_values, nu_values) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required for Tukey q table generation.", call. = FALSE)
  }

  python_bin <- Sys.which("python")
  if (!nzchar(python_bin)) {
    python_bin <- Sys.which("python3")
  }
  if (!nzchar(python_bin)) {
    stop("A Python interpreter with SciPy is required for Tukey q quantiles.", call. = FALSE)
  }

  payload <- jsonlite::toJSON(
    list(alpha_values = alpha_values, k_values = k_values, nu_values = nu_values),
    auto_unbox = TRUE
  )

  py_code <- paste(
    "import json, math, sys",
    "from scipy.stats import studentized_range",
    "from statsmodels.stats.libqsturng import qsturng",
    "payload = json.loads(sys.stdin.read())",
    "rows = []",
    "for alpha in payload['alpha_values']:",
    "    p = 1.0 - float(alpha)",
    "    for nu in payload['nu_values']:",
    "        df = math.inf if (nu == 'Inf' or nu == 'Infinity') else float(nu)",
    "        for k in payload['k_values']:",
    "            if p >= 0.9:",
    "                q = float(qsturng(p, int(k), df))",
    "            else:",
    "                q = float(studentized_range.ppf(p, float(k), df))",
    "            rows.append({'alpha': float(alpha), 'nu': nu, 'k': int(k), 'q': q})",
    "sys.stdout.write(json.dumps(rows))",
    sep = "\n"
  )

  py_file <- tempfile(fileext = ".py")
  payload_file <- tempfile(fileext = ".json")
  writeLines(py_code, py_file, useBytes = TRUE)
  writeLines(payload, payload_file, useBytes = TRUE)
  on.exit(unlink(c(py_file, payload_file)), add = TRUE)

  result <- system2(
    python_bin,
    args = py_file,
    stdin = payload_file,
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(result, "status")
  if (!is.null(status) && !identical(status, 0L)) {
    stop(
      paste(
        c("Python/SciPy failed while computing Tukey q quantiles.", result),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  q_rows <- jsonlite::fromJSON(paste(result, collapse = "\n"))
  q_rows$key <- paste(q_rows$alpha, q_rows$nu, q_rows$k, sep = "|")
  q_rows
}

build_tukey_q_table <- function(output_dir = ".",
                                alpha_values = alpha_values,
                                alpha_labels = alpha_labels,
                                k_values = k_values,
                                nu_values = nu_values,
                                compile_pdf = TRUE) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  csv_path <- file.path(output_dir, "tukey_q_critical_values.csv")
  tex_path <- file.path(output_dir, "tukey_q_critical_values.tex")
  pdf_path <- file.path(output_dir, "tukey_q_critical_values.pdf")
  q_rows <- compute_q_matrix(alpha_values = alpha_values, k_values = k_values, nu_values = nu_values)

  raw_rows <- list()
  tex_lines <- c(
    "\\documentclass[10pt]{article}",
    "\\usepackage[a4paper,margin=1.4cm]{geometry}",
    "\\usepackage{array}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{caption}",
    "\\setlength{\\LTpre}{0pt}",
    "\\setlength{\\LTpost}{0pt}",
    "\\renewcommand{\\arraystretch}{1.05}",
    "\\captionsetup{labelformat=empty}",
    "\\begin{document}"
  )

  k_groups <- split(k_values, ceiling(seq_along(k_values) / 9))

  for (panel_index in seq_along(alpha_values)) {
    alpha <- alpha_values[panel_index]
    alpha_label <- alpha_labels[panel_index]

    for (group_index in seq_along(k_groups)) {
      ks <- k_groups[[group_index]]
      is_first_page <- panel_index == 1 && group_index == 1
      title_text <- if (is_first_page) {
        "TABLE B.5: Critical Values of the $q$ Distribution, for the Tukey Test"
      } else {
        "TABLE B.5 (cont.): Critical Values of the $q$ Distribution, for the Tukey Test"
      }

      tex_lines <- c(
        tex_lines,
        if (!is_first_page) "\\newpage" else NULL,
        "\\thispagestyle{empty}",
        "\\begin{center}",
        paste0("{\\large\\bfseries ", title_text, "\\par}"),
        "\\end{center}",
        paste0("{\\large\\bfseries $\\alpha$ = ", alpha_label, "\\par}"),
        "\\vspace{0.45em}",
        "\\small",
        paste0(
          "\\begin{longtable}{@{}r!{\\vrule width 0.8pt\\hspace{0.55em}}",
          paste(rep("r", length(ks)), collapse = ""),
          "@{}}"
        ),
        "\\specialrule{1.1pt}{0pt}{0pt}",
        paste0(
          paste(
            c("$\\nu$", paste0("$k$: ", ks[1]), as.character(ks[-1])),
            collapse = " & "
          ),
          " \\\\"
        ),
        "\\specialrule{0.9pt}{0.2em}{0.3em}",
        "\\endfirsthead",
        "\\specialrule{1.1pt}{0pt}{0pt}",
        paste0(
          paste(
            c("$\\nu$", paste0("$k$: ", ks[1]), as.character(ks[-1])),
            collapse = " & "
          ),
          " \\\\"
        ),
        "\\specialrule{0.9pt}{0.2em}{0.3em}",
        "\\endhead",
        "\\specialrule{1.1pt}{0.3em}{0pt}",
        "\\endlastfoot"
      )

      for (i in seq_along(nu_values)) {
        nu <- nu_values[i]
        display_nu <- if (is.infinite(nu)) "$\\infty$" else as.character(nu)

        row_values <- vapply(
          ks,
          function(k) {
            key <- paste(alpha, if (is.infinite(nu)) "Inf" else as.character(nu), k, sep = "|")
            q_val <- q_rows$q[match(key, q_rows$key)]
            formatC(q_val, format = "f", digits = 3)
          },
          character(1)
        )

        tex_lines <- c(
          tex_lines,
          paste0(paste(c(display_nu, row_values), collapse = " & "), " \\\\")
        )

        raw_rows[[length(raw_rows) + 1]] <- data.frame(
          alpha = rep(alpha_label, length(ks)),
          nu = rep(if (is.infinite(nu)) "Inf" else as.character(nu), length(ks)),
          k = ks,
          q_critical = row_values,
          check.names = FALSE
        )

        if (i < length(nu_values) && i %% 5 == 0) {
          tex_lines <- c(tex_lines, "\\addlinespace[0.30em]")
        }
      }

      tex_lines <- c(tex_lines, "\\end{longtable}")
    }
  }

  tex_lines <- c(tex_lines, "\\end{document}")

  csv_data <- do.call(rbind, raw_rows)
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
          args = c("-interaction=nonstopmode", "-halt-on-error", basename(tex_path)),
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

  invisible(list(csv_path = csv_path, tex_path = tex_path, pdf_path = pdf_path))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  script_dir <- if (length(file_arg) == 1) {
    dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/"))
  } else {
    "."
  }

  build_tukey_q_table(
    output_dir = script_dir,
    alpha_values = alpha_values,
    alpha_labels = alpha_labels,
    k_values = k_values,
    nu_values = nu_values,
    compile_pdf = TRUE
  )
}
